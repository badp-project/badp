#' Calculation of the jointness measures
#'
#' This function calculates four types of the jointness measures based on the posterior model probabilities calculated using binomial and binomial-beta model prior. The four measures are: \cr
#' 1) HCGHM - for Hofmarcher et al. (2018) measure; \cr
#' 2) LS - for Ley & Steel (2007) measure; \cr
#' 3) DW - for Doppelhofer & Weeks (2009) measure; \cr
#' 4) PPI - for posterior probability of including both variables. \cr
#' The measures under binomial model prior will appear in a table above the diagonal, and the measure calculated under binomial-beta model prior below the diagonal. \cr
#' \cr
#' REFERENCES \cr
#' Doppelhofer G, Weeks M (2009) Jointness of growth determinants. Journal of Applied Econometrics., 24(2), 209-244. doi: 10.1002/jae.1046 \cr
#' Hofmarcher P, Crespo Cuaresma J, Gr\enc{ü}{u}n B, Humer S, Moser M (2018) Bivariate jointness measures in Bayesian Model Averaging: Solving the conundrum. Journal of Macroeconomics, 57, 150-165. doi: 10.1016/j.jmacro.2018.05.005 \cr
#' Ley E, Steel M (2007) Jointness in Bayesian variable selection with applications to growth regression. Journal of Macroeconomics, 29(3), 476-493. doi: 10.1016/j.jmacro.2006.12.002
#'
#' @param bma_list An object of class \code{badp_bma}, typically returned by \code{\link{bma}}.
#' @param measure Character string specifying the measure of jointness. One of: \cr
#' \code{"HCGHM"} - Hofmarcher et al. (2018) measure (default); \cr
#' \code{"LS"} - Ley & Steel (2007) measure; \cr
#' \code{"DW"} - Doppelhofer & Weeks (2009) measure; \cr
#' \code{"PPI"} - posterior probability of including both variables.
#' @param rho The parameter "rho" (\eqn{\rho}) to be used in HCGHM jointness measure (default rho = 0.5). Works only if HCGHM measure is chosen (Hofmarcher et al. 2018).
#' @param round Parameter indicating the decimal place to which the jointness measures should be rounded (default round = 3).
#'
#' @return A table with jointness measures for all the pairs of regressors used in the analysis. The results obtained with the binomial model prior are above the diagonal, while the ones obtained with the binomial-beta prior are below.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' data_prepared <- badp::economic_growth[, 1:6] %>%
#'   badp::feature_standardization(
#'     excluded_cols = c(country, year, gdp)
#'   ) %>%
#'   badp::feature_standardization(
#'     group_by_col  = year,
#'     excluded_cols = country,
#'     scale         = FALSE
#'   )
#'
#' bma_results <- bma(
#'   model_space = badp::small_model_space,
#'   round       = 3,
#'   dilution    = 0
#' )
#'
#' jointness_table <- jointness(bma_results, measure = "HCGHM", rho = 0.5, round = 3)
#' }
jointness <- function(bma_list, measure = c("HCGHM", "LS", "DW", "PPI"), rho = 0.5, round = 3) {
  measure <- match.arg(measure)

  # Extraction of the elements of the bma object
  reg_names <- bma_list[[3]]      # names of all regressors incl. lagged dep var
  reg_names <- reg_names[-1]      # drop lagged dep var: keep only the R regressors
  R <- bma_list[[4]]              # number of regressors
  forJointness <- bma_list[[6]]   # M x (R + 2): inclusion flags + two prior PMP columns

  # Inclusion matrix (0/1), M x R
  Z <- as.matrix(forJointness[, 1:R])
  # PMP weights of length M, one per model, under each model prior
  w_uniform <- as.numeric(forJointness[, R + 1])  # binomial (uniform) prior
  w_random  <- as.numeric(forJointness[, R + 2])  # binomial-beta (random) prior

  # All pairwise joint inclusion probabilities, vectorised over (a, b):
  # Pab[a, b] = sum over models containing BOTH a and b, weighted by PMP.
  # crossprod(Z * w, Z) == t(Z * w) %*% Z; Z * w scales each row by w[i].
  Pab_U <- crossprod(Z * w_uniform, Z)
  Pab_R <- crossprod(Z * w_random,  Z)

  # Marginal inclusion probabilities (length R), Pa[k] = sum_i w[i] * Z[i, k].
  Pa_U <- as.numeric(crossprod(w_uniform, Z))
  Pa_R <- as.numeric(crossprod(w_random,  Z))

  # Remaining cell probabilities via inclusion-exclusion. In all R x R matrices
  # below, row index = a and column index = b, so byrow=TRUE replicates Pa
  # along rows (giving P(b)) and byrow=FALSE replicates it down columns
  # (giving P(a)).
  Pb_mat_U <- matrix(Pa_U, nrow = R, ncol = R, byrow = TRUE)
  Pa_mat_U <- matrix(Pa_U, nrow = R, ncol = R, byrow = FALSE)
  Pb_mat_R <- matrix(Pa_R, nrow = R, ncol = R, byrow = TRUE)
  Pa_mat_R <- matrix(Pa_R, nrow = R, ncol = R, byrow = FALSE)

  Na_b_U  <- Pb_mat_U - Pab_U                       # P(NOT a and b)
  a_Nb_U  <- Pa_mat_U - Pab_U                       # P(a and NOT b)
  Na_Nb_U <- 1 - Pa_mat_U - Pb_mat_U + Pab_U        # P(NOT a and NOT b)
  Na_b_R  <- Pb_mat_R - Pab_R
  a_Nb_R  <- Pa_mat_R - Pab_R
  Na_Nb_R <- 1 - Pa_mat_R - Pb_mat_R + Pab_R

  # Measure formulas (element-wise on R x R matrices). `first` is the measure
  # under the binomial prior (uniform), `second` under binomial-beta (random).
  if (measure == "HCGHM") {
    num_U <- (Pab_U + rho) * (Na_Nb_U + rho) - (Na_b_U + rho) * (a_Nb_U + rho)
    den_U <- (Pab_U + rho) * (Na_Nb_U + rho) + (Na_b_U + rho) * (a_Nb_U + rho) - rho
    first  <- num_U / den_U
    num_R <- (Pab_R + rho) * (Na_Nb_R + rho) - (Na_b_R + rho) * (a_Nb_R + rho)
    den_R <- (Pab_R + rho) * (Na_Nb_R + rho) + (Na_b_R + rho) * (a_Nb_R + rho) - rho
    second <- num_R / den_R
  } else if (measure == "LS") {
    first  <- Pab_U / (Na_b_U + a_Nb_U)
    second <- Pab_R / (Na_b_R + a_Nb_R)
  } else if (measure == "DW") {
    first  <- log((Pab_U / Na_b_U) * (Na_Nb_U / a_Nb_U))
    second <- log((Pab_R / Na_b_R) * (Na_Nb_R / a_Nb_R))
  } else {  # "PPI"
    first  <- Pab_U
    second <- Pab_R
  }

  # Assemble final table: above diagonal = uniform measure, below = random.
  # The four jointness measures are symmetric in (a, b), so first and second
  # are themselves symmetric; we just need the right halves in the output.
  jointness_table <- first
  jointness_table[lower.tri(jointness_table)] <- second[lower.tri(second)]
  diag(jointness_table) <- NA_real_

  jointness_table <- round(jointness_table, digits = round)
  rownames(jointness_table) <- reg_names
  colnames(jointness_table) <- reg_names

  return(jointness_table)
}
