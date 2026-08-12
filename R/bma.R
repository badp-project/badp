# Marginal-likelihood approximations used to weight the models.
#
# Row 2 of a model space's statistics stores, for every model j,
#
#     scaled_log_weight_j = ( loglik_j - (k_j/2) * log(N*T) ) / N
#
# i.e. minus one half of the Schwarz criterion, divided by the number of
# entities. That is what Moral-Benito's GAUSS implementation computes and what
# reproduces the results published in Moral-Benito (2016). Since the mapping is
# invertible, the alternatives below can be formed from a fitted model space
# without re-estimating any model.
#
# Writing A_j = loglik_j - (k_j/2) * log(N*T) = N * scaled_log_weight_j, the
# three options return the log of the model weight w_j:
#
#   "mb2016"  log w_j = A_j / N
#             The reference implementation. Equivalent to raising the Schwarz
#             approximation to the power 1/N, which compresses log Bayes
#             factors by a factor of N.
#
#   "mb2012"  log w_j = A_j
#             The Schwarz criterion exactly as stated in Moral-Benito (2012,
#             equations 24-30), taking the sample size to be the number of
#             entity-period observations N*T.
#
#   "nt"      log w_j = A_j / (N*T)
#             As above but averaging over entity-periods rather than entities.
#             Included for comparison: it is the scaling that would be
#             internally consistent with the log(N*T) penalty, and it tempers T
#             times more strongly than "mb2016".
#
#   "curvature"  log w_j = eta_hat * A_j
#             A learning rate estimated from the data rather than fixed a
#             priori. See estimate_curvature_rate() below.
#
#   "uip"     log w_j = A_j + (k_j/2) * log(T)
#                     = loglik_j - (k_j/2) * log(N)
#             The Schwarz criterion with the entity as the unit of information.
#             The unit information prior underlying the approximation is
#             defined through the Fisher information for one observation; since
#             the likelihood factorises over entities, the entity is the natural
#             unit, giving a penalty in log(N) rather than log(N*T).
#
# Note that "mb2012", "mb2016", "nt" and "curvature" are the same construction
# with different learning rates eta: 1, 1/N, 1/(N*T) and eta_hat respectively.
# Only "uip" changes the penalty rather than the rate. Any eta != 1 yields a
# tempered (power) posterior over models, f(y|M_j)^eta, which is outside the
# Schwarz/Laplace theory that motivates the approximation and which does not
# concentrate as N grows.
#
# The result is returned on the log scale and shifted so that its maximum is
# zero. Posterior model probabilities are normalised, so the shift cancels; it
# only prevents overflow, which matters because A_j can be in the hundreds.
weighting_log_weights <- function(scaled_log_weight, weighting, n_params,
                                  n_entities, n_periods, eta = NULL) {
  weighting <- match.arg(
    weighting, c("mb2016", "mb2012", "nt", "curvature", "uip")
  )

  a <- n_entities * scaled_log_weight        # A_j

  log_w <- switch(
    weighting,
    mb2016    = a / n_entities,
    mb2012    = a,
    nt        = a / (n_entities * n_periods),
    curvature = eta * a,
    uip       = a + (n_params / 2) * log(n_periods)
  )

  exp(log_w - max(log_w))
}


# Learning rate estimated by the magnitude ("omnibus") adjustment used for
# composite and otherwise misspecified likelihoods (Chandler and Bate 2007;
# Ribatet, Cooley and Davison 2012). When a likelihood overstates the
# information in the data, the observed information H and the outer product of
# the per-entity scores J disagree, and the likelihood ratio no longer has its
# nominal distribution. The adjustment rescales the log-likelihood by
#
#     eta = p / tr(H^{-1} J),
#
# which restores the nominal calibration. Under the simplifying assumption
# J = c * H the trace is c * p, so eta = 1/c, and c is the factor by which the
# sandwich variance inflates the Hessian-based variance. badp stores the two
# sets of standard errors but not H and J themselves, so c is estimated
# coordinate-wise as the mean of (robust SE / Hessian SE)^2 within a model, and
# summarised across models by the median for robustness.
#
# This is an approximation in two respects: J = c * H need not hold, and only
# the diagonals of the two variance matrices are available. It is offered as a
# data-driven point of comparison to the fixed rates, not as a substitute for
# computing tr(H^{-1} J) directly, which would require storing H and J when the
# model space is estimated.
estimate_curvature_rate <- function(std, stdR, like_table = NULL, K = NULL) {
  # Exact route. Model spaces fitted from badp 0.6.0 onwards carry
  # tr(H^{-1} J) and dim(theta) in the last two rows of the statistics, so the
  # adjustment can be evaluated as written. The rate is summarised across
  # models by the median.
  if (!is.null(like_table) && !is.null(K) && nrow(like_table) >= 4 + 2 * K) {
    trace_hinv_j <- like_table[3 + 2 * K, ]
    n_theta <- like_table[4 + 2 * K, ]
    ok <- is.finite(trace_hinv_j) & is.finite(n_theta) & trace_hinv_j > 0
    if (any(ok)) {
      return(stats::median(n_theta[ok] / trace_hinv_j[ok]))
    }
  }

  # Fallback for model spaces fitted before those quantities were stored.
  # Only the diagonals of H^{-1} and H^{-1} J H^{-1} are available, so the
  # trace is approximated under the assumption J = c * H, for which
  # tr(H^{-1} J) = c * dim(theta) and c is the variance inflation implied by
  # the two sets of standard errors.
  message("This model space predates badp 0.6.0 and does not store ",
          "tr(H^{-1}J); the curvature-adjusted learning rate is being ",
          "approximated from the ratio of robust to Hessian-based standard ",
          "errors. Re-estimate the model space for the exact adjustment.")

  std <- as.matrix(std)
  stdR <- as.matrix(stdR)

  inflation <- vapply(seq_len(ncol(std)), function(j) {
    ok <- is.finite(std[, j]) & is.finite(stdR[, j]) & std[, j] > 0
    if (!any(ok)) return(NA_real_)
    mean((stdR[ok, j] / std[ok, j])^2)
  }, numeric(1))

  c_hat <- stats::median(inflation, na.rm = TRUE)

  if (!is.finite(c_hat) || c_hat <= 0) {
    warning("could not estimate a curvature-adjusted learning rate; ",
            "falling back to eta = 1 (the unadjusted Schwarz criterion).",
            call. = FALSE)
    return(1)
  }

  1 / c_hat
}


#' Calculation of the bma object
#'
#' This function calculates BMA statistics based on the provided model space.
#' Other objects for further analysis are also returned.
#'
#' @param model_space An object of class \code{badp_model_space}, typically returned by \code{\link{optim_model_space}}.
#' @param round Integer indicating the decimal place to which numbers in the BMA tables and prior and posterior model sizes should be rounded (default: 4).
#' @param EMS Numeric. Expected model size for binomial and binomial-beta model prior (default: R/2, where R is the number of regressors).
#' @param dilution Integer. Use 0 for no dilution prior (default), or 1 to apply a dilution prior (George 2010).
#' @param omega Numeric. The exponent of the determinant for the dilution prior (George 2010). Only used when \code{dilution = 1}. Default: 0.5.
#' @param weighting Character string selecting the approximation to the
#' marginal likelihood used to weight the models. One of: \cr
#' \code{"mb2016"} (default) - the approximation computed by the
#' implementation accompanying Moral-Benito (2016), namely
#' \eqn{\exp\{(\ell_j - (k_j/2)\log(NT))/N\}}, where \eqn{\ell_j} is the
#' maximised log-likelihood of model \eqn{j}, \eqn{k_j} its number of linear
#' parameters, \eqn{N} the number of entities and \eqn{T} the number of
#' periods. This is the Schwarz approximation raised to the power \eqn{1/N} and
#' is the option that reproduces the results published in Moral-Benito (2016);
#' \cr
#' \code{"mb2012"} - the Schwarz criterion as stated in equations (24)-(30) of
#' Moral-Benito (2012), \eqn{\exp\{\ell_j - (k_j/2)\log(NT)\}}, treating the
#' \eqn{NT} entity-period observations as the sample; \cr
#' \code{"nt"} - as \code{"mb2016"} but averaging over entity-periods rather
#' than entities, \eqn{\exp\{(\ell_j - (k_j/2)\log(NT))/(NT)\}}. This is the
#' scaling that would be internally consistent with the \eqn{\log(NT)} penalty,
#' and it tempers \eqn{T} times more strongly than \code{"mb2016"}; \cr
#' \code{"curvature"} - a learning rate estimated from the data by the
#' magnitude ("omnibus") adjustment used for misspecified and composite
#' likelihoods (Chandler and Bate 2007; Ribatet, Cooley and Davison 2012),
#' rather than fixed a priori. See Details; \cr
#' \code{"uip"} - the Schwarz criterion with the entity as the unit of
#' information, \eqn{\exp\{\ell_j - (k_j/2)\log(N)\}}. The unit information
#' prior underlying the approximation (Kass and Wasserman 1995) is defined
#' through the Fisher information for a single observation, and the likelihood
#' factorises over entities, so the entity is the natural unit.
#'
#' @details
#' Writing \eqn{A_j = \ell_j - (k_j/2)\log(NT)}, four of the five options are
#' the same construction with different learning rates \eqn{\eta}:
#' \eqn{\log w_j = \eta A_j}, with \eqn{\eta = 1} for \code{"mb2012"},
#' \eqn{1/N} for \code{"mb2016"}, \eqn{1/(NT)} for \code{"nt"} and an estimated
#' \eqn{\hat\eta} for \code{"curvature"}. Only \code{"uip"} alters the penalty
#' instead of the rate.
#'
#' Any \eqn{\eta \neq 1} produces a tempered (power) posterior over models,
#' proportional to \eqn{f(y|M_j)^\eta}, which lies outside the approximation
#' that motivates the criterion. What follows from that depends on how the rate
#' is set. A rate held fixed as the sample grows rescales every log weight by
#' the same factor, so differences between models still diverge with \eqn{N}
#' and the posterior continues to concentrate, more slowly when
#' \eqn{\eta < 1} and faster when \eqn{\eta > 1}.
#'
#' A rate that shrinks with the sample behaves differently. Under
#' \eqn{\eta = 1/N}, a model at Kullback-Leibler distance \eqn{c_j} per entity
#' has \eqn{\eta A_j = A_j/N \to -c_j}, so the weights converge to the fixed
#' positive constants \eqn{\exp(-c_j)} and the posterior never concentrates,
#' however large \eqn{N} becomes. The complexity penalty
#' \eqn{(k_j/2)\log(NT)/N} vanishes in the same limit, so overfitted models are
#' not eventually rejected. Conversely \eqn{\eta = 1} concentrates the
#' posterior sharply, which on likelihoods of this kind can place essentially
#' all mass on a single model.
#'
#' The \code{"curvature"} rate addresses this by estimating \eqn{\eta} rather
#' than assuming it. When a likelihood overstates the information in the data,
#' the observed information \eqn{H} and the outer product of the per-entity
#' scores \eqn{J} disagree, and rescaling the log-likelihood by
#' \eqn{\eta = \dim(\theta) / \mathrm{tr}(H^{-1}J)} restores nominal
#' calibration. The rate equals one exactly when \eqn{H = J}, that is when the
#' information matrix equality holds and the likelihood is correctly specified.
#' Both matrices are available from the automatic differentiation used to fit
#' the model space, so \eqn{\mathrm{tr}(H^{-1}J)} and \eqn{\dim(\theta)} are
#' computed per model and stored; \eqn{\eta} is the median of their ratio
#' across the model space, and the realised value is returned in the
#' \code{eta} element.
#'
#' Model spaces fitted before these quantities were stored do not carry them.
#' In that case \eqn{\eta} is approximated under the assumption \eqn{J = cH},
#' for which \eqn{\mathrm{tr}(H^{-1}J) = c\dim(\theta)}, taking \eqn{c} to be
#' the mean squared ratio of robust to Hessian-based standard errors; a message
#' is emitted and re-estimating the model space gives the exact adjustment.
#'
#' The default is retained for compatibility with the published literature;
#' users are encouraged to check the sensitivity of their conclusions to this
#' choice. Switching between the options requires no re-estimation, since all
#' are recovered from the same fitted model space.
#'
#' @section Identification of the robust standard deviations:
#' The columns \code{PSDR} and \code{PSDRcon}, and the
#' \code{weighting = "curvature"} learning rate, are derived from the
#' sandwich covariance \eqn{H^{-1} J H^{-1}}, where
#' \eqn{J = \sum_{i=1}^{N} s_i s_i'} is the outer product of the entity-level
#' score vectors. \eqn{J} has rank at most \eqn{N}, so it is singular for any
#' model with at least as many parameters as there are entities. When that
#' happens the diagonal of the sandwich is not a consistent variance estimate:
#' some linear combinations of the parameters are assigned zero estimated
#' variance, and the remaining entries depend appreciably on how the
#' derivatives are obtained. This is the too-few-clusters problem of
#' cluster-robust inference, with entities in the role of clusters, and it is a
#' property of the design rather than of the implementation.
#'
#' \code{optim_model_space} warns when this occurs. The likelihood, the
#' posterior means (\code{PM}, \code{PMcon}) and the posterior inclusion
#' probabilities (\code{PIP}) do not involve \eqn{J} and are unaffected, as are
#' the Hessian-based standard deviations \code{PSD} and \code{PSDcon}. Robust
#' standard deviations should be reported only when \eqn{\dim(\theta) < N},
#' and comfortably so.
#'
#' @return An object of class \code{badp_bma}, which is a list containing:
#'
#' \describe{
#'   \item{uniform_table}{A table containing the results based on the binomial model prior.}
#'   \item{random_table}{A table containing the results based on the binomial-beta model prior.}
#'   \item{reg_names}{A vector containing the names of the regressors, used by the functions.}
#'   \item{R}{The total number of regressors.}
#'   \item{num_of_models}{The number of models present in the model space.}
#'   \item{jointness_data}{A table containing model IDs and posterior model probabilities (PMPs) for the jointness function.}
#'   \item{best_models_data}{A table containing model IDs, PMPs, coefficients, standard deviations, and standardized regression coefficients (stdRs) for the best_models function.}
#'   \item{EMS}{The expected model size for the binomial and binomial-beta model priors, as specified by the user (default is EMS = R/2).}
#'   \item{size_priors}{A table of uniform and random model priors distributed over model sizes for the model_sizes function.}
#'   \item{PMPs}{A table containing the posterior model probabilities for use in the model_sizes function.}
#'   \item{model_priors}{A table containing the model priors, used by the model_pmp function.}
#'   \item{dilution}{A parameter indicating whether the priors were diluted, used in the model_sizes function.}
#'   \item{alphas}{A matrix of coefficients for the lagged dependent variable across all models, used in the coef_hist function.}
#'   \item{betas_nonzero}{A matrix of nonzero coefficients for the regressors, used in the coef_hist function.}
#'   \item{df_free}{A table containing the degrees of freedom for the estimated models in the best_models function.}
#'   \item{PMS_table}{A table containing the prior and posterior expected model sizes for the binomial and binomial-beta model priors.}
#'   \item{omega}{The dilution parameter used (the exponent of the determinant). Relevant only when \code{dilution = 1}.}
#' }
#'
#' @section Methods:
#' Objects of class \code{badp_bma} have the following methods available:
#' \itemize{
#'   \item \code{\link{print.badp_bma}} - Display results
#'   \item \code{\link{summary.badp_bma}} - Detailed statistical summary
#'   \item \code{\link{coef.badp_bma}} - Extract coefficients
#'   \item \code{\link{plot.badp_bma}} - Visualize results
#' }
#'
#' @export
#'
#' @import stats rje
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
#' }
bma <- function(
  model_space,
  round = 4,
  EMS = NULL,
  dilution = 0,
  omega = 0.5,
  weighting = c("mb2016", "mb2012", "nt", "curvature", "uip")
) {
  weighting <- match.arg(weighting)

  if (!is.null(model_space$convergence)) {
    n_nonconverged <- sum(model_space$convergence["converged", ] == 0)
    if (n_nonconverged > 0) {
      warning(sprintf(
        paste("%d of %d models in the model space did not converge; their",
              "posterior model probabilities may be understated. See the",
              "'convergence' element of the model space object."),
        n_nonconverged, ncol(model_space$convergence)
      ))
    }
  }

  reg_names <- model_space[[3]]
  # Regressors with lag
  K <- length(reg_names)
  # Regressors without lag
  R <- K - 1

  num_of_models <- 2^R
  observations_num <- model_space[[4]]

  model_space_params <- model_space[[1]]
  like_table <- model_space[[2]]

  likes <- matrix(like_table[2, ], nrow = 1, ncol = num_of_models)
  std <- like_table[3:(2 + K), ]
  stdR <- like_table[(3 + K):(2 + 2 * K), ]
  alphas <- matrix(model_space_params[1, ], nrow = 1, ncol = num_of_models)

  first_beta_row <- which(startsWith(rownames(model_space_params), "beta_"))[1]

  betas <- model_space_params[first_beta_row:(first_beta_row - 1 + R), ]
  reg_ID <- rje::powerSetMat(R)
  colnames(reg_ID) <- reg_names[2:K]

  # Marginal-likelihood approximation. Row 2 of the model space statistics is
  # stored as exp(A_j / N) with A_j = loglik_j - (k_j/2)*log(N*T), the form
  # used by Moral-Benito's implementation. Because that transformation is
  # invertible, all three weightings below can be recovered from a fitted
  # model space without re-estimating anything. See weighting_log_weights().
  n_entities <- length(unique(model_space[[5]][[2]]))
  n_periods <- nrow(model_space[[5]]) / n_entities - 1
  n_params <- rowSums(reg_ID) + 1        # k_j: regressors in model j, plus alpha

  # The sandwich covariance, and hence the curvature-adjusted learning rate,
  # rests on J = sum_i s_i s_i', which has rank at most n_entities. Models
  # with at least as many parameters as entities give a singular J; see
  # warn_if_rank_deficient().
  n_rank_deficient <- n_rank_deficient_models(like_table, n_entities, K = K)

  if (weighting == "curvature" && n_rank_deficient > 0) {
    warning(sprintf(
      paste("weighting = \"curvature\" estimates the learning rate from",
            "tr(H^-1 J), but %d of %d models have at least as many",
            "parameters as there are entities (N = %d), so J is singular",
            "and the estimated rate is not identified. Values above one,",
            "which the adjustment cannot take when the estimate is",
            "meaningful, are a symptom of this. Prefer a fixed learning",
            "rate for this model space."),
      n_rank_deficient, ncol(like_table), n_entities
    ), call. = FALSE)
  }

  eta <- if (weighting == "curvature") {
    estimate_curvature_rate(std, stdR, like_table = like_table, K = K)
  } else NULL

  likes <- matrix(
    weighting_log_weights(
      scaled_log_weight = log(as.numeric(likes)),
      weighting         = weighting,
      n_params          = n_params,
      n_entities        = n_entities,
      n_periods         = n_periods,
      eta               = eta
    ),
    nrow = 1, ncol = num_of_models
  )

  table <- t(rbind(alphas, betas, std, stdR, likes))

  table_names <- matrix(0, nrow = 1, ncol = (3 * K + 1))
  table_names[, 1:K] <- paste0(reg_names[1:K], "_coef")
  table_names[, (K + 1):(2 * K)] <- paste0(reg_names[1:K], "_std")
  table_names[, (2 * K + 1):(3 * K)] <- paste0(reg_names[1:K], "_stdR")
  table_names[, (3 * K + 1)] <- "liks"
  colnames(table) <- table_names

  BestModels_prep <- table[, -(3 * K + 1)]

  table <- cbind(reg_ID, table)

  table[is.na(table)] <- 0
  BestModels_prep[is.na(BestModels_prep)] <- 0

  ##### MODEL PRIORS:
  # binomial (Sala-I-Martin et al. 2004) [uniform];
  # binomial-beta (Ley and Steel 2009) [random]

  # Expected model prior
  if (is.null(EMS)) {
    EMS <- R / 2
  }
  if (EMS < 0.01 | EMS > R) {
    message("EMS was changed to R/2 (R - number of regressors)")
    EMS <- R / 2
  }

  uniform_models <- matrix(0, nrow = num_of_models, ncol = 1) # vector to store BINOMIAL probabilities ON MODELS
  uniform_sizes <- matrix(0, nrow = K, ncol = 1) # vector to store BINOMIAL probabilities ON MODEL SIZES
  random_models <- matrix(0, nrow = num_of_models, ncol = 1) # vector to store BINOMIAL-BETA probabilities ON MODELS
  random_sizes <- matrix(0, nrow = K, ncol = 1) # vector to store BINOMIAL-BETA probabilities ON MODEL SIZES

  r <- matrix(apply(table[, 1:R], 1, sum), nrow = num_of_models, ncol = 1)

  for (i in 1:num_of_models) {
    uniform_models[i, 1] <- ((EMS / R)^r[i, 1]) * (1 - EMS / R)^(R - r[i, 1])
    random_models[i, 1] <- gamma(1 + r[i, 1]) * gamma((R - EMS) / EMS + R - r[i, 1])
  }

  random_models <- random_models / sum(random_models) # here we do scaling

  ###### CONDITION for dilution prior
  if (dilution == 1) {
    df <- model_space[[5]]
    for_dilut <- df[, -(1:3)]
    for_dilut <- na.omit(for_dilut)
    dilut <- matrix(0, nrow = num_of_models, ncol = 1)
    dilut_sums <- matrix(rowSums(table[, 1:R]), nrow = num_of_models, ncol = 1)

    for (i in 1:num_of_models) {
      if (dilut_sums[i, 1] < 2) {
        dilut[i, 1] <- 1
      } else {
        cols_to_extract <- which(table[i, 1:R] == 1)
        dilut[i, 1] <- (det(stats::cor(for_dilut[, cols_to_extract, drop = FALSE])))^omega
      }
    }

    uniform_models <- uniform_models * dilut
    random_models <- random_models * dilut
    uniform_models <- uniform_models / sum(uniform_models)
    random_models <- random_models / sum(random_models)
  }
  ##################################

  PMP_uniform <- uniform_models * table[, (3 * K + R + 1)]
  PMP_uniform <- PMP_uniform / sum(PMP_uniform)
  PMP_random <- random_models * table[, (3 * K + R + 1)]
  PMP_random <- PMP_random / sum(PMP_random)

  ##### FOR model_sizes
  sizes <- matrix(0, nrow = R + 1, ncol = 1) # vector to store number of models in a given model size

  for (k in 0:R) { # at this LOOP we add all combinations of regressors up models with R variables
    sizes[k + 1, 1] <- choose(R, k) # number of models of the size k out of R regressors
  }

  ind <- matrix(cumsum(sizes), nrow = R + 1, ncol = 1) # we create a vector with the number of models in each model size category

  for_sizes <- cbind(rowSums(reg_ID), reg_ID, uniform_models, random_models)
  for_sizes <- for_sizes[order(for_sizes[, 1]), ]
  uniform_models_ordered <- matrix(for_sizes[, (R + 2)], nrow = num_of_models, ncol = 1)
  random_models_ordered <- matrix(for_sizes[, (R + 3)], nrow = num_of_models, ncol = 1)

  for (i in 1:(R + 1)) {
    if (i == 1) {
      uniform_sizes[i, 1] <- uniform_models_ordered[1, 1]
      random_sizes[i, 1] <- random_models_ordered[1, 1]
    } # we collect probabilities for different model sizes: the case of the model with no regressors
    else {
      uniform_sizes[i, 1] <- sum(uniform_models_ordered[(ind[i - 1] + 1):ind[i], 1])
      random_sizes[i, 1] <- sum(random_models_ordered[(ind[i - 1] + 1):ind[i], 1])
    } # we collect probabilities for different model sizes: the case of models with regressors
  }
  ######################################################

  for_PM_uniform <- matrix(0, nrow = num_of_models, ncol = K)
  for_PM_random <- matrix(0, nrow = num_of_models, ncol = K)
  for_var_uniform <- matrix(0, nrow = num_of_models, ncol = K)
  for_var_random <- matrix(0, nrow = num_of_models, ncol = K)
  for_varR_uniform <- matrix(0, nrow = num_of_models, ncol = K)
  for_varR_random <- matrix(0, nrow = num_of_models, ncol = K)

  for (i in 1:K) {
    for_PM_uniform[, i] <- table[, R + i] * PMP_uniform
    for_PM_random[, i] <- table[, R + i] * PMP_random
    for_var_uniform[, i] <- (table[, K + R + i]^2) * PMP_uniform
    for_var_random[, i] <- (table[, K + R + i]^2) * PMP_random
    for_varR_uniform[, i] <- (table[, 2 * K + R + i]^2) * PMP_uniform
    for_varR_random[, i] <- (table[, 2 * K + R + i]^2) * PMP_random
  }

  PM_uniform <- matrix(colSums(for_PM_uniform), nrow = K, ncol = 1)
  PM_random <- matrix(colSums(for_PM_random), nrow = K, ncol = 1)
  var_uniform <- matrix(colSums(for_var_uniform), nrow = K, ncol = 1)
  var_random <- matrix(colSums(for_var_random), nrow = K, ncol = 1)
  varR_uniform <- matrix(colSums(for_varR_uniform), nrow = K, ncol = 1)
  varR_random <- matrix(colSums(for_varR_random), nrow = K, ncol = 1)

  ind_variables <- matrix(table[, (R + 1):(R + K)], nrow = num_of_models, ncol = K)
  PM_dev_uniform_prep <- matrix(0, nrow = num_of_models, ncol = K)
  PM_dev_random_prep <- matrix(0, nrow = num_of_models, ncol = K)

  for (i in 1:K) {
    PM_dev_uniform_prep[, i] <- ((ind_variables[, i] - PM_uniform[i, 1])^2) * PMP_uniform
    PM_dev_random_prep[, i] <- ((ind_variables[, i] - PM_random[i, 1])^2) * PMP_random
  }

  PM_dev_uniform <- matrix(colSums(PM_dev_uniform_prep), nrow = K, ncol = 1)
  PM_dev_random <- matrix(colSums(PM_dev_random_prep), nrow = K, ncol = 1)

  VAR_uniform <- PM_dev_uniform + var_uniform
  VAR_random <- PM_dev_random + var_random
  VAR_R_uniform <- PM_dev_uniform + varR_uniform
  VAR_R_random <- PM_dev_random + varR_random

  PSD_uniform <- (PM_dev_uniform + var_uniform)^(0.5)
  PSD_random <- (PM_dev_random + var_random)^(0.5)
  PSD_R_uniform <- (PM_dev_uniform + varR_uniform)^(0.5)
  PSD_R_random <- (PM_dev_random + varR_random)^(0.5)

  reg_ID <- table[, 1:R]
  alphas <- matrix(table[, R + 1], nrow = num_of_models, ncol = 1)
  betas <- table[, (R + 2):(2 * R + 1)]
  betas_nonzero <- matrix(0, nrow = num_of_models / 2, ncol = R)
  PM_uniform_nonzero <- matrix(0, nrow = num_of_models / 2, ncol = R)
  PM_random_nonzero <- matrix(0, nrow = num_of_models / 2, ncol = R)
  Positive_betas <- matrix(0, nrow = R, ncol = 1)
  Positive_alpha <- 0
  df_free <- matrix(0, nrow = num_of_models, ncol = 1) # Degrees of freedom
  reg_sums <- matrix(rowSums(reg_ID), nrow = num_of_models, ncol = 1)

  for (i in 1:num_of_models) {
    df_free[i, 1] <- observations_num - reg_sums[i, 1] - 1
    if (alphas[i, 1] > 0) {
      Positive_alpha <- 1 / num_of_models + Positive_alpha
    }
  }

  for (i in 1:R) {
    k <- 1
    for (j in 1:num_of_models) {
      if (betas[j, i] != 0) {
        betas_nonzero[k, i] <- betas[j, i]
        PM_uniform_nonzero[k, i] <- PMP_uniform[j, 1]
        PM_random_nonzero[k, i] <- PMP_random[j, 1]
        if (betas[j, i] > 0) {
          Positive_betas[i, 1] <- 2 / num_of_models + Positive_betas[i, 1]
        }
        k <- k + 1
      }
    }
  }

  PIP_uniform <- rbind(1, matrix(apply(PM_uniform_nonzero, 2, sum), nrow = R, ncol = 1))
  PIP_random <- rbind(1, matrix(apply(PM_random_nonzero, 2, sum), nrow = R, ncol = 1))
  Positive <- rbind(as.numeric(Positive_alpha), Positive_betas) * 100

  con_PM_uniform <- PM_uniform / PIP_uniform
  con_PM_random <- PM_random / PIP_random

  con_PSD_uniform <- ((VAR_uniform + PM_uniform^2) / PIP_uniform - con_PM_uniform^2)^(0.5)
  con_PSD_random <- ((VAR_random + PM_random^2) / PIP_random - con_PM_random^2)^(0.5)
  con_PSD_R_uniform <- ((VAR_R_uniform + PM_uniform^2) / PIP_uniform - con_PM_uniform^2)^(0.5)
  con_PSD_R_random <- ((VAR_R_random + PM_random^2) / PIP_random - con_PM_random^2)^(0.5)

  uniform_table <- cbind(PIP_uniform, PM_uniform, PSD_uniform, PSD_R_uniform, con_PM_uniform, con_PSD_uniform, con_PSD_R_uniform, Positive)
  random_table <- cbind(PIP_random, PM_random, PSD_random, PSD_R_random, con_PM_random, con_PSD_random, con_PSD_R_random, Positive)
  uniform_table <- round(uniform_table, round)
  random_table <- round(random_table, round)

  bma_names <- c("PIP", "PM", "PSD", "PSDR", "PMcon", "PSDcon", "PSDRcon", "%(+)")

  colnames(uniform_table) <- bma_names
  row.names(uniform_table) <- reg_names
  colnames(random_table) <- bma_names
  row.names(random_table) <- reg_names

  uniform_table[1, 1] <- NA
  random_table[1, 1] <- NA

  PIPs <- cbind(PIP_uniform, PIP_random) # Table with PIP under different model priors for Jointness function
  forJointness <- cbind(reg_ID, PMP_uniform, PMP_random) # Table with model IDs and PMPs for Jointness function
  forBestModels <- cbind(reg_ID, BestModels_prep, PMP_uniform, PMP_random) # Table with model IDs, coefs, stds, stdRs, PMP_uniform, PMP_random for bestModels function
  sizePriors <- cbind(uniform_sizes, random_sizes) # Table with uniform and random model priors spread over model sizes
  PMPs <- cbind(reg_ID, PMP_uniform, PMP_random)
  modelPriors <- cbind(uniform_models, random_models)

  PriorMS <- matrix(EMS, nrow = 1, ncol = 2)
  PosteriorMS <- matrix((colSums(PIPs) - 1), nrow = 1, ncol = 2)
  PMStable <- round(t(rbind(PriorMS, PosteriorMS)), round)
  colnames(PMStable) <- c("Prior model size", "Posterior model size")
  row.names(PMStable) <- c("Binomial", "Binomial-beta")

  bma_list <- list(
    uniform_table, random_table, reg_names, R, num_of_models, forJointness,
    forBestModels, EMS, sizePriors, PMPs, modelPriors, dilution,
    alphas, betas_nonzero, df_free, PMStable, omega, weighting,
    switch(weighting,
           mb2012    = 1,
           mb2016    = 1 / n_entities,
           nt        = 1 / (n_entities * n_periods),
           curvature = eta,
           uip       = NA_real_)
  )
  names(bma_list) <- c(
    "uniform_table", "random_table", "reg_names", "R",
    "num_of_models", "jointness_data", "best_models_data",
    "EMS", "size_priors", "PMPs", "model_priors", "dilution",
    "alphas", "betas_nonzero", "df_free", "PMS_table", "omega",
    "weighting", "eta"
  )
  class(bma_list) <- "badp_bma"

  return(bma_list)
}
