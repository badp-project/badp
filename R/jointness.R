#' Calculation of of the jointness measures
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
#' Hofmarcher P, Crespo Cuaresma J, Grün B, Humer S, Moser M (2018) Bivariate jointness measures in Bayesian Model Averaging: Solving the conundrum. Journal of Macroeconomics, 57, 150-165. doi: 10.1016/j.jmacro.2018.05.005 \cr
#' Ley E, Steel M (2007) Jointness in Bayesian variable selection with applications to growth regression. Journal of Macroeconomics, 29(3), 476-493. doi: 10.1016/j.jmacro.2006.12.002
#'
#' @param bma_list bma object (the result of the bma function)
#' @param measure Parameter for choosing the measure of jointness: \cr
#' HCGHM - for Hofmarcher et al. (2018) measure; \cr
#' LS - for Ley & Steel (2007) measure; \cr
#' DW - for Doppelhofer & Weeks (2009) measure; \cr
#' PPI - for posterior probability of including both variables.
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
#' data_prepared <- bdsm::economic_growth[, 1:6] %>%
#'   bdsm::feature_standardization(
#'     excluded_cols = c(country, year, gdp)
#'   ) %>%
#'   bdsm::feature_standardization(
#'     group_by_col  = year,
#'     excluded_cols = country,
#'     scale         = FALSE
#'   )
#'
#' bma_results <- bma(
#'   model_space = bdsm::small_model_space,
#'   round       = 3,
#'   dilution    = 0
#' )
#'
#' jointness_table <- jointness(bma_results, measure = "HCGHM", rho = 0.5, round = 3)
#' }


jointness <- function(bma_list, measure = "HCGHM", rho = 0.5, round = 3){

  # Extraction of the elements of the bma object
  reg_names <- bma_list[[3]] # vector with names of the regressors from bma object
  reg_names <- reg_names[-1]
  R <- bma_list[[4]] # total number of regressors from bma object
  M <- bma_list[[5]] # size of the mode space from bma object
  forJointness <- bma_list[[6]] # matrix from bma object
  reg_ID <- forJointness[,1:R] # we extract regressor IDs
  PMP_uniform <- as.matrix(forJointness[,R+1]) # PMP under unifrom prior
  PMP_random <- as.matrix(forJointness[,R+2]) # PMP under random prior

  reg_ID2 <- matrix(0, nrow = M, ncol = R)

  for (i in 1:M){
    for (j in 1:R){
      if (reg_ID[i,j]==1){
        reg_ID2[i,j]=j
      }
    }
  }

  reg_ID <-reg_ID2

  # Information about pairs of regressors: number of pairs, list of all possible regressors pairs
  c = choose(R,2) # number of pairs e.i. jointness measures
  Pairs <- as.matrix(utils::combn(1:R,2)) # a list of all possible pairs of regressors

  # introducing notation a matrices to store posterior objects
  PMP_uniform_a_b <- matrix(0, nrow = c, ncol = 1) # a_b - P(a and b) for uniform prior
  PMP_uniform_Na_b <- matrix(0, nrow = c, ncol = 1) # Na_b - P(NOT a and b) for uniform prior
  PMP_uniform_a_Nb <- matrix(0, nrow = c, ncol = 1) # a_Nb - P(a and NOT b) for uniform prior
  PMP_uniform_Na_Nb <- matrix(0, nrow = c, ncol = 1) # Na_Nb - P(NOT a and NOT b) for uniform prior
  PMP_random_a_b <- matrix(0, nrow = c, ncol = 1) # a_b - P(a and b) for random prior
  PMP_random_Na_b <- matrix(0, nrow = c, ncol = 1) # Na_b - P(NOT a and b) for random prior
  PMP_random_a_Nb <- matrix(0, nrow = c, ncol = 1) # a_Nb - P(a and NOT b) for random prior
  PMP_random_Na_Nb <- matrix(0, nrow = c, ncol = 1) # Na_Nb - P(NOT a and NOT b) for random prior

  for (j in 1:c){
    a <- Pairs[1,j] # ID of the first regressor
    b <- Pairs[2,j] # ID of the second regressor
    for (i in 1:M){
      aIN <- 0 # setting the initial value before the LOOP
      bIN <- 0 # setting the initial value before the LOOP
      for (t in 1:R){
        if (reg_ID[i,t]==a){aIN <- 1} # CONDITION for presence of the regressors a in the model
        if (reg_ID[i,t]==b){bIN <- 1} # CONDITION for presence of the regressors b in the model
      }
      if (aIN==1&bIN==1){# CONDNION for both variables being included in the model
        PMP_uniform_a_b[j,1] = PMP_uniform_a_b[j,1] + PMP_uniform[i,1]
        PMP_random_a_b[j,1] = PMP_random_a_b[j,1] + PMP_random[i,1]
      }
      else if(aIN==0&bIN==1) {# CONDNION for only regressor b being included in the model
        PMP_uniform_Na_b[j,1] = PMP_uniform_Na_b[j,1] + PMP_uniform[i,1]
        PMP_random_Na_b[j,1] = PMP_random_Na_b[j,1] + PMP_random[i,1]
      }
      else if(aIN==1&bIN==0) {# CONDNION for only regressor a being included in the model
        PMP_uniform_a_Nb[j,1] = PMP_uniform_a_Nb[j,1] + PMP_uniform[i,1]
        PMP_random_a_Nb[j,1] = PMP_random_a_Nb[j,1] + PMP_random[i,1]
      }
      else if(aIN==0&bIN==0) {# CONDNION for both variables being excluded from the model
        PMP_uniform_Na_Nb[j,1] = PMP_uniform_Na_Nb[j,1] + PMP_uniform[i,1]
        PMP_random_Na_Nb[j,1] = PMP_random_Na_Nb[j,1] + PMP_random[i,1]
      }
    }
  }

  # MEASURES OF JOINTNESS
  if (measure=="HCGHM"){
    PMP_uniform_HCGHM_m <- matrix(0, nrow = c, ncol = 1) # Hofmarcher et al. (2018)
    PMP_random_HCGHM_m <- matrix(0, nrow = c, ncol = 1) # Hofmarcher et al. (2018)
    for (j in 1:c){
      PMP_uniform_HCGHM_m[j,1] = ((PMP_uniform_a_b[j,1]+rho)*(PMP_uniform_Na_Nb[j,1]+rho)-(PMP_uniform_Na_b[j,1]+rho)*(PMP_uniform_a_Nb[j,1]+rho))/((PMP_uniform_a_b[j,1]+rho)*(PMP_uniform_Na_Nb[j,1]+rho)+(PMP_uniform_Na_b[j,1]+rho)*(PMP_uniform_a_Nb[j,1]+rho)-rho)
      PMP_random_HCGHM_m[j,1] = ((PMP_random_a_b[j,1]+rho)*(PMP_random_Na_Nb[j,1]+rho)-(PMP_random_Na_b[j,1]+rho)*(PMP_random_a_Nb[j,1]+rho))/((PMP_random_a_b[j,1]+rho)*(PMP_random_Na_Nb[j,1]+rho)+(PMP_random_Na_b[j,1]+rho)*(PMP_random_a_Nb[j,1]+rho)-rho)
    }
    first <- PMP_uniform_HCGHM_m
    second <- PMP_random_HCGHM_m
  }
  if (measure=="LS"){
    PMP_uniform_LS_m <- matrix(0, nrow = c, ncol = 1) # Ley & Steel (2007)
    PMP_random_LS_m <- matrix(0, nrow = c, ncol = 1) # Ley & Steel (2007)
    for (j in 1:c){
      PMP_uniform_LS_m[j,1] = PMP_uniform_a_b[j,1] / (PMP_uniform_Na_b[j,1] + PMP_uniform_a_Nb[j,1])
      PMP_random_LS_m[j,1] = PMP_random_a_b[j,1] / (PMP_random_Na_b[j,1] + PMP_random_a_Nb[j,1])
    }
    first <- PMP_uniform_LS_m
    second <- PMP_random_LS_m
  }
  if (measure=="DW"){
    PMP_uniform_DW_m <- matrix(0, nrow = c, ncol = 1) # Doppelhofer & Weeks (2009)
    PMP_random_DW_m <- matrix(0, nrow = c, ncol = 1) # Doppelhofer & Weeks (2009)
    for (j in 1:c){
      PMP_uniform_DW_m[j,1] = log((PMP_uniform_a_b[j,1] / PMP_uniform_Na_b[j,1])*(PMP_uniform_Na_Nb[j,1] / PMP_uniform_a_Nb[j,1]))
      PMP_random_DW_m[j,1] = log((PMP_random_a_b[j,1] / PMP_random_Na_b[j,1])*(PMP_random_Na_Nb[j,1] / PMP_random_a_Nb[j,1]))
    }
    first <- PMP_uniform_DW_m
    second <- PMP_random_DW_m
  }
  if (measure=="PPI"){
    first <- PMP_uniform_a_b
    second <- PMP_random_a_b
  }

  # Table to store the jointness results
  jointness_table <- matrix(1, nrow = R , ncol = R)

  for (j in 1:c){
    # ABOVE THE DIAGONAL
    jointness_table[Pairs[1,j],Pairs[2,j]] = first[j,1]
    # BELOW THE DIAGONAL
    jointness_table[Pairs[2,j],Pairs[1,j]] = second[j,1]
  }

  # Rounding up the numbers in a table
  jointness_table <- round(jointness_table, digits=round)

  for (i in 1:R){
    jointness_table[i,i] = NA
  }

  # Adding names to rows and columns
  rownames(jointness_table) <- reg_names # we add regressor names to the rows
  colnames(jointness_table) <- reg_names # we add regressor names to the columns

  # creation of the Jointness object
  return(jointness_table)
}
