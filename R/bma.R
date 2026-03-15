#' Calculation of the bma object
#'
#' This function calculates BMA statistics based on the provided model space.
#' Other objects for furhter analysis are also returned.
#'
#' @param model_space List with params and stats from the model space
#' @param round Parameter indicating the decimal place to which number in the BMA tables and prior and posterior model sizes should be rounded (default round = 4)
#' @param EMS Expected model size for model binomial and binomial-beta model prior
#' @param dilution Binary parameter: 0 - NO application of a dilution prior; 1 - application of a dilution prior (George 2010).
#' @param dil.Par Parameter associated with dilution prior - the exponent of the determinant (George 2010). Used only if parameter dilution = 1.
#'
#' @return A list with 16 elements.
#'
#' \describe{
#'   \item{uniform_table}{A table containing the results based on the binomialmodel prior.}
#'   \item{random_table}{A table containing the results based on the binomial-beta model prior.}
#'   \item{reg_names}{A vector containing the names of the regressors, used by the functions.}
#'   \item{R}{The total number of regressors.}
#'   \item{num_of_models}{The number of models present in the model space.}
#'   \item{forJointnes}{A table containing model IDs and posterior model probabilities (PMPs) for the jointness function.}
#'   \item{forBestModels}{A table containing model IDs, PMPs, coefficients, standard deviations,and standardized regression coefficients (stdRs) for the best_models function.}
#'   \item{EMS}{The expected model size for the binomial and binomial-beta model priors, as specified by the user (default is EMS = R/2).}
#'   \item{sizePriors}{A table of uniform and random model priors distributed over model sizes for the model_sizes function.}
#'   \item{PMPs}{A table containing the posterior model probabilities for use in the model_sizes function.}
#'   \item{modelPriors}{A table containing the model priors, used by the model_pmp function.}
#'   \item{dilution}{A parameter indicating whether the priors were diluted, used in the model_sizes function.}
#'   \item{alphas}{A vector of coefficients for the lagged dependent variable in the coef_hist function.}
#'   \item{betas_nonzero}{A vector of nonzero coefficients for the regressors in the coef_hist function.}
#'   \item{d_free}{A table containing the degrees of freedom for the estimated models in the best_models function.}
#'   \item{PMStable}{A table containing the prior and posterior expected model sizes for the binomial and binomial-beta model priors.}
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
    dil.Par = 0.5
  ){

  reg_names <- model_space[[3]]
  # Regressors with lag
  K <- length(reg_names)
  # Regressors without lag
  R <- K-1

  num_of_models <- 2^R
  observations_num <- model_space[[4]]

  model_space_params <- model_space[[1]]
  like_table <- model_space[[2]]

  likes <- matrix(like_table[2,], nrow = 1, ncol = num_of_models)
  std <- like_table[3:(2+K),]
  stdR <- like_table[(3+K):(2+2*K),]
  alphas <- matrix(model_space_params[1,], nrow = 1, ncol = num_of_models)

  first_beta_row <- which(startsWith(rownames(model_space_params), "beta_"))[1]

  betas <- model_space_params[first_beta_row:(first_beta_row-1+R),]
  reg_ID <- rje::powerSetMat(R)
  colnames(reg_ID) <- reg_names[2:K]

  table <- t(rbind(alphas, betas, std, stdR, likes))

  table_names<-matrix(0, nrow = 1, ncol = (3*K+1))
  table_names[,1:K] <- paste0(reg_names[1:K], "_coef")
  table_names[,(K+1):(2*K)] <- paste0(reg_names[1:K], "_std")
  table_names[,(2*K+1):(3*K)] <- paste0(reg_names[1:K], "_stdR")
  table_names[,(3*K+1)] <- "liks"
  colnames(table) <- table_names

  BestModels_prep <- table[,-(3*K+1)]

  table <- cbind(reg_ID,table)

  table[is.na(table)] <- 0
  BestModels_prep[is.na(BestModels_prep)] <- 0

  ##### MODEL PRIORS:
  # binomial (Sala-I-Martin et al. 2004) [uniform];
  # binomial-beta (Ley and Steel 2009) [random]

  # Expected model prior
  if (is.null(EMS)){EMS = R / 2}
  if (EMS<0.01|EMS>R){
    message("EMS was changed to R/2 (R - number of regressors)")
    EMS = R / 2}

  uniform_models <- matrix(0, nrow = num_of_models, ncol = 1) # vector to store BINOMIAL probabilities ON MODELS
  uniform_sizes <- matrix(0, nrow = K, ncol = 1) # vector to store BINOMIAL probabilities ON MODEL SIZES
  random_models <- matrix(0, nrow = num_of_models, ncol = 1) # vector to store BINOMIAL-BETA probabilities ON MODELS
  random_sizes <- matrix(0, nrow = K, ncol = 1) # vector to store BINOMIAL-BETA probabilities ON MODEL SIZES

  r <- matrix(apply(table[,1:R], 1, sum), nrow = num_of_models, ncol = 1)

  for (i in 1:num_of_models){
    uniform_models[i,1] = ((EMS/R)^r[i,1])*(1-EMS/R)^(R-r[i,1])
    random_models[i,1] = gamma(1+r[i,1])*gamma((R-EMS)/EMS+R-r[i,1])
  }

  random_models <-random_models/sum(random_models) # here we do scaling

  ###### CONDITION for dilution prior
  if (dilution==1){
    if (!exists("dil.Par")) { dil.Par <- 0.5 } # CONDITION for setting the default value of dil.Par
    df <- model_space[[5]]
    for_dilut <- df[,-(1:3)]
    for_dilut <- na.omit(for_dilut)
    dilut <- matrix(0, nrow = num_of_models, ncol = 1)
    dilut_sums <- matrix(rowSums(table[,1:R]), nrow = num_of_models, ncol = 1)

    for (i in 1:num_of_models){
      if (dilut_sums[i,1]<2){
        dilut[i,1] = 1
      }else{
        cols_to_extract <- which(table[i,1:R] == 1)
        dilut[i,1] = (det(stats::cor(for_dilut[, cols_to_extract, drop = FALSE])))^dil.Par
      }
    }

    uniform_models <- uniform_models*dilut
    random_models <- random_models*dilut
    uniform_models <- uniform_models / sum(uniform_models)
    random_models <- random_models / sum(random_models)
  }
  ##################################

  PMP_uniform <- uniform_models*table[,(3*K+R+1)]
  PMP_uniform <- PMP_uniform / sum(PMP_uniform)
  PMP_random <- random_models*table[,(3*K+R+1)]
  PMP_random <- PMP_random / sum(PMP_random)

  ##### FOR model_sizes
  sizes <- matrix(0, nrow = R+1, ncol = 1) # vector to store number of models in a given model size

  for (k in 0:R){ # at this LOOP we add all combinations of regressors up models with R variables
    sizes[k+1,1] <- choose(R,k) # number of models of the size k out of R regressors
  }

  ind <- matrix(cumsum(sizes), nrow = R+1, ncol = 1) # we create a vector with the number of models in each model size category

  for_sizes <- cbind(rowSums(reg_ID),reg_ID,uniform_models,random_models)
  for_sizes <- for_sizes[order(for_sizes[,1]), ]
  uniform_models_ordered <- matrix(for_sizes[,(R+2)], nrow = num_of_models, ncol = 1)
  random_models_ordered <- matrix(for_sizes[,(R+3)], nrow = num_of_models, ncol = 1)

  for (i in 1:(R+1)){
    if (i==1){uniform_sizes[i,1] = uniform_models_ordered[1,1]
    random_sizes[i,1] = random_models_ordered[1,1]} # we collect probabilities for different model sizes: the case of the model with no regressors
    else{uniform_sizes[i,1] = sum(uniform_models_ordered[(ind[i-1]+1):ind[i],1])
    random_sizes[i,1] = sum(random_models_ordered[(ind[i-1]+1):ind[i],1])
    } # we collect probabilities for different model sizes: the case of models with regressors
  }
  ######################################################

  for_PM_uniform <- matrix(0, nrow = num_of_models, ncol = K)
  for_PM_random <- matrix(0, nrow = num_of_models, ncol = K)
  for_var_uniform <- matrix(0, nrow = num_of_models, ncol = K)
  for_var_random <- matrix(0, nrow = num_of_models, ncol = K)
  for_varR_uniform <- matrix(0, nrow = num_of_models, ncol = K)
  for_varR_random <- matrix(0, nrow = num_of_models, ncol = K)

  for (i in 1:K){
    for_PM_uniform[,i] = table[,R+i]*PMP_uniform
    for_PM_random[,i] = table[,R+i]*PMP_random
    for_var_uniform[,i] = (table[,K+R+i]^2)*PMP_uniform
    for_var_random[,i] = (table[,K+R+i]^2)*PMP_random
    for_varR_uniform[,i] = (table[,2*K+R+i]^2)*PMP_uniform
    for_varR_random[,i] = (table[,2*K+R+i]^2)*PMP_random
  }

  PM_uniform <- matrix(colSums(for_PM_uniform), nrow = K, ncol = 1)
  PM_random <- matrix(colSums(for_PM_random), nrow = K, ncol = 1)
  var_uniform <- matrix(colSums(for_var_uniform), nrow = K, ncol = 1)
  var_random <- matrix(colSums(for_var_random), nrow = K, ncol = 1)
  varR_uniform <- matrix(colSums(for_varR_uniform), nrow = K, ncol = 1)
  varR_random <- matrix(colSums(for_varR_random), nrow = K, ncol = 1)

  ind_variables <- matrix(table[,(R+1):(R+K)], nrow = num_of_models, ncol = K)
  PM_dev_uniform_prep <- matrix(0, nrow = num_of_models, ncol = K)
  PM_dev_random_prep <- matrix(0, nrow = num_of_models, ncol = K)

  for (i in 1:K){
    PM_dev_uniform_prep[,i] <- ((ind_variables[,i] - PM_uniform[i,1])^2)*PMP_uniform
    PM_dev_random_prep[,i] <- ((ind_variables[,i] - PM_random[i,1])^2)*PMP_random
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

  reg_ID <- table[,1:R]
  alphas <- matrix(table[,R+1], nrow = num_of_models, ncol = 1)
  betas <- table[,(R+2):(2*R+1)]
  betas_nonzero <- matrix(0, nrow = num_of_models/2, ncol = R)
  PM_uniform_nonzero <- matrix(0, nrow = num_of_models/2, ncol = R)
  PM_random_nonzero <- matrix(0, nrow = num_of_models/2, ncol = R)
  Positive_betas <- matrix(0, nrow = R, ncol = 1)
  Positive_alpha <- 0
  d_free <- matrix(0, nrow = num_of_models, ncol = 1) # Degrees of freedom
  reg_sums <- matrix(rowSums(reg_ID), nrow = num_of_models, ncol = 1)

  for (i in 1:num_of_models){
    d_free[i,1] = observations_num - reg_sums[i,1] - 1
    if(alphas[i,1]>0){
      Positive_alpha <- 1/num_of_models + Positive_alpha
    }
  }

  for (i in 1:R){
    k=1
    for (j in 1:num_of_models){
      if (betas[j,i]!=0){
        betas_nonzero[k,i] = betas[j,i]
        PM_uniform_nonzero[k,i] = PMP_uniform[j,1]
        PM_random_nonzero[k,i] = PMP_random[j,1]
        if (betas[j,i]>0){
          Positive_betas[i,1] = 2/num_of_models + Positive_betas[i,1]
        }
        k <- k + 1
      }
    }
  }

  PIP_uniform <- rbind(1, matrix(apply(PM_uniform_nonzero, 2, sum), nrow = R, ncol = 1))
  PIP_random <- rbind(1, matrix(apply(PM_random_nonzero, 2, sum), nrow = R, ncol = 1))
  Positive <- rbind(as.numeric(Positive_alpha), Positive_betas)*100

  con_PM_uniform <-  PM_uniform / PIP_uniform
  con_PM_random <-  PM_random / PIP_random

  con_PSD_uniform <- ((VAR_uniform + PM_uniform^2) / PIP_uniform - con_PM_uniform^2)^(0.5)
  con_PSD_random <- ((VAR_random + PM_random^2) / PIP_random - con_PM_random^2)^(0.5)
  con_PSD_R_uniform <- ((VAR_R_uniform + PM_uniform^2) / PIP_uniform - con_PM_uniform^2)^(0.5)
  con_PSD_R_random <- ((VAR_R_random + PM_random^2) / PIP_random - con_PM_random^2)^(0.5)

  uniform_table <- cbind(PIP_uniform,PM_uniform,PSD_uniform,PSD_R_uniform,con_PM_uniform,con_PSD_uniform,con_PSD_R_uniform,Positive)
  random_table <- cbind(PIP_random,PM_random,PSD_random,PSD_R_random,con_PM_random,con_PSD_random,con_PSD_R_random,Positive)
  uniform_table <- round(uniform_table, round)
  random_table <- round(random_table, round)

  bma_names <- c("PIP", "PM", "PSD", "PSDR","PMcon", "PSDcon", "PSDRcon", "%(+)")

  colnames(uniform_table) <- bma_names
  row.names(uniform_table) <- reg_names
  colnames(random_table) <- bma_names
  row.names(random_table) <- reg_names

  uniform_table[1,1] <- NA
  random_table[1,1] <- NA

  PIPs <- cbind(PIP_uniform, PIP_random) # Table with PIP under different model priors for Jointness function
  forJointness <- cbind(reg_ID, PMP_uniform, PMP_random) # Table with model IDs and PMPs for Jointness function
  forBestModels <- cbind(reg_ID, BestModels_prep, PMP_uniform, PMP_random) # Table with model IDs, coefs, stds, stdRs, PMP_uniform, PMP_random for bestModels function
  sizePriors <- cbind(uniform_sizes, random_sizes) # Table with uniform and random model priors spread over model sizes
  PMPs <- cbind(reg_ID,PMP_uniform, PMP_random)
  modelPriors <- cbind(uniform_models, random_models)

  PriorMS <- matrix(EMS, nrow = 1, ncol = 2)
  PosteriorMS <- matrix((colSums(PIPs)-1), nrow = 1, ncol = 2)
  PMStable <- round(t(rbind(PriorMS,PosteriorMS)),round)
  colnames(PMStable) <- c("Prior model size", "Posterior model size")
  row.names(PMStable) <- c("Binomial", "Binomial-beta")

  bma_list <- list(uniform_table,random_table,reg_names,R,num_of_models,forJointness,
                   forBestModels,EMS,sizePriors,PMPs,modelPriors,dilution,
                   alphas,betas_nonzero,d_free,PMStable)
  names(bma_list) <- c("Table with the binomial model prior results", "Table with the binomial model prior results",
                       "Names of variables", "Number of regressors", "Size of the model space (number of models)",
                       "Table for jointness function","Table for best_models function", "Expected model size",
                       "Table with model size priors","Table with posterior model probabilities","Table with model priors",
                       "Paremeter indication use of dilution","Ceofficients on the lagged dependent variable",
                       "Coefficients on regressors", "degrees of freedom of the models","Table with prior and posterior model sizes")

  return(bma_list)
}
