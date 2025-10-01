#' Table with the best models according to one of the posterior criteria
#'
#' This function creates a ranking of best models according to one of the possible criterion (PMP under binomial model prior, PMP under binomial-beta model prior, R^2 under binomial model prior, R^2 under binomial-beta model prior).
#' The function gives two types of tables in three different formats: inclusion table (where 1 indicates presence of the regressor in the model and 0 indicates that the variable is excluded from the model) and estimation results table (it displays the best models and estimation output for those models: point estimates, standard errors, significance level, and R^2).
#'
#' @param bma_list bma object (the result of the bma function)
#' @param criterion The criterion that will be used for a basis of the model ranking: \cr
#' 1 - binomial model prior \cr
#' 2 - binomial-beta model prior
#' @param best The number of the best models to be considered
#' @param round Parameter indicating the decimal place to which number in the tables should be rounded (default round = 3)
#' @param estimate A parameter with values TRUE or FALSE indicating which table should be displayed when
#' TRUE - table with estimation to the results \cr
#' FALSE - table with the inclusion of regressors in the best models
#' @param robust A parameter with values TRUE or FALSE indicating which type of stanrdard errors should be displayed
#' when the function finishes calculations. Works only if estimate = TRUE. Works well when best is small.\cr
#' TRUE - robust standard errors \cr
#' FALSE - regular standard errors
#'
#' @return A list with best_models objects: \cr
#' 1. matrix with inclusion of the regressors in the best models \cr
#' 2. matrix with estimation output in the best models with regular standard errors \cr
#' 3. matrix with estimation output in the best models with robust standard errors \cr
#' 4. knitr_kable table with inclusion of the regressors in the best models (the best for the display on the console - up to 11 models) \cr
#' 5. knitr_kable table with estimation output in the best models with regular standard errors (the best for the display on the console - up to 6 models) \cr
#' 6. knitr_kable table with estimation output in the best models with robust standard errors (the best for the display on the console - up to 6 models) \cr
#' 7. gTree table with inclusion of the regressors in the best models (displayed as a plot). Use grid::grid.draw() to display.\cr
#' 8. gTree table with estimation output in the best models with regular standard errors (displayed as a plot). Use grid::grid.draw() to display.
#' 9. gTree table with estimation output in the best models with robust standard errors (displayed as a plot). Use grid::grid.draw() to display.
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
#'   df          = data_prepared,
#'   round       = 3,
#'   dilution    = 0
#' )
#'
#' best_5_models <- best_models(bma_results, criterion = 1, best = 5, estimate = TRUE, robust = TRUE)
#' }

best_models <- function(bma_list, criterion = 1, best = 5, round = 3, estimate = TRUE, robust = TRUE){

  R <- bma_list[[4]] # number of regressors from bma object
  K <- R+1 # number of variables
  reg_names <- matrix(bma_list[[3]], nrow = K, ncol = 1) # vector with names of the regressors from bma object
  M <- bma_list[[5]] # size of the mode space from bma object
  info <- bma_list[[7]][,1:(R+3*K)]
  PMP_unifrom <- matrix(bma_list[[7]][,R+3*K+1], nrow = M, ncol = 1)
  PMP_random <- matrix(bma_list[[7]][,R+3*K+2], nrow = M, ncol = 1)
  d_free <-bma_list[[15]]

  if (best>M){
    message("best > M - number of best models cannot be bigger than the total number of models. We set best = M and continiue :)")
    best = M
  }

  # check for the criterion chosen by the user
  if (criterion==1){ranking <- PMP_unifrom}
  if (criterion==2){ranking <- PMP_random}

  Ranking<-cbind(ranking,info,d_free) # we add ranking criterion based on the users choice

  # ordering the models according to PMP criterion
  Ranking <- Ranking[order(Ranking[,1],decreasing=T),] # ordering of the models

  Best_models <- Ranking[1:best, 2:(R+1)] # model IDs
  Ranks <- matrix(round(Ranking[1:best, 1], digits = 3), nrow = best, ncol = 1) # PMPs of the first 'best' models
  bestBetas <- Ranking[1:best, (R+2):(R+K+1)] # coefficients
  bestBetas[bestBetas == 0] <- NA
  bestBetas <- t(round(bestBetas,round))
  bestSTDs <- Ranking[1:best, (R+K+2):(R+2*K+1)] # standard errors
  bestSTDs[bestSTDs == 0] <- NA
  bestSTDs <- t(round(bestSTDs,round))
  bestSTDRs <- Ranking[1:best, (R+2*K+2):(R+3*K+1)] # robust standard errors
  bestSTDRs[bestSTDRs == 0] <- NA
  bestSTDRs <- t(round(bestSTDRs,round))
  best_d_free <- matrix( Ranking[1:best, R+3*K+2], nrow = best, ncol = 1)

  inclusion_table <- t(cbind(matrix(1, nrow = best, ncol = 1), Best_models, Ranks))
  row.names(inclusion_table) <- rbind(reg_names,"PMP")

  names <- matrix(0, nrow = best, ncol = 1)

  for (i in 1:best){
    names[i,1] = paste0("'No. ",i,"'")
  }

  colnames(inclusion_table) <- names

  models_std <- matrix(0, nrow = K, ncol = best)
  models_stdR <- matrix(0, nrow = K, ncol = best)
  p_values <- matrix(0, nrow = K, ncol = best)
  p_valuesR <- matrix(0, nrow = K, ncol = best)
  asterisks <- matrix(0, nrow = K, ncol = best)
  asterisksR <- matrix(0, nrow = K, ncol = best)

  for (i in 1:K){
    for (j in 1:best){
      if (!is.na(bestBetas[i,j])){
        models_std[i,j] = paste0(bestBetas[i,j]," (",bestSTDs[i,j],")")
        models_stdR[i,j] = paste0(bestBetas[i,j]," (",bestSTDRs[i,j],")")
        p_values[i,j] = 2*stats::pt(abs(bestBetas[i,j]/bestSTDs[i,j]), df = best_d_free[j,1], lower.tail = FALSE)
        p_valuesR[i,j] = 2*stats::pt(abs(bestBetas[i,j]/bestSTDRs[i,j]), df =best_d_free[j,1], lower.tail = FALSE)

        if (is.na(p_values[i,j]) || p_values[i,j] >= 0.1){
          asterisks[i,j] = NA
        } else if (p_values[i,j] >= 0.05){
          asterisks[i,j] = "*"
        } else if (p_values[i,j] >= 0.01){
          asterisks[i,j] = "**"
        } else {
          asterisks[i,j]="***"
        }

        if (is.na(p_valuesR[i,j]) || p_valuesR[i,j] >= 0.1){
          asterisksR[i,j] = NA
        } else if (p_valuesR[i,j] >= 0.05){
          asterisksR[i,j] = "*"
        } else if (p_valuesR[i,j] >= 0.01){
          asterisksR[i,j] = "**"
        } else {
          asterisksR[i,j]="***"
        }
      } else{
        models_std[i,j] = NA
        models_stdR[i,j] = NA
        p_values[i,j] = NA
        p_valuesR[i,j] = NA
        asterisks[i,j] = NA
        asterisksR[i,j] = NA
      }
    }
  }

  for (i in 1:K){
    for (j in 1:best){
      if (!is.na(asterisks[i,j])){
        models_std[i,j] = paste0(models_std[i,j],asterisks[i,j])
      }
      if (!is.na(asterisksR[i,j])){
        models_stdR[i,j] = paste0(models_stdR[i,j],asterisksR[i,j])
      }
    }
  }

  models_std <- rbind(models_std, t(Ranks))
  models_stdR <- rbind(models_stdR, t(Ranks))

  colnames(models_std) <- names
  colnames(models_stdR) <- names
  row.names(models_std) <- rbind(reg_names,"PMP")
  row.names(models_stdR) <- rbind(reg_names,"PMP")

  inclusion_2 <- knitr::kable(inclusion_table, row.names = TRUE, align = "c")
  models_std_2 <- knitr::kable(models_std, row.names = TRUE, align = "c")
  models_stdR_2 <- knitr::kable(models_stdR, row.names = TRUE, align = "c")
  inclusion_3 <- grid::grid.grabExpr(gridExtra::grid.table(inclusion_table))
  models_std_3 <- grid::grid.grabExpr(gridExtra::grid.table(models_std))
  models_stdR_3 <- grid::grid.grabExpr(gridExtra::grid.table(models_stdR))


  out <- list(inclusion_table, models_std, models_stdR, inclusion_2, models_std_2,
              models_stdR_2, inclusion_3, models_std_3, models_stdR_3)

  if (estimate==FALSE){
    gridExtra::grid.table(inclusion_table)
  }
  if (estimate==TRUE){
    if (robust==TRUE){
      gridExtra::grid.table(models_stdR)
    }else{
      gridExtra::grid.table(models_std)
    }
  }
  return(out)
}
