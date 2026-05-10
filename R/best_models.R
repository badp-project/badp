#' Table with the best models according to one of the posterior criteria
#'
#' This function creates a ranking of best models according to one of the possible criterion (PMP under binomial model prior, PMP under binomial-beta model prior, R^2 under binomial model prior, R^2 under binomial-beta model prior).
#' The function gives two types of tables in three different formats: inclusion table (where 1 indicates presence of the regressor in the model and 0 indicates that the variable is excluded from the model) and estimation results table (it displays the best models and estimation output for those models: point estimates, standard errors, significance level, and R^2).
#'
#' @param bma_list An object of class \code{badp_bma}, typically returned by \code{\link{bma}}.
#' @param criterion Integer specifying the ranking criterion: \cr
#' \code{1} - binomial model prior (default); \cr
#' \code{2} - binomial-beta model prior.
#' @param best Integer. The number of best models to display (default: 5).
#' @param round Integer indicating the decimal place to which numbers in the tables should be rounded (default: 3).
#' @param estimate A parameter with values TRUE or FALSE indicating which table should be displayed when
#' TRUE - table with the estimation results \cr
#' FALSE - table with the inclusion of regressors in the best models
#' @param robust A parameter with values TRUE or FALSE indicating which type of standard errors should be displayed
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
#' best_5_models <- best_models(bma_results, criterion = 1, best = 5, estimate = TRUE, robust = TRUE)
#' }

best_models <- function(bma_list, criterion = 1, best = 5, round = 3, estimate = TRUE, robust = TRUE){

  R <- bma_list[[4]] # number of regressors from bma object
  K <- R+1 # number of variables
  reg_names <- matrix(bma_list[[3]], nrow = K, ncol = 1) # vector with names of the regressors from bma object
  M <- bma_list[[5]] # size of the mode space from bma object
  info <- bma_list[[7]][,1:(R+3*K)]
  PMP_uniform <- matrix(bma_list[[7]][,R+3*K+1], nrow = M, ncol = 1)
  PMP_random <- matrix(bma_list[[7]][,R+3*K+2], nrow = M, ncol = 1)
  d_free <-bma_list[[15]]

  if (best>M){
    message("best > M - number of best models cannot be bigger than the total number of models. We set best = M and continue :)")
    best = M
  }

  # check for the criterion chosen by the user
  if (criterion==1){ranking <- PMP_uniform}
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
  inclusion_3 <- as_drawable_grob(
    grid::grid.grabExpr(gridExtra::grid.table(inclusion_table)))
  models_std_3 <- as_drawable_grob(
    grid::grid.grabExpr(gridExtra::grid.table(models_std)))
  models_stdR_3 <- as_drawable_grob(
    grid::grid.grabExpr(gridExtra::grid.table(models_stdR)))


  out <- list(inclusion_table, models_std, models_stdR, inclusion_2, models_std_2,
              models_stdR_2, inclusion_3, models_std_3, models_stdR_3)

  # Remember which table the caller asked to display, so the print method
  # can reproduce the original side effect when the result is auto-printed
  # at the top level (i.e. `best_models(...)` without assignment).
  attr(out, "estimate") <- isTRUE(estimate)
  attr(out, "robust")   <- isTRUE(robust)
  class(out) <- c("badp_best_models", "list")

  out
}

#' Print Best Models Tables
#'
#' Print method for objects of class \code{badp_best_models} returned by
#' \code{\link{best_models}}. Draws the table chosen by the \code{estimate}
#' and \code{robust} arguments of the original \code{\link{best_models}}
#' call to the active graphics device.
#'
#' Because R only auto-prints expressions evaluated at the top level, calling
#' \code{best_models(bma_list)} without assignment triggers this method (and
#' hence the table is drawn), while \code{best <- best_models(bma_list)}
#' stays silent.
#'
#' @param x An object of class \code{badp_best_models}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @seealso \code{\link{best_models}}
#'
#' @export
print.badp_best_models <- function(x, ...) {
  estimate <- isTRUE(attr(x, "estimate"))
  robust   <- isTRUE(attr(x, "robust"))

  if (!estimate) {
    gridExtra::grid.table(x[[1]])         # inclusion_table
  } else if (robust) {
    gridExtra::grid.table(x[[3]])         # models_stdR (robust SE)
  } else {
    gridExtra::grid.table(x[[2]])         # models_std (regular SE)
  }

  invisible(x)
}


# Internal helper: tag a captured gTree so that auto-printing it (e.g.
# typing `best[[9]]` at the console) renders the picture via
# `grid::grid.draw()` instead of just printing a description string.
# The original gTree / grob / gDesc classes are preserved, so any code
# that does `grid::grid.draw(best[[9]])` continues to work unchanged.
as_drawable_grob <- function(g) {
  class(g) <- c("badp_drawable_grob", class(g))
  g
}


#' Print a Drawable Best-Models Grob
#'
#' Print method for the captured \code{gTree} objects stored in positions
#' 7-9 of the list returned by \code{\link{best_models}}. Renders the grob
#' to the active graphics device via \code{\link[grid]{grid.draw}} so that
#' typing e.g. \code{best[[9]]} at the console displays the picture
#' instead of a description string.
#'
#' @param x A \code{badp_drawable_grob} object (a \code{gTree} captured by
#'   \code{\link[grid]{grid.grabExpr}}).
#' @param newpage Logical; if \code{TRUE} (default) a new graphics page is
#'   started before drawing.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @seealso \code{\link{best_models}}, \code{\link[grid]{grid.draw}}
#'
#' @export
print.badp_drawable_grob <- function(x, newpage = TRUE, ...) {
  if (isTRUE(newpage)) grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
