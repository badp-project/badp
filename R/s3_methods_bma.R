#' Print Bayesian Model Averaging Results
#'
#' Print method for objects of class \code{badp_bma}.
#'
#' @param x An object of class \code{badp_bma}, typically the result of \code{\link{bma}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @details
#' This function provides a formatted summary of the Bayesian Model Averaging results,
#' including model space information, coefficient estimates under both binomial and
#' binomial-beta priors, and prior/posterior model sizes.
#'
#' @seealso \code{\link{bma}}, \code{\link{summary.badp_bma}}, \code{\link{coef.badp_bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#' print(results)
#' }
#'
#' @export
print.badp_bma <- function(x, ...) {
  cat("Bayesian Model Averaging Results\n")
  cat("=================================\n\n")
  cat("Model Space:", x$num_of_models, "models\n")
  cat("Regressors:", x$R, "\n")
  cat("Expected Model Size:", round(x$EMS, 3), "\n")
  cat("Dilution Prior:", ifelse(x$dilution == 1, "Yes", "No"), "\n\n")

  cat("Binomial Model Prior Results:\n")
  cat("------------------------------\n")
  print(x$uniform_table)
  cat("\n")

  cat("Binomial-Beta Model Prior Results:\n")
  cat("-----------------------------------\n")
  print(x$random_table)
  cat("\n")

  cat("Prior and Posterior Model Sizes:\n")
  cat("---------------------------------\n")
  print(x$PMS_table)
  cat("\n")

  cat("Use summary() for detailed statistics\n")
  cat("Use coef() to extract coefficients\n")
  cat("Use plot() to visualize results\n")

  invisible(x)
}


#' Summarize Bayesian Model Averaging Results
#'
#' Summary method for objects of class \code{badp_bma}.
#'
#' @param object An object of class \code{badp_bma}, typically the result of \code{\link{bma}}.
#' @param prior Character string specifying which prior to use for the summary.
#'   Options are \code{"binomial"} (default) or \code{"beta"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{summary.badp_bma} containing:
#' \itemize{
#'   \item \code{model_space_size} - Total number of models in the model space
#'   \item \code{num_regressors} - Number of regressors (excluding lagged dependent variable)
#'   \item \code{expected_model_size} - Expected model size
#'   \item \code{dilution_applied} - Logical indicating if dilution prior was used
#'   \item \code{prior_type} - Character string indicating prior type
#'   \item \code{results} - Coefficient table for selected prior
#'   \item \code{model_sizes} - Prior and posterior model sizes table
#'   \item \code{reg_names} - Variable names
#' }
#'
#' @details
#' This function creates a comprehensive summary object that includes model space information,
#' coefficient estimates, and highlights variables with high posterior inclusion probabilities.
#'
#' @seealso \code{\link{bma}}, \code{\link{print.badp_bma}}, \code{\link{coef.badp_bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#' summary(results)
#' summary(results, prior = "beta")
#' }
#'
#' @export
summary.badp_bma <- function(object, prior = "binomial", ...) {
  # Validate prior argument
  prior <- match.arg(prior, c("binomial", "beta"))

  # Select appropriate table based on prior
  results_table <- if (prior == "binomial") object$uniform_table else object$random_table

  # Create summary object
  summary_obj <- structure(
    list(
      model_space_size = object$num_of_models,
      num_regressors = object$R,
      expected_model_size = object$EMS,
      dilution_applied = object$dilution == 1,
      prior_type = prior,
      results = results_table,
      model_sizes = object$PMS_table,
      reg_names = object$reg_names
    ),
    class = "summary.badp_bma"
  )

  return(summary_obj)
}


#' Print Summary of Bayesian Model Averaging Results
#'
#' Print method for \code{summary.badp_bma} objects.
#'
#' @param x An object of class \code{summary.badp_bma}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @seealso \code{\link{summary.badp_bma}}
#'
#' @export
print.summary.badp_bma <- function(x, ...) {
  cat("Bayesian Model Averaging Summary\n")
  cat("=================================\n\n")
  cat("Model Space Information:\n")
  cat("  Total models:", x$model_space_size, "\n")
  cat("  Number of regressors:", x$num_regressors, "\n")
  cat("  Expected model size:", round(x$expected_model_size, 3), "\n")
  cat("  Dilution prior:", ifelse(x$dilution_applied, "Applied", "Not applied"), "\n")
  cat("  Model prior:", x$prior_type, "\n\n")

  cat("Coefficient Estimates (", x$prior_type, " prior):\n", sep = "")
  cat(strrep("-", 40), "\n", sep = "")
  print(x$results)
  cat("\n")

  cat("Prior and Posterior Model Sizes:\n")
  cat(strrep("-", 40), "\n", sep = "")
  print(x$model_sizes)
  cat("\n")

  # Identify important variables (high PIP)
  if ("PIP" %in% colnames(x$results)) {
    pips <- x$results[, "PIP"]
    high_pip <- which(!is.na(pips) & pips > 0.5)
    if (length(high_pip) > 0) {
      cat("Variables with PIP > 0.5:\n")
      cat("  ", paste(x$reg_names[high_pip], collapse = ", "), "\n\n")
    }
  }

  invisible(x)
}


#' Extract Coefficients from Bayesian Model Averaging Results
#'
#' Coefficient extraction method for objects of class \code{badp_bma}.
#'
#' @param object An object of class \code{badp_bma}, typically the result of \code{\link{bma}}.
#' @param prior Character string specifying which prior to use.
#'   Options are \code{"binomial"} (default) or \code{"beta"}.
#' @param type Character string specifying the type of coefficient to extract.
#'   Options are:
#'   \itemize{
#'     \item \code{"posterior_mean"} - Posterior mean (default)
#'     \item \code{"conditional_mean"} - Conditional posterior mean
#'   }
#' @param se Logical. If \code{TRUE}, returns a data frame including standard errors
#'   and posterior inclusion probabilities. If \code{FALSE} (default), returns a
#'   named numeric vector of coefficients.
#' @param ... Additional arguments (currently unused).
#'
#' @return If \code{se = FALSE}, a named numeric vector of coefficient estimates.
#'   If \code{se = TRUE}, a data frame with columns:
#'   \itemize{
#'     \item \code{estimate} - Coefficient estimate
#'     \item \code{std.error} - Standard error
#'     \item \code{PIP} - Posterior inclusion probability
#'   }
#'
#' @details
#' This function extracts coefficient estimates from Bayesian Model Averaging results.
#' The posterior mean is the weighted average across all models, while the conditional
#' mean is the average conditional on inclusion in the model.
#'
#' @seealso \code{\link{bma}}, \code{\link{summary.badp_bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' # Extract posterior means
#' coef(results)
#'
#' # Extract with standard errors
#' coef(results, se = TRUE)
#'
#' # Extract conditional means
#' coef(results, type = "conditional_mean", se = TRUE)
#' }
#'
#' @export
coef.badp_bma <- function(object, prior = "binomial", type = "posterior_mean",
                          se = FALSE, ...) {
  # Validate arguments
  prior <- match.arg(prior, c("binomial", "beta"))
  type <- match.arg(type, c("posterior_mean", "conditional_mean"))

  # Select table based on prior
  table <- if (prior == "binomial") object$uniform_table else object$random_table

  # Extract based on type
  coef_col <- switch(type,
    "posterior_mean" = "PM",
    "conditional_mean" = "PMcon"
  )

  coefs <- table[, coef_col]
  names(coefs) <- object$reg_names

  if (se) {
    se_col <- switch(type,
      "posterior_mean" = "PSD",
      "conditional_mean" = "PSDcon"
    )
    ses <- table[, se_col]

    result <- data.frame(
      estimate = coefs,
      std.error = ses,
      PIP = table[, "PIP"],
      row.names = object$reg_names
    )
    return(result)
  }

  return(coefs)
}


#' Plot Bayesian Model Averaging Results
#'
#' Plot method for objects of class \code{badp_bma}.
#'
#' @param x An object of class \code{badp_bma}, typically the result of \code{\link{bma}}.
#' @param which Character string specifying which plot to create. Options are:
#'   \itemize{
#'     \item \code{"model_sizes"} - Model size distributions (default)
#'     \item \code{"best_models"} - Best models
#'     \item \code{"jointness"} - Jointness analysis
#'     \item \code{"coef_hist"} - Coefficient histograms
#'     \item \code{"posterior_dens"} - Posterior densities
#'     \item \code{"model_pmp"} - Model posterior probabilities
#'   }
#' @param ... Additional arguments passed to the underlying plot function.
#'
#' @return The object returned by the selected visualization helper. Depending on
#'   \code{which}, this may be a single plot object or a list containing plots
#'   and/or tables; some helpers may also print output as a side effect.
#'
#' @details
#' This function dispatches to the appropriate visualization function based on the
#' \code{which} parameter. The default plot shows model size distributions, which
#' provides a comprehensive overview of the prior and posterior distributions over
#' model sizes.
#'
#' @seealso \code{\link{bma}}, \code{\link{model_sizes}}, \code{\link{best_models}},
#'   \code{\link{jointness}}, \code{\link{coef_hist}}, \code{\link{posterior_dens}},
#'   \code{\link{model_pmp}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' # Default plot (model sizes)
#' plot(results)
#'
#' # Other plot types
#' plot(results, which = "best_models")
#' plot(results, which = "jointness")
#' }
#'
#' @export
plot.badp_bma <- function(x, which = "model_sizes", ...) {
  which <- match.arg(which, c("model_sizes", "best_models", "jointness",
                               "coef_hist", "posterior_dens", "model_pmp"))

  switch(which,
    "model_sizes" = model_sizes(x, ...),
    "best_models" = best_models(x, ...),
    "jointness" = jointness(x, ...),
    "coef_hist" = coef_hist(x, ...),
    "posterior_dens" = posterior_dens(x, ...),
    "model_pmp" = model_pmp(x, ...)
  )
}
