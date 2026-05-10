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
#' This method is a thin wrapper that delegates to
#' \code{\link{summary.badp_bma}} and then prints the resulting summary, so
#' \code{print(x)} and \code{print(summary(x))} produce identical output.
#' The output displays BMA statistics for both the binomial and binomial-beta
#' priors to allow direct comparison.
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
  print(summary(x, ...))
  invisible(x)
}


#' Summarize Bayesian Model Averaging Results
#'
#' Summary method for objects of class \code{badp_bma}.
#'
#' @param object An object of class \code{badp_bma}, typically the result of \code{\link{bma}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{summary.badp_bma} containing:
#' \itemize{
#'   \item \code{model_space_size} - Total number of models in the model space
#'   \item \code{num_regressors} - Number of regressors (excluding lagged dependent variable)
#'   \item \code{expected_model_size} - Expected model size
#'   \item \code{dilution_applied} - Logical indicating if dilution prior was used
#'   \item \code{dilution_param} - Numeric value of the dilution parameter (\code{dilution_par}); only relevant when \code{dilution_applied} is \code{TRUE}
#'   \item \code{results_binomial} - Coefficient table for binomial prior
#'   \item \code{results_beta} - Coefficient table for binomial-beta prior
#'   \item \code{model_sizes} - Prior and posterior model sizes table
#'   \item \code{reg_names} - Variable names
#' }
#'
#' @details
#' This function creates a comprehensive summary object that includes model space information,
#' BMA statistics for both priors, and highlights variables with high posterior inclusion
#' probabilities. The summary always displays results for both the binomial and binomial-beta
#' priors to allow direct comparison.
#'
#' @seealso \code{\link{bma}}, \code{\link{print.badp_bma}}, \code{\link{coef.badp_bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#' summary(results)
#' }
#'
#' @export
summary.badp_bma <- function(object, ...) {
  # Create summary object
  summary_obj <- structure(
    list(
      model_space_size = object$num_of_models,
      num_regressors = object$R,
      expected_model_size = object$EMS,
      dilution_applied = object$dilution == 1,
      dilution_param = object$dilution_par,
      results_binomial = object$uniform_table,
      results_beta = object$random_table,
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
  if (isTRUE(x$dilution_applied) && !is.null(x$dilution_param)) {
    cat("  Dilution parameter (dilution_par):", x$dilution_param, "\n")
  }
  cat("  Model prior: binomial, binomial-beta\n\n")

  cat("BMA statistics (binomial prior):\n")
  cat(strrep("-", 40), "\n", sep = "")
  print(x$results_binomial)
  cat("\n")

  cat("BMA statistics (binomial-beta prior):\n")
  cat(strrep("-", 40), "\n", sep = "")
  print(x$results_beta)
  cat("\n")

  cat("Prior and Posterior Model Sizes:\n")
  cat(strrep("-", 40), "\n", sep = "")
  print(x$model_sizes)
  cat("\n")

  # Identify important variables (high PIP)
  if ("PIP" %in% colnames(x$results_binomial)) {
    pips <- x$results_binomial[, "PIP"]
    high_pip <- which(!is.na(pips) & pips > 0.5)
    if (length(high_pip) > 0) {
      cat("Variables with PIP > 0.5:\n")
      cat("  ", paste(x$reg_names[high_pip], collapse = ", "), "\n\n")
    }
  }

  invisible(x)
}


#' Extract posterior statistics from Bayesian Model Averaging Results
#'
#' Coefficient extraction method for objects of class \code{badp_bma}.
#'
#' @param object An object of class \code{badp_bma}, typically the result of \code{\link{bma}}.
#' @param prior Character string specifying which prior to use. Options are
#'   \code{"both"} (default), \code{"binomial"}, or \code{"beta"}. With
#'   \code{"both"} the result reports estimates under the binomial and the
#'   binomial-beta priors and is printed via
#'   \code{\link{print.badp_bma_coef}}.
#' @param conditional Logical. If \code{TRUE}, returns posterior means (and
#'   posterior standard deviations when \code{se = TRUE}) \emph{conditional on inclusion}
#'   of the variable in a model - i.e. the columns whose names end in
#'   \code{"con"} (\code{PMcon}, \code{PSDcon}, \code{PSDRcon}). If
#'   \code{FALSE} (default), returns the unconditional posterior mean and
#'   standard error.
#' @param se Logical. If \code{TRUE}, includes a posterior standard deviation column
#'   alongside each posterior mean. Defaults to \code{FALSE}.
#' @param robustSE Logical. Only meaningful when \code{se = TRUE}. If
#'   \code{TRUE}, uses the robust posterior standard deviation
#'   (\code{PSDR} / \code{PSDRcon}); if \code{FALSE} (default), uses the
#'   non-robust version (\code{PSD} / \code{PSDcon}). Ignored with a
#'   warning when \code{se = FALSE}.
#' @param PIP Logical. If \code{TRUE} (default), includes a posterior
#'   inclusion probability column for each prior. Set to \code{FALSE} to
#'   suppress.
#' @param ... Additional arguments (currently unused).
#'
#' @return The shape of the return value depends on \code{prior},
#'   \code{conditional}, \code{se}, \code{robustSE} and \code{PIP}:
#'   \itemize{
#'     \item \code{prior = "both"}: always a \code{badp_bma_coef} data frame.
#'       Columns are grouped by prior; for each prior the columns
#'       \code{binom_PM} / \code{beta_PM} (or \code{binom_PMcon} / \code{beta_PMcon}
#'       when \code{conditional = TRUE}), standard error columns
#'       \code{binom_PSD} / \code{beta_PSD} (or \code{binom_PSDR} / \code{beta_PSDR}
#'       when \code{robustSE = TRUE}, or their \code{con} variants when
#'       \code{conditional = TRUE}) when \code{se = TRUE}, and
#'       \code{binom_PIP} / \code{beta_PIP} when \code{PIP = TRUE}.
#'     \item \code{prior = "binomial"} or \code{"beta"}: a named numeric
#'       vector of estimates when \code{se = FALSE} and \code{PIP = FALSE};
#'       otherwise a data frame with columns \code{PM} (or \code{PMcon}
#'       when \code{conditional = TRUE}), posterior standard deviation column
#'       \code{PSD} / \code{PSDR} (or \code{PSDcon} / \code{PSDRcon})
#'       when \code{se = TRUE}, and \code{PIP} when \code{PIP = TRUE}.
#'   }
#'
#' @details
#' This function extracts coefficient estimates from Bayesian Model Averaging
#' results. By default both priors are reported so the user can compare them
#' at a glance; set \code{prior = "binomial"} or \code{prior = "beta"} to
#' obtain the legacy single-prior return values (useful when feeding
#' coefficients into downstream code).
#'
#' @seealso \code{\link{bma}}, \code{\link{summary.badp_bma}},
#'   \code{\link{print.badp_bma_coef}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' # Posterior means under both priors with PIP
#' coef(results)
#'
#' # With standard errors
#' coef(results, se = TRUE)
#'
#' # With robust standard errors
#' coef(results, se = TRUE, robustSE = TRUE)
#'
#' # Conditional posterior means and SEs
#' coef(results, conditional = TRUE, se = TRUE)
#'
#' # Suppress PIP column
#' coef(results, PIP = FALSE)
#'
#' # Single-prior numeric vector (legacy behaviour)
#' coef(results, prior = "binomial", PIP = FALSE)
#' }
#'
#' @export
coef.badp_bma <- function(object,
                          prior       = "both",
                          conditional = FALSE,
                          se          = FALSE,
                          robustSE    = FALSE,
                          PIP         = TRUE,
                          ...) {
  # Validate arguments
  prior <- match.arg(prior, c("both", "binomial", "beta"))

  if (!is.logical(conditional) || length(conditional) != 1L) {
    stop("`conditional` must be a single logical value.")
  }
  if (!is.logical(se) || length(se) != 1L) {
    stop("`se` must be a single logical value.")
  }
  if (!is.logical(robustSE) || length(robustSE) != 1L) {
    stop("`robustSE` must be a single logical value.")
  }
  if (!is.logical(PIP) || length(PIP) != 1L) {
    stop("`PIP` must be a single logical value.")
  }
  if (robustSE && !se) {
    warning("`robustSE = TRUE` has no effect when `se = FALSE`; ignoring.")
  }

  # Resolve column names from the bma tables
  coef_col <- if (conditional) "PMcon" else "PM"
  se_col <- if (conditional) {
    if (robustSE) "PSDRcon" else "PSDcon"
  } else {
    if (robustSE) "PSDR" else "PSD"
  }

  reg <- object$reg_names

  # ---- Single-prior path: preserve legacy return shape when possible ----
  if (prior %in% c("binomial", "beta")) {
    tab <- if (prior == "binomial") object$uniform_table else object$random_table

    coefs <- tab[, coef_col]
    names(coefs) <- reg

    if (!se && !PIP) {
      return(coefs)
    }

    cols <- list()
    cols[[coef_col]] <- unname(coefs)
    if (se)  cols[[se_col]] <- unname(tab[, se_col])
    if (PIP) cols[["PIP"]]  <- unname(tab[, "PIP"])

    return(do.call(data.frame,
                   c(cols, list(row.names = reg, check.names = FALSE))))
  }

  # ---- Both-priors path ----
  bin <- object$uniform_table
  bet <- object$random_table

  cols <- list()
  cols[[paste0("binom_", coef_col)]] <- unname(bin[, coef_col])
  if (se)  cols[[paste0("binom_", se_col)]] <- unname(bin[, se_col])
  if (PIP) cols[["binom_PIP"]]              <- unname(bin[, "PIP"])
  cols[[paste0("beta_", coef_col)]] <- unname(bet[, coef_col])
  if (se)  cols[[paste0("beta_", se_col)]] <- unname(bet[, se_col])
  if (PIP) cols[["beta_PIP"]]              <- unname(bet[, "PIP"])

  out <- do.call(data.frame,
                 c(cols, list(row.names = reg, check.names = FALSE)))

  attr(out, "conditional") <- conditional
  attr(out, "se")          <- se
  attr(out, "robustSE")    <- robustSE
  attr(out, "PIP")         <- PIP
  attr(out, "coef_col")    <- coef_col
  attr(out, "se_col")      <- se_col
  class(out) <- c("badp_bma_coef", "data.frame")
  out
}


#' Print Coefficient Tables from Bayesian Model Averaging
#'
#' Print method for objects of class \code{badp_bma_coef} produced by
#' \code{\link{coef.badp_bma}} when \code{prior = "both"}.
#'
#' @param x An object of class \code{badp_bma_coef}.
#' @param digits Integer. Number of significant digits used when printing
#'   numeric columns. Defaults to \code{4}.
#' @param ... Additional arguments passed to \code{\link[base]{print.data.frame}}.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @details
#' When the result contains only point estimates (\code{se = FALSE},
#' \code{PIP = FALSE}), the two priors are printed side by side. Otherwise
#' the output is split into two stacked panels - one per prior - each with
#' the requested combination of \code{estimate}, \code{std.error} and
#' \code{PIP} columns. The header reflects whether the estimates are
#' unconditional or conditional on inclusion, and whether standard errors
#' are robust.
#'
#' @seealso \code{\link{coef.badp_bma}}
#'
#' @export
print.badp_bma_coef <- function(x, digits = 4, ...) {
  conditional <- isTRUE(attr(x, "conditional"))
  has_se      <- isTRUE(attr(x, "se"))
  has_PIP     <- isTRUE(attr(x, "PIP"))
  robustSE    <- isTRUE(attr(x, "robustSE"))

  # Resolve the bma-table column names for this view (matches what
  # appears in the summary tables).
  coef_col <- attr(x, "coef_col")
  if (is.null(coef_col)) coef_col <- if (conditional) "PMcon" else "PM"
  se_col <- attr(x, "se_col")
  if (is.null(se_col)) {
    se_col <- if (conditional) {
      if (robustSE) "PSDRcon" else "PSDcon"
    } else {
      if (robustSE) "PSDR" else "PSD"
    }
  }

  mean_label <- if (conditional) "Conditional posterior mean" else "Posterior mean"

  # ---- Side-by-side form: estimates only ----
  if (!has_se && !has_PIP) {
    title <- paste0(mean_label, " (both priors)")
    cat(title, "\n", sep = "")
    cat(strrep("-", nchar(title)), "\n", sep = "")
    y <- data.frame(
      binomial        = x[[paste0("binom_", coef_col)]],
      `binomial-beta` = x[[paste0("beta_",  coef_col)]],
      row.names       = rownames(x),
      check.names     = FALSE
    )
    print(y, digits = digits, ...)
    return(invisible(x))
  }

  # ---- Two-panel form ----
  title <- mean_label
  if (has_se) {
    title <- paste0(title,
                    if (robustSE) " with robust std. errors" else " with std. errors")
  }
  if (has_PIP) {
    title <- paste0(title, if (has_se) " and PIP" else " with PIP")
  }
  title <- paste0(title, " (both priors)")
  cat(title, "\n", sep = "")
  cat(strrep("-", nchar(title)), "\n", sep = "")

  build_panel <- function(prefix) {
    cols <- list()
    cols[[coef_col]] <- x[[paste0(prefix, "_", coef_col)]]
    if (has_se)  cols[[se_col]] <- x[[paste0(prefix, "_", se_col)]]
    if (has_PIP) cols[["PIP"]]  <- x[[paste0(prefix, "_PIP")]]
    do.call(data.frame,
            c(cols, list(row.names = rownames(x), check.names = FALSE)))
  }

  cat("Binomial prior:\n")
  print(build_panel("binom"), digits = digits, ...)
  cat("\n")
  cat("Binomial-beta prior:\n")
  print(build_panel("beta"), digits = digits, ...)

  invisible(x)
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
