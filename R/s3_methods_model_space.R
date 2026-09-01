#' Print Model Space Object
#'
#' Print method for objects of class \code{badp_model_space}.
#'
#' @param x An object of class \code{badp_model_space}, typically the result of
#'   \code{\link{optim_model_space}}.
#' @param ... Additional arguments forwarded to \code{\link{summary.badp_model_space}}.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @details
#' This method is a thin wrapper that delegates to
#' \code{\link{summary.badp_model_space}} and then prints the resulting
#' summary, so \code{print(x)} and \code{print(summary(x))} produce
#' identical output.
#'
#' @seealso \code{\link{summary.badp_model_space}},
#'   \code{\link{optim_model_space}}, \code{\link{bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' print(full_model_space)
#' }
#'
#' @export
print.badp_model_space <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
}


#' Summarize a Model Space Object
#'
#' Summary method for objects of class \code{badp_model_space}. Replaces
#' the default \code{summary()} output (which just lists the structure of
#' the underlying list) with a structured object describing the dimensions
#' of the model space, its variables, and a brief look at the per-model
#' log-likelihoods stored in \code{object$stats}.
#'
#' @param object An object of class \code{badp_model_space}, typically the
#'   result of \code{\link{optim_model_space}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{summary.badp_model_space} containing:
#' \itemize{
#'   \item \code{num_models} - Number of models in the model space (\eqn{2^R}).
#'   \item \code{num_regressors} - Number of regressors excluding the lagged
#'     dependent variable (\eqn{R}).
#'   \item \code{num_params} - Number of parameters in the full parameter
#'     vector (rows of \code{object$params}).
#'   \item \code{observations_num} - Number of observations used.
#'   \item \code{is_nested} - Logical, whether the model space is nested.
#'   \item \code{reg_names} - All variable names (lagged dependent first).
#'   \item \code{dep_var_name} - The (lagged) dependent variable name.
#'   \item \code{regressor_names} - The regressor names.
#'   \item \code{data_dim} - Dimensions of the source data frame, or
#'     \code{NULL} if not stored.
#'   \item \code{likelihoods} - Per-model log-likelihood values (row 1 of
#'     \code{object$stats}), or \code{NULL} if not available.
#'   \item \code{num_nonconverged} - Number of models whose optimization did
#'     not converge, or \code{NULL} if the model space carries no convergence
#'     diagnostics (e.g. objects created before badp 0.6.0).
#' }
#'
#' @seealso \code{\link{print.badp_model_space}},
#'   \code{\link{print.summary.badp_model_space}},
#'   \code{\link{optim_model_space}}, \code{\link{bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' summary(full_model_space)
#' }
#'
#' @export
summary.badp_model_space <- function(object, ...) {
  num_models <- ncol(object$params)
  num_params <- nrow(object$params)
  reg <- object$reg_names
  R  <- length(reg) - 1L

  likelihoods <- if (!is.null(object$stats) && nrow(object$stats) >= 1L) {
    object$stats[1L, ]
  } else {
    NULL
  }

  data_dim <- if (!is.null(object$df)) dim(object$df) else NULL

  num_nonconverged <- if (!is.null(object$convergence)) {
    sum(object$convergence["converged", ] == 0)
  } else {
    NULL
  }

  # Fraction of parameter directions spanned by the outer product of the
  # entity-level scores, in the worst model. Below one means the robust
  # standard errors rest on a rank-deficient J; see ?score_rank.
  K <- length(reg)
  score_span <- if (!is.null(object$stats) &&
                    nrow(object$stats) >= 5L + 2L * K) {
    min(object$stats[5L + 2L * K, ] / object$stats[4L + 2L * K, ])
  } else {
    NULL
  }

  result <- list(
    num_models       = num_models,
    num_regressors   = R,
    num_params       = num_params,
    observations_num = object$observations_num,
    is_nested        = object$is_nested,
    reg_names        = reg,
    dep_var_name     = reg[1L],
    regressor_names  = reg[-1L],
    data_dim         = data_dim,
    likelihoods      = likelihoods,
    num_nonconverged = num_nonconverged,
    score_span       = score_span
  )
  class(result) <- "summary.badp_model_space"
  result
}


#' Print Summary of a Model Space Object
#'
#' Print method for \code{summary.badp_model_space} objects.
#'
#' @param x An object of class \code{summary.badp_model_space}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @seealso \code{\link{summary.badp_model_space}}
#'
#' @export
print.summary.badp_model_space <- function(x, ...) {
  cat("Model Space Summary\n")
  cat("===================\n\n")

  cat("Dimensions:\n")
  cat("  Number of models:        ", x$num_models,
      " (2^", x$num_regressors, ")\n", sep = "")
  cat("  Number of regressors:    ", x$num_regressors,
      "  (excluding lagged dependent variable)\n", sep = "")
  cat("  Parameters per model:    ", x$num_params, "\n", sep = "")
  cat("  Observations used:       ", x$observations_num, "\n", sep = "")
  if (!is.null(x$data_dim)) {
    cat("  Source data:             ", x$data_dim[1L], " rows x ",
        x$data_dim[2L], " columns\n", sep = "")
  }
  cat("\n")

  cat("Variables:\n")
  cat("  Dependent (with lag):    ", x$dep_var_name, "\n", sep = "")
  cat("  Regressors:              ",
      paste(x$regressor_names, collapse = ", "), "\n", sep = "")
  cat("\n")

  cat("Structure:\n")
  cat("  Nested model space:      ",
      if (isTRUE(x$is_nested)) "yes" else "no", "\n", sep = "")
  cat("\n")

  if (!is.null(x$num_nonconverged)) {
    cat("Optimization:\n")
    if (x$num_nonconverged > 0) {
      cat("  Non-converged models:    ", x$num_nonconverged,
          "  (see the 'convergence' element)\n", sep = "")
    } else {
      cat("  All models converged.\n")
    }
    cat("\n")
  }

  if (!is.null(x$score_span) && is.finite(x$score_span)) {
    cat("Robust standard errors:\n")
    cat(sprintf("  Score directions spanned: %.0f%% of parameters%s\n",
                100 * x$score_span,
                if (x$score_span < 1) "  (see ?score_rank)" else ""))
    cat("\n")
  }

  if (!is.null(x$likelihoods) && length(x$likelihoods) > 0L) {
    finite_lik <- x$likelihoods[is.finite(x$likelihoods)]
    if (length(finite_lik) > 0L) {
      cat("Per-model log-likelihood:\n")
      cat(sprintf("  Min: %.3f   Median: %.3f   Max: %.3f\n",
                  min(finite_lik),
                  stats::median(finite_lik),
                  max(finite_lik)))
      cat("\n")
    }
  }

  cat("Use with bma() to perform Bayesian Model Averaging.\n")
  invisible(x)
}
