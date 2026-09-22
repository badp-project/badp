#' Best Models by Posterior Model Probability
#'
#' Ranks the models in a fitted model space by their posterior model
#' probability and returns the highest ranked ones as a list of individual
#' models, each carrying its own estimates and diagnostics.
#'
#' The object returned is a list of \code{badp_model} objects, one per
#' selected model, ordered from the highest to the lowest posterior model
#' probability. Individual models are reached by position, for example
#' \code{best_models(bma_results)[[1]]} for the best one, and each has its own
#' \code{print}, \code{summary} and \code{coef} methods. The container has
#' \code{print}, \code{summary} and \code{plot} methods that display the whole
#' selection.
#'
#' @param x An object of class \code{badp_bma}, typically the result of
#'   \code{\link{bma}}.
#' @param prior Model prior used to rank the models: \code{"binomial"} (the
#'   default) or \code{"beta"} for the binomial-beta prior.
#' @param best Number of models to return. If it exceeds the size of the model
#'   space, every model is returned and a message is issued.
#' @param round Number of decimal places used when the estimates are formatted
#'   for display. The stored values are not rounded.
#'
#' @return An object of class \code{badp_best_models}: a list of
#'   \code{badp_model} objects with attributes \code{prior}, \code{n_models}
#'   (the size of the model space), \code{reg_names} and \code{digits}.
#'
#' @seealso \code{\link{bma}}, \code{\link{summary.badp_best_models}},
#'   \code{\link{plot.badp_best_models}}, \code{\link{print.badp_model}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' best <- best_models(results, best = 5)
#' best                      # the selection
#' best[[1]]                 # the single best model
#' coef(best[[1]])           # its coefficients
#' summary(best, robust = TRUE)
#' }
#'
#' @export
best_models <- function(x, prior = "binomial", best = 5, round = 3) {

  prior <- match.arg(prior, c("binomial", "beta"))

  R <- x$R                   # number of regressors
  K <- R + 1                        # regressors plus lagged dependent variable
  reg_names <- as.character(matrix(x$reg_names, nrow = K, ncol = 1))
  M <- x$num_of_models       # size of the model space
  info <- x$best_models_data[, 1:(R + 3 * K)]
  PMP_uniform <- x$best_models_data[, R + 3 * K + 1]
  PMP_random <- x$best_models_data[, R + 3 * K + 2]

  if (best > M) {
    message("best cannot exceed the size of the model space. Setting best = ",
            M, " and continuing.")
    best <- M
  }

  ranking <- if (prior == "binomial") PMP_uniform else PMP_random

  Ranking <- cbind(ranking, info)
  Ranking <- Ranking[order(Ranking[, 1], decreasing = TRUE), , drop = FALSE]

  models <- vector("list", best)

  for (i in seq_len(best)) {
    row <- as.numeric(Ranking[i, ])

    inclusion <- as.logical(row[2:(R + 1)])
    names(inclusion) <- reg_names[-1]

    beta <- row[(R + 2):(R + K + 1)]
    se <- row[(R + K + 2):(R + 2 * K + 1)]
    robust_se <- row[(R + 2 * K + 2):(R + 3 * K + 1)]

    beta[beta == 0] <- NA
    se[se == 0] <- NA
    robust_se[robust_se == 0] <- NA

    names(beta) <- names(se) <- names(robust_se) <- reg_names

    # The estimator is maximum likelihood and the standard errors come from
    # the observed information, so the Wald statistic is referred to the
    # standard normal rather than to a t distribution: there is no exact
    # small-sample result here, and no residual degrees of freedom to count.
    p_value <- 2 * stats::pnorm(abs(beta / se), lower.tail = FALSE)
    p_value_robust <- 2 * stats::pnorm(abs(beta / robust_se),
                                       lower.tail = FALSE)

    models[[i]] <- structure(
      list(
        rank = i,
        prior = prior,
        pmp = row[1],
        inclusion = inclusion,
        included = names(inclusion)[inclusion],
        coefficients = beta,
        se = se,
        robust_se = robust_se,
        p_value = p_value,
        p_value_robust = p_value_robust,
        digits = round
      ),
      class = "badp_model"
    )
  }

  names(models) <- paste0("No. ", seq_len(best))

  structure(
    models,
    class = c("badp_best_models", "list"),
    prior = prior,
    n_models = M,
    reg_names = reg_names,
    digits = round
  )
}


# Significance stars, using the conventional 0.1 / 0.05 / 0.01 breaks.
badp_stars <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.1] <- "*"
  out[!is.na(p) & p < 0.05] <- "**"
  out[!is.na(p) & p < 0.01] <- "***"
  out
}


# "estimate (standard error)stars", blank where the parameter is absent.
badp_format_estimates <- function(beta, se, p, digits) {
  out <- rep(NA_character_, length(beta))
  keep <- !is.na(beta)
  out[keep] <- paste0(round(beta[keep], digits),
                      " (", round(se[keep], digits), ")",
                      badp_stars(p[keep]))
  names(out) <- names(beta)
  out
}


#' Coefficients of a Single Model
#'
#' Extracts the maximum likelihood estimates of one of the best models.
#'
#' @param object An object of class \code{badp_model}, an element of the list
#'   returned by \code{\link{best_models}}.
#' @param se Logical. If \code{TRUE}, a matrix with the estimates, their
#'   standard errors, robust standard errors and the corresponding p-values is
#'   returned instead of the estimates alone. The p-values are Wald p-values
#'   referred to the standard normal distribution.
#' @param include_absent Logical. If \code{FALSE} (the default), parameters
#'   excluded from the model are dropped rather than returned as \code{NA}.
#' @param digits Number of decimal places to round the result to. By default
#'   the values are returned exactly as estimated; supplying \code{digits}
#'   is a convenience for displaying them, equivalent to wrapping the call in
#'   \code{\link{round}}. The \code{print} and \code{summary} methods round
#'   to the number of digits given to \code{\link{best_models}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A named numeric vector, or a matrix when \code{se = TRUE}.
#'
#' @seealso \code{\link{best_models}}, \code{\link{summary.badp_model}}
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 3)
#'
#' coef(best[[1]])
#' coef(best[[1]], se = TRUE)
#' coef(best[[1]], se = TRUE, digits = 3)
#' coef(best[[1]], include_absent = TRUE)
#'
#' @export
coef.badp_model <- function(object, se = FALSE, include_absent = FALSE,
                            digits = NULL, ...) {
  keep <- if (include_absent) rep(TRUE, length(object$coefficients)) else
    !is.na(object$coefficients)

  out <- if (!se) {
    object$coefficients[keep]
  } else {
    cbind(
      Estimate = object$coefficients[keep],
      `Std. Error` = object$se[keep],
      `Robust Std. Error` = object$robust_se[keep],
      `Pr(>|z|)` = object$p_value[keep],
      `Robust Pr(>|z|)` = object$p_value_robust[keep]
    )
  }

  # Rounding is opt-in: the values are returned as estimated unless the caller
  # asks for a display-ready table.
  if (is.null(digits)) out else round(out, digits)
}


#' Print a Single Model
#'
#' Prints the regressors included in one of the best models together with its
#' posterior model probability and its coefficient table.
#'
#' @param x An object of class \code{badp_model}.
#' @param robust Logical. If \code{TRUE}, robust standard errors are shown.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @seealso \code{\link{best_models}}, \code{\link{summary.badp_model}}
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 3)
#'
#' best[[1]]
#' print(best[[1]], robust = TRUE)
#'
#' @export
print.badp_model <- function(x, robust = FALSE, ...) {
  cat("Model No. ", x$rank, " of the ", x$prior, " ranking\n", sep = "")
  cat("Posterior model probability: ", round(x$pmp, x$digits), "\n", sep = "")
  cat("Regressors included: ",
      if (length(x$included)) paste(x$included, collapse = ", ") else "none",
      "\n\n", sep = "")

  keep <- !is.na(x$coefficients)
  std <- if (robust) x$robust_se else x$se
  p <- if (robust) x$p_value_robust else x$p_value

  tab <- data.frame(
    Estimate = round(x$coefficients[keep], x$digits),
    `Std. Error` = round(std[keep], x$digits),
    `Pr(>|z|)` = round(p[keep], x$digits),
    ` ` = badp_stars(p[keep]),
    check.names = FALSE
  )
  print(tab)

  cat("\nStandard errors: ", if (robust) "robust" else "conventional",
      ". Wald p-values, standard normal reference.\n", sep = "")
  cat("Signif. codes: 0.01 '***'  0.05 '**'  0.1 '*'\n")

  invisible(x)
}


#' Summarize a Single Model
#'
#' @param object An object of class \code{badp_model}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{summary.badp_model} holding the rank, the
#'   posterior model probability, the included regressors and the full
#'   coefficient matrix.
#'
#' @seealso \code{\link{best_models}}, \code{\link{print.badp_model}}
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 3)
#'
#' summary(best[[1]])
#'
#' @export
summary.badp_model <- function(object, ...) {
  structure(
    list(
      rank = object$rank,
      prior = object$prior,
      pmp = object$pmp,
      included = object$included,
      n_included = length(object$included),
      digits = object$digits,
      coefficients = coef(object, se = TRUE)
    ),
    class = "summary.badp_model"
  )
}


#' Print the Summary of a Single Model
#'
#' @param x An object of class \code{summary.badp_model}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @seealso \code{\link{summary.badp_model}}
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 3)
#'
#' print(summary(best[[1]]))
#'
#' @export
print.summary.badp_model <- function(x, ...) {
  cat("Model No. ", x$rank, " of the ", x$prior, " ranking\n", sep = "")
  cat("=======================================\n\n")
  cat("Posterior model probability: ", round(x$pmp, x$digits), "\n", sep = "")
  cat("Regressors included (", x$n_included, "): ",
      if (x$n_included) paste(x$included, collapse = ", ") else "none",
      "\n", sep = "")
  cat("\n")
  cat("Coefficients, conventional and robust standard errors",
      " (Wald p-values, standard normal reference):\n", sep = "")
  print(round(x$coefficients, x$digits))
  invisible(x)
}


#' Subset a Best-Models Selection
#'
#' Keeps the class and the attributes of the selection when a subset of the
#' models is taken.
#'
#' @param x An object of class \code{badp_best_models}.
#' @param i Indices of the models to keep.
#'
#' @return An object of class \code{badp_best_models}.
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 5)
#'
#' best[1:2]   # a badp_best_models object holding the two best models
#' best[[1]]   # a single badp_model
#'
#' @export
`[.badp_best_models` <- function(x, i) {
  out <- unclass(x)[i]
  attr(out, "class") <- c("badp_best_models", "list")
  attr(out, "prior") <- attr(x, "prior")
  attr(out, "n_models") <- attr(x, "n_models")
  attr(out, "reg_names") <- attr(x, "reg_names")
  attr(out, "digits") <- attr(x, "digits")
  out
}


# Inclusion matrix: one row per parameter, one column per selected model.
badp_inclusion_table <- function(x) {
  reg_names <- attr(x, "reg_names")
  digits <- attr(x, "digits")

  tab <- vapply(
    x,
    function(m) c(1, as.numeric(m$inclusion), round(m$pmp, digits)),
    numeric(length(reg_names) + 1)
  )
  rownames(tab) <- c(reg_names, "PMP")
  colnames(tab) <- names(x)
  tab
}


# Formatted estimates: one row per parameter, one column per selected model.
badp_estimates_table <- function(x, robust = FALSE) {
  reg_names <- attr(x, "reg_names")
  digits <- attr(x, "digits")

  tab <- vapply(
    x,
    function(m) {
      std <- if (robust) m$robust_se else m$se
      p <- if (robust) m$p_value_robust else m$p_value
      c(badp_format_estimates(m$coefficients, std, p, digits),
        as.character(round(m$pmp, digits)))
    },
    character(length(reg_names) + 1)
  )
  rownames(tab) <- c(reg_names, "PMP")
  colnames(tab) <- names(x)
  tab
}


#' Print a Best-Models Selection
#'
#' Prints, to the console, which regressors enter each of the selected models
#' together with their posterior model probabilities.
#'
#' @param x An object of class \code{badp_best_models}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @seealso \code{\link{best_models}},
#'   \code{\link{summary.badp_best_models}},
#'   \code{\link{plot.badp_best_models}}
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 3)
#'
#' best
#'
#' @export
print.badp_best_models <- function(x, ...) {
  cat("Best ", length(x), " of ", attr(x, "n_models"),
      " models, ranked by the ", attr(x, "prior"),
      " posterior model probability\n\n", sep = "")

  tab <- badp_inclusion_table(x)
  display <- tab
  incl <- seq_len(nrow(tab) - 1)
  display[incl, ] <- ifelse(tab[incl, ] == 1, "x", ".")
  print(noquote(display))

  cat("\n'x' marks an included regressor. Use summary() for the estimates,\n")
  cat("plot() for the same tables as a graphic, and [[i]] for one model.\n")

  invisible(x)
}


#' Summarize a Best-Models Selection
#'
#' @param object An object of class \code{badp_best_models}.
#' @param robust Logical. If \code{TRUE}, robust standard errors are shown in
#'   the table of estimates.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{summary.badp_best_models} holding the
#'   inclusion table, the table of formatted estimates, the posterior model
#'   probabilities and the regressors entering each model.
#'
#' @seealso \code{\link{best_models}}, \code{\link{print.badp_best_models}}
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 3)
#'
#' summary(best)
#' summary(best, robust = TRUE)
#'
#' @export
summary.badp_best_models <- function(object, robust = FALSE, ...) {
  structure(
    list(
      prior = attr(object, "prior"),
      n_models = attr(object, "n_models"),
      n_best = length(object),
      robust = robust,
      pmp = vapply(object, function(m) m$pmp, numeric(1)),
      n_included = vapply(object, function(m) length(m$included), integer(1)),
      inclusion = badp_inclusion_table(object),
      estimates = badp_estimates_table(object, robust = robust)
    ),
    class = "summary.badp_best_models"
  )
}


#' Print the Summary of a Best-Models Selection
#'
#' @param x An object of class \code{summary.badp_best_models}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @seealso \code{\link{summary.badp_best_models}}
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 3)
#'
#' print(summary(best))
#'
#' @export
print.summary.badp_best_models <- function(x, ...) {
  cat("Best Models Summary\n")
  cat("===================\n\n")
  cat("Models shown:            ", x$n_best, " of ", x$n_models, "\n", sep = "")
  cat("Ranking model prior:     ", x$prior, "\n", sep = "")
  cat("Posterior mass covered:  ", round(sum(x$pmp), 4), "\n", sep = "")
  cat("Regressors per model:    ", paste(x$n_included, collapse = ", "),
      "\n\n", sep = "")
  cat("Estimates (", if (x$robust) "robust" else "conventional",
      " standard errors in parentheses):\n", sep = "")
  print(noquote(ifelse(is.na(x$estimates), "", x$estimates)))
  cat("\nSignif. codes: 0.01 '***'  0.05 '**'  0.1 '*'\n")
  invisible(x)
}


#' Plot a Best-Models Selection
#'
#' Draws the inclusion table or the table of estimates for the selected models
#' to the active graphics device.
#'
#' @param x An object of class \code{badp_best_models}.
#' @param which Which table to draw: \code{"estimates"} (the default) or
#'   \code{"inclusion"}.
#' @param robust Logical. If \code{TRUE}, robust standard errors are used in
#'   the table of estimates.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the drawn table.
#'
#' @seealso \code{\link{best_models}}, \code{\link{print.badp_best_models}}
#'
#' @examples
#' data(small_model_space)
#' best <- best_models(bma(small_model_space), best = 3)
#'
#' plot(best)
#' plot(best, which = "inclusion")
#'
#' @export
plot.badp_best_models <- function(x, which = c("estimates", "inclusion"),
                                  robust = FALSE, ...) {
  which <- match.arg(which)

  tab <- if (which == "inclusion") {
    badp_inclusion_table(x)
  } else {
    estimates <- badp_estimates_table(x, robust = robust)
    ifelse(is.na(estimates), "", estimates)
  }

  grid::grid.newpage()
  gridExtra::grid.table(tab)

  invisible(tab)
}


