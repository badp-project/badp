#' Extract the BMA Statistics Table
#'
#' Returns the table of Bayesian model averaging statistics computed under one
#' of the two model priors, without reaching into the internal structure of the
#' object.
#'
#' The columns are \code{PIP}, the posterior inclusion probability; \code{PM}
#' and \code{PSD}, the posterior mean and posterior standard deviation;
#' \code{PSDR}, the posterior standard deviation built from robust standard
#' errors; \code{PMcon}, \code{PSDcon} and \code{PSDRcon}, the same three
#' quantities conditional on the regressor being included; and \code{\%(+)},
#' the percentage of models in which the coefficient is positive. The first row
#' is the lagged dependent variable, which enters every model by construction.
#'
#' @param x An object of class \code{badp_bma}, typically the result of
#'   \code{\link{bma}}.
#' @param ... Arguments passed to methods.
#'
#' @return A numeric matrix with one row per parameter and eight columns.
#'
#' @seealso \code{\link{bma}}, \code{\link{pip}}, \code{\link{coef.badp_bma}},
#'   \code{\link{summary.badp_bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' bma_table(results)
#' bma_table(results, prior = "beta")
#' }
#'
#' @export
bma_table <- function(x, ...) UseMethod("bma_table")

#' @rdname bma_table
#' @param prior Model prior: \code{"binomial"} (the default) or \code{"beta"}
#'   for the binomial-beta prior.
#' @export
bma_table.badp_bma <- function(x, prior = c("binomial", "beta"), ...) {
  prior <- match.arg(prior)
  if (prior == "binomial") x$uniform_table else x$random_table
}


#' Extract Posterior Inclusion Probabilities
#'
#' Returns the posterior inclusion probability of each regressor.
#'
#' The lagged dependent variable enters every model by construction and
#' therefore carries no inclusion probability. It is omitted unless
#' \code{include_lagged = TRUE}, in which case it appears first with value
#' \code{NA}.
#'
#' @param x An object of class \code{badp_bma}.
#' @param ... Arguments passed to methods.
#'
#' @return A named numeric vector of posterior inclusion probabilities.
#'
#' @seealso \code{\link{bma_table}}, \code{\link{pmp}}, \code{\link{bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' pip(results)
#' sort(pip(results), decreasing = TRUE)
#' }
#'
#' @export
pip <- function(x, ...) UseMethod("pip")

#' @rdname pip
#' @param prior Model prior: \code{"binomial"} (the default) or \code{"beta"}.
#' @param include_lagged Logical. Include the lagged dependent variable, whose
#'   inclusion probability is undefined. Defaults to \code{FALSE}.
#' @export
pip.badp_bma <- function(x, prior = c("binomial", "beta"),
                         include_lagged = FALSE, ...) {
  tab <- bma_table(x, prior = prior)
  out <- tab[, "PIP"]
  names(out) <- rownames(tab)
  if (include_lagged) out else out[-1]
}


#' Extract Posterior Model Probabilities
#'
#' Returns the posterior probability of each model in the model space.
#'
#' @param x An object of class \code{badp_bma}.
#' @param ... Arguments passed to methods.
#'
#' @return A numeric vector of posterior model probabilities, one per model,
#'   in the order in which the models are held in the model space unless
#'   \code{top} is given.
#'
#' @seealso \code{\link{pip}}, \code{\link{best_models}},
#'   \code{\link{model_pmp}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' sum(pmp(results))        # sums to one
#' pmp(results, top = 5)    # the five most probable models
#' }
#'
#' @export
pmp <- function(x, ...) UseMethod("pmp")

#' @rdname pmp
#' @param prior Model prior: \code{"binomial"} (the default) or \code{"beta"}.
#' @param top Optional number of models to return, ordered by decreasing
#'   posterior probability. By default every model is returned, unsorted.
#' @export
pmp.badp_bma <- function(x, prior = c("binomial", "beta"), top = NULL, ...) {
  prior <- match.arg(prior)
  column <- if (prior == "binomial") x$R + 1 else x$R + 2
  out <- as.numeric(x$PMPs[, column])

  if (is.null(top)) return(out)

  if (top > length(out)) {
    message("top cannot exceed the size of the model space. Setting top = ",
            length(out), " and continuing.")
    top <- length(out)
  }
  sort(out, decreasing = TRUE)[seq_len(top)]
}


#' Extract the Prior and Posterior Model Sizes
#'
#' Returns the table of prior and posterior expected model sizes under the
#' binomial and binomial-beta model priors. Model size counts regressors and
#' excludes the lagged dependent variable, which is present in every model.
#'
#' @param x An object of class \code{badp_bma}.
#' @param ... Arguments passed to methods.
#'
#' @return A numeric matrix with two rows, one per model prior, and two
#'   columns holding the prior and the posterior expected model size.
#'
#' @seealso \code{\link{bma}}, \code{\link{model_sizes}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' model_size_table(results)
#' }
#'
#' @export
model_size_table <- function(x, ...) UseMethod("model_size_table")

#' @rdname model_size_table
#' @export
model_size_table.badp_bma <- function(x, ...) x$PMS_table


#' Names of the Regressors
#'
#' Returns the names of the regressors considered in the analysis.
#'
#' @param x An object of class \code{badp_bma} or \code{badp_model_space}.
#' @param ... Arguments passed to methods.
#'
#' @return A character vector of regressor names.
#'
#' @seealso \code{\link{bma}}, \code{\link{optim_model_space}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' regressors(full_model_space)
#' regressors(full_model_space, include_lagged = TRUE)
#' }
#'
#' @export
regressors <- function(x, ...) UseMethod("regressors")

#' @rdname regressors
#' @param include_lagged Logical. Include the lagged dependent variable, which
#'   is the first name and enters every model. Defaults to \code{FALSE}.
#' @export
regressors.badp_bma <- function(x, include_lagged = FALSE, ...) {
  out <- as.character(x$reg_names)
  if (include_lagged) out else out[-1]
}

#' @rdname regressors
#' @export
regressors.badp_model_space <- function(x, include_lagged = FALSE, ...) {
  out <- as.character(x$reg_names)
  if (include_lagged) out else out[-1]
}


#' Size of the Model Space
#'
#' Returns the number of models over which the averaging is performed. The
#' lagged dependent variable enters every model and is never averaged over, so
#' the count is two to the power of \code{length(regressors(x))}, not of the
#' total number of columns.
#'
#' @param x An object of class \code{badp_bma} or \code{badp_model_space}.
#' @param ... Arguments passed to methods.
#'
#' @return A single integer.
#'
#' @seealso \code{\link{bma}}, \code{\link{optim_model_space}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' n_models(full_model_space)
#' }
#'
#' @export
n_models <- function(x, ...) UseMethod("n_models")

#' @rdname n_models
#' @export
n_models.badp_bma <- function(x, ...) as.integer(x$num_of_models)

#' @rdname n_models
#' @export
n_models.badp_model_space <- function(x, ...) ncol(x$params)


#' Marginal-Likelihood Weighting Used
#'
#' Returns the approximation to the marginal likelihood used to weight the
#' models, and the learning rate it implies.
#'
#' The four approximations are described in \code{\link{bma}}. Three of them
#' share the same construction at different learning rates, so the rate is the
#' quantity that distinguishes them; the fourth, \code{"uip"}, alters the
#' penalty instead and has no learning rate.
#'
#' @param x An object of class \code{badp_bma}.
#' @param ... Arguments passed to methods.
#'
#' @return For \code{weighting}, a character string. For
#'   \code{learning_rate}, a single number, or \code{NA} when the weighting
#'   does not correspond to a learning rate.
#'
#' @seealso \code{\link{bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' weighting(results)
#' learning_rate(results)
#' }
#'
#' @export
weighting <- function(x, ...) UseMethod("weighting")

#' @rdname weighting
#' @export
weighting.badp_bma <- function(x, ...) x$weighting

#' @rdname weighting
#' @export
learning_rate <- function(x, ...) UseMethod("learning_rate")

#' @rdname weighting
#' @export
learning_rate.badp_bma <- function(x, ...) x$eta


#' Per-Model Convergence Diagnostics
#'
#' Returns the diagnostics recorded for the numerical optimization of every
#' model in the model space: whether it converged, the code returned by
#' \code{\link[stats]{optim}}, the number of restarts and of initial draws, and
#' the largest absolute gradient at the reported optimum.
#'
#' Models that failed to converge are retained in the model space rather than
#' dropped, so that they can be inspected. See \code{\link{optim_model_space}}
#' for what the individual diagnostics mean.
#'
#' @param x An object of class \code{badp_model_space}.
#' @param ... Arguments passed to methods.
#'
#' @return A numeric matrix with one column per model.
#'
#' @seealso \code{\link{optim_model_space}},
#'   \code{\link{summary.badp_model_space}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' convergence(full_model_space)[, 1:5]
#' }
#'
#' @export
convergence <- function(x, ...) UseMethod("convergence")

#' @rdname convergence
#' @export
convergence.badp_model_space <- function(x, ...) x$convergence


#' Table of the Models in a Model Space
#'
#' Returns one row per model of the model space, describing which regressors
#' the model includes and how well it fits. This is the direct way to rank or
#' filter the estimated models, for example by their maximized log-likelihood,
#' without reaching into the parameter and statistics matrices.
#'
#' A regressor counts as included in a model when its coefficient
#' \code{beta_<name>} was estimated in that model. The lagged dependent
#' variable enters every model and is not counted in \code{size}.
#'
#' The log-likelihood is the value maximized by \code{\link{optim_model_space}}.
#' Unless the model space was estimated with \code{exact_value = TRUE}, it
#' omits a constant common to all models, so that differences between models
#' are exact while the level is not.
#'
#' @param x An object of class \code{badp_model_space}.
#' @param sort_by Ordering of the rows: \code{"none"} (the default) keeps the
#'   order in which the models are held in the model space, \code{"loglik"}
#'   sorts by decreasing log-likelihood, and \code{"size"} by increasing number
#'   of regressors, ties broken by decreasing log-likelihood.
#' @param top Optional number of rows to return, taken after sorting.
#' @param ... Arguments passed to methods.
#'
#' @return A data frame with one row per model and columns
#' \describe{
#'   \item{\code{model}}{Position of the model in the model space, the index
#'     of its column in the parameter matrix.}
#'   \item{\code{size}}{Number of regressors included.}
#'   \item{\code{regressors}}{Names of the included regressors, separated by
#'     commas, or \code{"(none)"} for the model with the lagged dependent
#'     variable only.}
#'   \item{\code{loglik}}{Maximized log-likelihood.}
#'   \item{\code{converged}}{Logical, whether the optimization converged. Only
#'     present when the model space records convergence diagnostics; see
#'     \code{\link{convergence}}.}
#' }
#'
#' @seealso \code{\link{optim_model_space}}, \code{\link{convergence}},
#'   \code{\link{regressors}}, \code{\link{n_models}}
#'
#' @examples
#' data(small_model_space)
#'
#' model_table(small_model_space)
#'
#' # the five best-fitting models
#' model_table(small_model_space, sort_by = "loglik", top = 5)
#'
#' @export
model_table <- function(x, ...) UseMethod("model_table")

#' @rdname model_table
#' @export
model_table.badp_model_space <- function(x,
                                         sort_by = c("none", "loglik", "size"),
                                         top = NULL, ...) {
  sort_by <- match.arg(sort_by)

  inclusion <- model_inclusion(x)
  reg <- colnames(inclusion)

  out <- data.frame(
    model      = seq_len(nrow(inclusion)),
    size       = as.integer(rowSums(inclusion)),
    regressors = apply(inclusion, 1, function(included) {
      if (any(included)) paste(reg[included], collapse = ", ") else "(none)"
    }),
    loglik     = as.numeric(x$stats[1, ]),
    stringsAsFactors = FALSE
  )

  if (!is.null(x$convergence)) {
    out$converged <- as.numeric(x$convergence["converged", ]) == 1
  }

  ord <- switch(sort_by,
    none   = out$model,
    loglik = order(-out$loglik),
    size   = order(out$size, -out$loglik)
  )
  out <- out[ord, , drop = FALSE]

  if (!is.null(top)) {
    out <- utils::head(out, top)
  }

  rownames(out) <- NULL
  out
}


#' Regressor Inclusion Matrix of a Model Space
#'
#' @param x An object of class \code{badp_model_space}.
#'
#' @return A logical matrix with one row per model and one column per
#'   regressor, \code{TRUE} where the regressor's coefficient was estimated.
#'
#' @keywords internal
#' @noRd
model_inclusion <- function(x) {
  reg <- regressors(x)
  rows <- match(paste0("beta_", reg), rownames(x$params))
  if (anyNA(rows)) {
    stop("The parameter matrix lacks the rows ",
         paste0("beta_", reg[is.na(rows)], collapse = ", "), ".")
  }
  betas <- x$params[rows, , drop = FALSE]
  inclusion <- t(!is.na(betas) & betas != 0)
  dimnames(inclusion) <- list(NULL, reg)
  inclusion
}
