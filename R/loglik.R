#' Log-Likelihood of an Estimated Model
#'
#' Returns the maximized log-likelihood of a model as an object of class
#' \code{"logLik"}, so that the generic tools of the \pkg{stats} package, such
#' as \code{\link[stats]{AIC}} and \code{\link[stats]{BIC}}, apply to it.
#'
#' The value is the exact log-likelihood of the model evaluated at the
#' estimates, including all constants, whatever \code{exact_value} was used
#' during the optimization. The \code{df} attribute counts every estimated
#' parameter of the model, the regression coefficients as well as the
#' parameters of the covariance structure, and \code{nobs} is the number of
#' observations used in the estimation.
#'
#' Note that \code{\link[stats]{BIC}} applied to these objects is the
#' textbook criterion, which penalizes all \code{df} parameters. The
#' approximation to the marginal likelihood that \code{\link{bma}} uses to
#' weight the models penalizes only the regression coefficients, so the two
#' need not rank the models identically.
#'
#' @param object An object of class \code{badp_model_space}, or a single model
#'   of class \code{badp_model} as returned within \code{\link{best_models}}.
#' @param model For a model space, the position of the model, as reported in
#'   the \code{model} column of \code{\link{model_table}}. A single integer.
#' @param ... Further arguments, currently unused.
#'
#' @return An object of class \code{"logLik"}: a number with attributes
#'   \code{df} and \code{nobs}.
#'
#' @seealso \code{\link{model_table}}, \code{\link{best_models}},
#'   \code{\link[stats]{logLik}}
#'
#' @examples
#' data(small_model_space)
#'
#' # the model with all regressors
#' ll <- logLik(small_model_space, model = n_models(small_model_space))
#' ll
#' AIC(ll)
#' BIC(ll)
#'
#' \donttest{
#' results <- bma(small_model_space)
#' best <- best_models(results, best = 2)
#' logLik(best[[1]])
#' }
#'
#' @name logLik.badp
#' @export
logLik.badp_model_space <- function(object, model, ...) {
  if (missing(model)) {
    stop("Specify the model by its position in the model space, for example ",
         "logLik(x, model = 1); model_table(x) lists the positions.")
  }
  model <- check_model_index(model, ncol(object$params))

  structure(
    as.numeric(object$stats[1L, model]),
    df = sum(!is.na(object$params[, model])),
    nobs = as.integer(object$observations_num),
    class = "logLik"
  )
}

#' @rdname logLik.badp
#' @export
logLik.badp_model <- function(object, ...) {
  if (is.null(object$loglik) || is.na(object$loglik)) {
    stop("This model carries no log-likelihood. It was created from a ",
         "badp_bma object computed before log-likelihoods were stored; recompute it with bma().")
  }
  structure(
    as.numeric(object$loglik),
    df = as.integer(object$n_params),
    nobs = as.integer(object$nobs),
    class = "logLik"
  )
}
