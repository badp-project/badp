#' Print Model Space Object
#'
#' Print method for objects of class \code{badp_model_space}.
#'
#' @param x An object of class \code{badp_model_space}, typically the result of
#'   \code{\link{compute_model_space_stats}} or \code{\link{optim_model_space}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @details
#' This function provides a formatted summary of the model space object,
#' including the number of models, parameters, variables, and observations.
#' Model space objects are typically used as input to the \code{\link{bma}} function.
#'
#' @seealso \code{\link{compute_model_space_stats}}, \code{\link{optim_model_space}},
#'   \code{\link{bma}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' print(full_model_space)
#' }
#'
#' @export
print.badp_model_space <- function(x, ...) {
  cat("Model Space Object\n")
  cat("==================\n\n")
  cat("Number of models:", ncol(x$params), "\n")
  cat("Number of parameters in full parameter vector:", nrow(x$params), "\n")
  cat("Variables:", paste(x$reg_names, collapse = ", "), "\n")
  cat("Observations:", x$observations_num, "\n")
  cat("Nested:", x$is_nested, "\n\n")

  cat("Use with bma() to perform Bayesian Model Averaging\n")
  invisible(x)
}
