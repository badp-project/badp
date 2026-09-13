#' Print a Collection of Plots
#'
#' Print method for the plot collections returned by \code{\link{model_sizes}},
#' \code{\link{model_pmp}}, \code{\link{coef_hist}} and
#' \code{\link{posterior_dens}}.
#'
#' A bare list of \pkg{ggplot2} objects is auto-printed one element at a time,
#' so on an interactive device each plot replaces the one before it and only
#' the last remains visible. These functions therefore return a classed
#' collection, and this method draws a single figure: the combined plot when
#' the collection provides one, and otherwise every plot arranged together with
#' \code{\link[patchwork]{wrap_plots}}. Individual plots are still reached by
#' name or position and printed on their own.
#'
#' @param x An object of class \code{badp_plots}.
#' @param which Optional name or position of a single plot to draw instead of
#'   the default figure. A name must be one of \code{names(x)} and a position
#'   an index between 1 and \code{length(x)}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @seealso \code{\link{model_sizes}}, \code{\link{model_pmp}},
#'   \code{\link{coef_hist}}, \code{\link{posterior_dens}}
#'
#' @examples
#' \donttest{
#' data(full_model_space)
#' results <- bma(full_model_space)
#'
#' sizes <- model_sizes(results)
#' sizes                       # one combined figure
#' print(sizes, which = "binomial")
#' names(sizes)
#' }
#'
#' @export
print.badp_plots <- function(x, which = NULL, ...) {
  plots <- unclass(x)
  target <- if (!is.null(which)) which else attr(x, "default")

  if (!is.null(target)) {
    # `[[` would reject a bad subscript anyway, but with a message about
    # subscripts rather than about the panels this object actually holds.
    bad_which <- function() {
      stop("'which' must be a single plot name (one of: ",
           paste(names(plots), collapse = ", "),
           ") or an index between 1 and ", length(plots), ".", call. = FALSE)
    }
    if (length(target) != 1L || is.na(target)) {
      bad_which()
    } else if (is.character(target)) {
      if (!target %in% names(plots)) bad_which()
    } else if (is.numeric(target)) {
      if (target %% 1 != 0 || target < 1 || target > length(plots)) bad_which()
    } else {
      bad_which()
    }
    print(plots[[target]])
    return(invisible(x))
  }

  print(patchwork::wrap_plots(plots))
  invisible(x)
}
