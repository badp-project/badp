#' Arrange two plots with a common legend and labels
#'
#' Stacks two ggplot objects vertically, prepends the given panel labels to
#' their titles, and collects their legends into a single shared legend at the
#' bottom of the figure.
#'
#' Unlike the previous \code{gridExtra::grid.arrange}-based implementation
#' (which returned a \code{gtable}/\code{TableGrob} that does not auto-render
#' when printed at the console), this version returns a \pkg{patchwork}
#' object that inherits from \code{ggplot} and therefore auto-prints as a
#' rendered plot when you do something like \code{out[[3]]} interactively.
#'
#' @param plot1 First ggplot object
#' @param plot2 Second ggplot object
#' @param labels Character vector of length 2 with panel labels
#' @return A \code{patchwork}/\code{ggplot} object that draws when printed
#' @noRd
arrange_plots_common_legend <- function(plot1, plot2, labels = c("a)", "b)")) {
  # Prepend labels to the plot titles (matching ggpubr::ggarrange style)
  title1 <- plot1$labels$title
  title2 <- plot2$labels$title

  p1 <- plot1 + ggplot2::ggtitle(paste(labels[1], title1))
  p2 <- plot2 + ggplot2::ggtitle(paste(labels[2], title2))

  # Stack vertically, collect both legends into one shared legend at the
  # bottom of the figure.
  patchwork::wrap_plots(p1, p2, ncol = 1, guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}
