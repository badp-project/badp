#' Arrange two plots with a common legend and labels
#'
#' @param plot1 First ggplot object
#' @param plot2 Second ggplot object
#' @param labels Character vector of length 2 with panel labels
#' @return A gtable object (invisibly) produced by gridExtra::grid.arrange
#' @noRd
arrange_plots_common_legend <- function(plot1, plot2, labels = c("a)", "b)")) {
  # Prepend labels to the plot titles (matching ggpubr::ggarrange style)
  title1 <- plot1$labels$title
  title2 <- plot2$labels$title
  p1 <- plot1 +
    ggplot2::ggtitle(paste(labels[1], title1)) +
    ggplot2::theme(legend.position = "none")
  p2 <- plot2 +
    ggplot2::ggtitle(paste(labels[2], title2)) +
    ggplot2::theme(legend.position = "none")

  # Extract legend as horizontal bar
  plot_for_legend <- plot1 +
    ggplot2::theme(legend.position = "bottom")
  g1 <- ggplot2::ggplotGrob(plot_for_legend)
  grob_names <- vapply(g1$grobs, function(x) x$name, character(1))
  legend_idx <- which(grepl("^guide-box", grob_names))
  legend_grob <- g1$grobs[[legend_idx[1]]]

  gridExtra::grid.arrange(
    p1,
    p2,
    legend_grob,
    ncol = 1,
    heights = grid::unit(c(5, 5, 0.8), "null")
  )
}
