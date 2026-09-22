utils::globalVariables(c("ID", "Value", "Probability", ".data"))

#' Graphs of the Prior and Posterior Probabilities of Individual Models
#'
#' This function draws four graphs of prior and posterior model probabilities for the best individual models: \cr
#' a) The results with binomial model prior (based on PMP - posterior model probability) \cr
#' b) The results with binomial-beta model prior (based on PMP - posterior model probability) \cr
#' Models on the graph are ordered according to their posterior model probability.
#'
#'
#' @param x An object of class \code{badp_bma}, typically returned by \code{\link{bma}}.
#' @param top The number of the best model to be placed on the graphs
#'
#' @param type Character, either \code{"line"} (the default) for the prior and
#'   posterior drawn as lines against the model ranking, or
#'   \code{"histogram"} for them drawn as side-by-side bars. The bar form
#'   suits a small \code{top}, where individual models can still be told
#'   apart; with a large \code{top} the lines are easier to read.
#' @return A list with three graphs with prior and posterior model probabilities for individual models:\cr
#' 1) The results with binomial model prior (based on PMP - posterior model probability) \cr
#' 2) The results with binomial-beta model prior (based on PMP - posterior model probability) \cr
#' 3) One graph combining the aforementioned graphs
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' data_prepared <- badp::economic_growth[, 1:6] %>%
#'   badp::feature_standardization(
#'     excluded_cols = c(country, year, gdp)
#'   ) %>%
#'   badp::feature_standardization(
#'     group_by_col  = year,
#'     excluded_cols = country,
#'     scale         = FALSE
#'   )
#'
#' bma_results <- bma(
#'   model_space = badp::small_model_space,
#'   round       = 3,
#'   dilution    = 0
#' )
#'
#' model_graphs <- model_pmp(bma_results, top = 16)
#'
#' # bars instead of lines
#' model_pmp(bma_results, top = 5, type = "histogram")
#' }
model_pmp <- function(x, top = NULL, type = c("line", "histogram")){

type <- match.arg(type)

# Collecting information from the x
R <- x$R # total number of regressors
M <- x$num_of_models # size of the model space
EMS <- x$EMS # expected model size
PMPs <- x$PMPs[,(R+1):(R+2)] # PMP_uniform, PMP_random
Priors <- x$model_priors # Priors: uniform and random
dilution <- x$dilution # 0 - no dilution prior, 1 - dilution prior

if (is.null(top)){
  top <- R
}

if (top > M) {
  # `top` counts models, so the cap is the size of the model space. It was
  # previously reset to R, the number of regressors, which is unrelated and
  # silently plotted too few models.
  message("top cannot exceed the size of the model space. Setting top = ",
          M, " and continuing.")
  top <- M
}

# Objects to store posteriors and priors
PMP_uniform <- cbind(PMPs[,1], Priors[,1])
PMP_random <- cbind(PMPs[,2], Priors[,2])

# Ordering of the models according to posterior criterion
PMP_uniform <- PMP_uniform[order(PMP_uniform[,1], decreasing=T),]
PMP_random <- PMP_random[order(PMP_random[,1], decreasing=T),]

ranking <- matrix(1:M, nrow = M, ncol = 1)

# Adding a ranking number
PMP_uniform <- cbind(ranking[1:top,], PMP_uniform[1:top,])
PMP_random <- cbind(ranking[1:top,], PMP_random[1:top,])

IDnames <- cbind("ID","Posterior","Prior") # names of the variables to be used by 'tidyverse'

colnames(PMP_uniform) <- IDnames
colnames(PMP_random) <- IDnames

forGraph1 <- as.data.frame(PMP_uniform)
forGraph2 <- as.data.frame(PMP_random)

## Preparation of the Figures with ggplot
forGraph1 <- tidyr::gather(forGraph1, key = "Probability", value = "Value", -ID)

forGraph2 <- tidyr::gather(forGraph2, key = "Probability", value = "Value", -ID)

# One builder for both display types, both priors and both dilution
# settings, so that they cannot drift apart.
pmp_plot <- function(df, title = NULL) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$ID, y = .data$Value))
  if (type == "line") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = .data$Probability,
                                      linetype = .data$Probability)) +
      ggplot2::scale_color_manual(values = c("darkred", "steelblue"))
  } else {
    p <- p +
      ggplot2::geom_col(ggplot2::aes(fill = .data$Probability),
                        position = ggplot2::position_dodge(width = 0.75),
                        width = 0.7) +
      ggplot2::scale_fill_manual(values = c("darkred", "steelblue"))
    # label every rank while there are few enough for that to stay legible
    if (top <= 30) p <- p + ggplot2::scale_x_continuous(breaks = 1:top)
  }
  p <- p +
    ggplot2::ylab("Prior, Posterior") +
    ggplot2::xlab("Model number in the ranking")
  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
  p
}

Graph1 <- pmp_plot(forGraph1)
Graph2 <- pmp_plot(forGraph2)

## Titled versions for the combined graph
dil_label <- if (identical(as.numeric(dilution), 1)) "diluted " else ""
Graph1_2 <- pmp_plot(forGraph1, paste0("Results with ", dil_label,
                                       "binomial model prior (EMS = ", EMS, ")"))
Graph2_2 <- pmp_plot(forGraph2, paste0("Results with ", dil_label,
                                       "binomial-beta model prior (EMS = ", EMS, ")"))

# Putting together the last plot
Finalplot <- arrange_plots_common_legend(Graph1_2, Graph2_2)

out <- list(binomial = Graph1, beta = Graph2, combined = Finalplot)
structure(out, class = "badp_plots", default = "combined")
}
