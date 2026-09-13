utils::globalVariables(c("ID", "Value", "Probability", ".data"))

#' Graphs of the Prior and Posterior Probabilities of Model Sizes
#'
#' This function draws two graphs of prior and posterior model probabilities: \cr
#' a) The results with binomial model prior \cr
#' b) The results with binomial-beta model prior \cr
#' c) One graph combining all the aforementioned graphs
#'
#' @param x An object of class \code{badp_bma}, typically returned by \code{\link{bma}}.
#'
#' @param type Character, either \code{"line"} (the default) for the prior and
#'   posterior drawn as lines against model size, or \code{"histogram"} for
#'   them drawn as side-by-side bars. Bars read well when there are few model
#'   sizes; with many regressors the line form stays legible where the bars
#'   become crowded.
#' @return A list with three graphs with prior and posterior model probabilities for model sizes:\cr
#' 1) The results with binomial model prior \cr
#' 2) The results with binomial-beta model prior  \cr
#' 3) One graph combining all the aforementioned graphs
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
#' size_graphs <- model_sizes(bma_results)
#'
#' # bars instead of lines
#' model_sizes(bma_results, type = "histogram")
#' }
model_sizes <- function(x, type = c("line", "histogram")){

  type <- match.arg(type)

  R <- x$R # total number of regressors
  M <- x$num_of_models # size of the model space
  EMS <- x$EMS # expected model size
  sizePriors <- x$size_priors # table with uniform and random model priors spread over model sizes
  modelPosterior <- x$PMPs # table with posterior model probabilities
  dilution <- x$dilution # 0 - no dilution prior, 1 - dilution prior

  reg_ID <- modelPosterior[,1:R]
  uniform_posterior <- matrix(modelPosterior[,R+1], nrow = M, ncol = 1)
  random_posterior <- matrix(modelPosterior[,R+2], nrow = M, ncol = 1)

  sizes <- matrix(0, nrow = R+1, ncol = 1) # vector to store number of models in a given model size

  for (k in 0:R){
    sizes[k+1,1] <- choose(R,k) # number of models of the size k out of R regressors
  }

  ind <- matrix(cumsum(sizes), nrow = R+1, ncol = 1) # we create a vector with the number of models in each model size category

  for_sizes <- cbind(rowSums(reg_ID), reg_ID, uniform_posterior, random_posterior)
  for_sizes <- for_sizes[order(for_sizes[,1]), ]
  model_posterior <- matrix(for_sizes[,(R+2):(R+3)], nrow = M, ncol = 2)

  Posterior_sizes <- matrix(0, nrow = R+1, ncol = 2) # matrix to store posterior probabilities over model sizes

  for (i in 1:(R+1)){
    if (i==1){Posterior_sizes[i,1]=model_posterior[1,1]
    Posterior_sizes[i,2]=model_posterior[1,2]} # we collect probabilities for different model sizes: the case of the model with no regressors
    else{Posterior_sizes[i,1]=sum(model_posterior[(ind[i-1]+1):ind[i],1])
    Posterior_sizes[i,2]=sum(model_posterior[(ind[i-1]+1):ind[i],2])
    } # we collect probabilities for different model sizes: the case of models with regressors
  }

  # Preparation of the tables for graphs
  forGraph1 <- cbind(0:R, sizePriors[,1], Posterior_sizes[,1])
  forGraph2 <- cbind(0:R, sizePriors[,2], Posterior_sizes[,2])

  IDnames <- cbind("ID", "Prior", "Posterior") # names of the variables to be used by 'tidyverse'

  colnames(forGraph1) <- IDnames
  colnames(forGraph2) <- IDnames

  forGraph1 <- as.data.frame(forGraph1)
  forGraph2 <- as.data.frame(forGraph2)

  ## Preparation of the Figures with ggplot
  forGraph1 <- tidyr::gather(forGraph1, key = "Probability", value = "Value", -ID)
  forGraph2 <- tidyr::gather(forGraph2, key = "Probability", value = "Value", -ID)

  ## One builder for both display types, both priors and both dilution
  ## settings, so that they cannot drift apart.
  size_plot <- function(df, title = NULL) {
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
        ggplot2::scale_fill_manual(values = c("darkred", "steelblue")) +
        # bars sit at integer model sizes, so label every one of them
        ggplot2::scale_x_continuous(breaks = 0:R)
    }
    p <- p +
      ggplot2::ylab("Prior, Posterior") +
      ggplot2::xlab("Model size (number of regressors)")
    if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
    p
  }

  Graph1 <- size_plot(forGraph1)
  Graph2 <- size_plot(forGraph2)

  ## Titled versions for the combined graph
  dil_label <- if (identical(as.numeric(dilution), 1)) "diluted " else ""
  Graph1_2 <- size_plot(forGraph1, paste0("Results with ", dil_label,
                                          "binomial model prior (EMS = ", EMS, ")"))
  Graph2_2 <- size_plot(forGraph2, paste0("Results with ", dil_label,
                                          "binomial-beta model prior (EMS = ", EMS, ")"))

  # Putting together the last plot
  Finalplot <- arrange_plots_common_legend(Graph1_2, Graph2_2)

  out <- list(binomial = Graph1, beta = Graph2, combined = Finalplot)
  structure(out, class = "badp_plots", default = "combined")
}
