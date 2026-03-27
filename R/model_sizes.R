#' Graphs of the prior and posterior model probabilities of the model sizes
#'
#' This function draws two graphs of prior and posterior model probabilities: \cr
#' a) The results with binomial model prior (based on PMP - posterior model probability) \cr
#' b) The results with binomial-beta model prior (based on PMP - posterior model probability)
#'
#' @param bma_list bma_list object (the result of the bma function)
#'
#' @return A list with three graphs with prior and posterior model probabilities for model sizes:\cr
#' 1) The results with binomial model prior (based on PMP - posterior model probability) \cr
#' 2) The results with binomial-beta model prior (based on PMP - posterior model probability) \cr
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
#' }
#'
#'@name model_sizes

utils::globalVariables(c("ID", "Value", "Probability"))

model_sizes <- function(bma_list){

  R <- bma_list[[4]] # total number of regressors
  M <- bma_list[[5]] # size of the model space
  EMS <- bma_list[[8]] # expected model size
  sizePriors <- bma_list[[9]] # table with uniform and random model priors spread over model sizes
  modelPosterior <- bma_list[[10]] # table with posterior model probabilities
  dilution <- bma_list[[12]] # 0 - no dilution prior, 1 - dilution prior

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

  ## Preparation of the Figures with ggplot

  Graph1 <- ggplot2::ggplot(forGraph1, ggplot2::aes(x = ID, y = Value)) +
    ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
    ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
    ggplot2::ylab("Prior, Posterior") + ggplot2::xlab("Model size (number of regressors)")

  Graph2 <- ggplot2::ggplot(forGraph2, ggplot2::aes(x = ID, y = Value)) +
    ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
    ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
    ggplot2::ylab("Prior, Posterior") + ggplot2::xlab("Model size (number of regressors)")

  ## Preparation of the data for BIG COMBINED GRAPH
  if (dilution==0){
    Graph1_2 <- ggplot2::ggplot(forGraph1, ggplot2::aes(x = ID, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
      ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
      ggplot2::ylab("Prior, Posterior") + ggplot2::xlab("Model size (number of regressors)") +
      ggplot2::ggtitle(paste0("Results with binomial model prior (EMS = ", EMS, ")"))

    Graph2_2 <- ggplot2::ggplot(forGraph2, ggplot2::aes(x = ID, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
      ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
      ggplot2::ylab("Prior, Posterior") + ggplot2::xlab("Model size (number of regressors)") +
      ggplot2::ggtitle(paste0("Results with binomial-beta model prior (EMS = ", EMS, ")"))
  }

  if (dilution==1){
    Graph1_2 <- ggplot2::ggplot(forGraph1, ggplot2::aes(x = ID, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
      ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
      ggplot2::ylab("Prior, Posterior") + ggplot2::xlab("Model size (number of regressors)") +
      ggplot2::ggtitle(paste0("Results with diluted binomial model prior (EMS = ", EMS, ")"))

    Graph2_2 <- ggplot2::ggplot(forGraph2, ggplot2::aes(x = ID, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
      ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
      ggplot2::ylab("Prior, Posterior") + ggplot2::xlab("Model size (number of regressors)") +
      ggplot2::ggtitle(paste0("Results with diluted binomial-beta model prior (EMS = ", EMS, ")"))
  }

  # Putting together the last plot
  Finalplot <- ggpubr::ggarrange(Graph1_2,Graph2_2,
                                 labels = c("a)", "b)"),
                                 ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

  print(Finalplot)

  out <- list(Graph1, Graph2, Finalplot)
  return(out)
}
