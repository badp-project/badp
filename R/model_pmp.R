#' Graphs of the prior and posterior model probabilities for the best individual models
#'
#' This function draws four graphs of prior and posterior model probabilities for the best individual models: \cr
#' a) The results with binomial model prior (based on PMP - posterior model probability) \cr
#' b) The results with binomial-beta model prior (based on PMP - posterior model probability) \cr
#' Models on the graph are ordered according to their posterior model probability.
#'
#'
#' @param bma_list bma_list object (the result of the bma function)
#' @param top The number of the best model to be placed on the graphs
#'
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
#' }
#'
#'@name model_pmp

utils::globalVariables(c("ID", "Value", "Probability"))

model_pmp <- function(bma_list, top = NULL){

# Collecting information from the bma_list
R <- bma_list[[4]] # total number of regressors
M <- bma_list[[5]] # size of the model space
EMS <- bma_list[[8]] # expected model size
PMPs <- bma_list[[10]][,(R+1):(R+2)] # PMP_uniform, PMP_random
Priors <- bma_list[[11]] # Priors: uniform and random
dilution <- bma_list[[12]] # 0 - no dilution prior, 1 - dilution prior

if (is.null(top)){
  top <- M
}

if (top>M){# CONDITION about what to do if the user sets top that is higher than M
  # we tell the user that we are setting top = R
  message("The number of the best models (top) cannot be higher than the total number of models. We set top = R (total number of regressors) and continue :)")
  top = M
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

## Preparation of the Figures with ggplot
Graph1 <- ggplot2::ggplot(forGraph1, ggplot2::aes(x = ID, y = Value)) +
  ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
  ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
  ggplot2::ylab("Prior, Posterior") +
  ggplot2::xlab("Model number in the ranking")

Graph2 <- ggplot2::ggplot(forGraph2, ggplot2::aes(x = ID, y = Value)) +
  ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
  ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
  ggplot2::ylab("Prior, Posterior") +
  ggplot2::xlab("Model number in the ranking")

if (dilution==0){
Graph1_2 <- ggplot2::ggplot(forGraph1, ggplot2::aes(x = ID, y = Value)) +
  ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
  ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
  ggplot2::ylab("Prior, Posterior") +
  ggplot2::xlab("Model number in the ranking") +
  ggplot2::ggtitle(paste0("Results with binomial model prior (EMS = ", EMS, ")"))

Graph2_2 <- ggplot2::ggplot(forGraph2, ggplot2::aes(x = ID, y = Value)) +
  ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
  ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
  ggplot2::ylab("Prior, Posterior") +
  ggplot2::xlab("Model number in the ranking") +
  ggplot2::ggtitle(paste0("Results with binomial-beta model prior (EMS = ", EMS, ")"))
}

if (dilution==1){
  Graph1_2 <- ggplot2::ggplot(forGraph1, ggplot2::aes(x = ID, y = Value)) +
    ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
    ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
    ggplot2::ylab("Prior, Posterior") +
    ggplot2::xlab("Model number in the ranking") +
    ggplot2::ggtitle(paste0("Results with diluted binomial model prior (EMS = ", EMS, ")"))

  Graph2_2 <- ggplot2::ggplot(forGraph2, ggplot2::aes(x = ID, y = Value)) +
    ggplot2::geom_line(ggplot2::aes(color = Probability, linetype = Probability)) +
    ggplot2::scale_color_manual(values = c("darkred", "steelblue")) +
    ggplot2::ylab("Prior, Posterior") +
    ggplot2::xlab("Model number in the ranking") +
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
