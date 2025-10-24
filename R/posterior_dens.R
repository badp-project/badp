#' Graphs of the posterior densities of the coefficients
#'
#' This function draws graphs of the posterior densities of all the coeffcients of interest.
#'
#' @name posterior_dens
#'
#' @param bma_list bma object (the result of the bma function)
#' @param prior Parameter indicating which model prior should be used for calculations:
#' 1) "binomial" - using binomial model prior (default option) \cr
#' 2) "beta" - using binomial-beta model prior
#' @param SE Parameter indicating which standard errors should be used in calculation of posterior standard deviation: \cr
#' 1) "standard" - regular standard errors (default option)\cr
#' 2) "robust" - robust standard errors
#'
#' @return A list with the graphs of the posterior densities of coefficients for all the considered regressors.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' data_prepared <- bdsm::economic_growth[, 1:6] %>%
#'   bdsm::feature_standardization(
#'     excluded_cols = c(country, year, gdp)
#'   ) %>%
#'   bdsm::feature_standardization(
#'     group_by_col  = year,
#'     excluded_cols = country,
#'     scale         = FALSE
#'   )
#'
#' bma_results <- bma(
#'   model_space = bdsm::small_model_space,
#'   df          = data_prepared,
#'   round       = 3,
#'   dilution    = 0
#' )
#'
#' posterior_graphs <- posterior_dens(bma_results, prior = "binomial", SE = "robust")
#' }

utils::globalVariables(".data")

posterior_dens <- function(bma_list, prior = "binomial", SE = "standard"){

  if (!(prior %in% c("binomial", "beta"))) {
    stop("prior is wrongly specified: please use 'binomial' or 'beta'")
  }

  if (!(SE %in% c("standard", "robust"))) {
    stop("weight is wrongly specified: 'standard', or 'robust'")
  }

  if (prior=="binomial"){
    post_table <- bma_list[[1]]
  }else{
    post_table <- bma_list[[2]]
  }

  post_table <- post_table[,c(2:4)]
  x_names <- bma_list[[3]] # names of variables
  K <- length(x_names)

  if (SE=="standard"){
    error_column <- 2
  }else{
    error_column <- 3
  }

  distPlots<-list() # Opening a list  for the histogram plots

  for (i in 1:K){
    mu <- post_table[i,1]
    sigma <- post_table[i,error_column]
    x <- seq(mu - 4*sigma, mu + 4*sigma, length = 300)
    df <- data.frame(x = x, y = dnorm(x, mu, sigma))
    distPlots[[i]]<-invisible(ggplot2::ggplot(df, ggplot2::aes(x, y)) +
                                ggplot2::geom_line(color = "blue", size = 1.2) +
                                ggplot2::geom_area(data = subset(df, x >= mu - sigma & x <= mu + sigma),
                                                   ggplot2::aes(x, y), fill = "blue", alpha = 0.2) +

                                # Vertical lines: PM (red), ±PSD (blue)
                                ggplot2::geom_vline(xintercept = mu, color = "red", linetype = "dashed", size = 1) +
                                ggplot2::geom_vline(xintercept = mu - sigma, color = "blue", linetype = "dashed", size = 0.8) +
                                ggplot2::geom_vline(xintercept = mu + sigma, color = "blue", linetype = "dashed", size = 0.8) +

                                # PM label near the x-axis (same as before)
                                ggplot2::annotate("text",
                                         x = mu + 0.02 * (max(df$x) - min(df$x)),
                                         y = min(df$y) + 0.02 * (max(df$y) - min(df$y)),
                                         label = "PM", color = "red", fontface = "bold", size = 3, hjust = 0) +

                                # PM ± PSD labels near the top
                                ggplot2::annotate("text",
                                         x = mu - sigma,
                                         y = max(df$y) * 0.9,
                                         label = "PM - PSD",
                                         color = "blue", fontface = "bold", size = 3, hjust = 1.1) +

                                ggplot2::annotate("text",
                                         x = mu + sigma,
                                         y = max(df$y) * 0.9,
                                         label = "PM + PSD",
                                         color = "blue", fontface = "bold", size = 3, hjust = -0.1) +

                                ggplot2::labs(title = paste0(x_names[i], "  (PM=", mu, ", PSD=", sigma, ")"),
                                     x = NULL, y = "Density") +
                                ggplot2::theme_minimal(base_size = 14) +
                                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    )
    names(distPlots)[[i]] <- x_names[[i]]
  }

return(distPlots)

}
