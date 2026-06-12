utils::globalVariables(".data")

#' Graphs of the distribution of the coefficients over the model space
#'
#' This function draws graphs of the distribution (in the form of histogram or kernel density) of
#' the coefficients for all the considered regressors over the part of the model space that includes
#' these regressors (half of the model space).
#'
#' @param bma_list An object of class \code{badp_bma}, typically returned by \code{\link{bma}}.
#' @param weight Parameter indicating whether the coefficients should be weighted by posterior model probabilities:
#' 1) NULL - no weighting (default option) \cr
#' 2) "binomial" - using posterior model probabilities based on binomial model prior \cr
#' 3) "beta" - using posterior model probabilities based on binomial-beta model prior
#' @param bin_method Character string specifying the method for bin widths (default: \code{"FD"}). One of: \cr
#' \code{"FD"} - Freedman-Diaconis method; \cr
#' \code{"SC"} - Scott method; \cr
#' \code{"vec"} - user specified bin widths provided through a vector (parameter: \code{bin_widths}).
#' @param bin_widths A vector with bin widths to be used to construct histograms for the regressors. The vector must be of the size equal to total number of regressors plus one for the lagged dependent variable. The vector with bin widths is used only if parameter \code{bin_method = "vec"}.
#' @param use_bin_count Parameter taking the values (default: \code{use_bin_count = 0}): \cr
#' 1 - the histogram will be built based on the number of bins specified by the user through parameter \code{bin_counts}. If \code{use_bin_count = 1}, the function ignores parameter \code{bin_method}. \cr
#' 0 - the histogram will be built in line with parameter \code{bin_method}.
#' @param bin_counts A vector with the numbers of bins to be used to construct histograms for the regressors. The vector must be of the size equal to total number of regressors plus one for the lagged dependent variable. The vector with bin counts is used only if parameter \code{use_bin_count = 1}.
#' @param use_kernel A parameter taking the values (default: \code{use_kernel = 0}):\cr
#' 1 - the function will build graphs using kernel density for the distribution of coefficients (with \code{use_kernel = 1}, the function ignores parameters \code{bin_method} and \code{use_bin_count}) \cr
#' 0 - the function will build regular histogram density for the distribution of coefficients
#'
#' @return A list with the graphs of the distribution of coefficients for all the considered regressors and the lagged dependent variable.
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
#' coef_plots <- coef_hist(bma_results, use_kernel = 1)
#' }
coef_hist <- function(bma_list, weight = NULL, bin_method = c("FD", "SC", "vec"), bin_widths = NULL, use_bin_count = 0, bin_counts = NULL, use_kernel = 0){
  if (!(is.null(weight) || weight %in% c("binomial", "beta"))) {
      stop("weight is wrongly specified: please use NULL, 'binomial', or 'beta'")
  }
  bin_method <- match.arg(bin_method)
  if (!(use_kernel %in% c(0, 1))) {
    stop("use_kernel must be 0 or 1")
  }
  if (!(use_bin_count %in% c(0, 1))) {
    stop("use_bin_count must be 0 or 1")
  }

  x_names <- bma_list[[3]] # names of variables
  K <- bma_list[[4]] + 1 # number of variables
  alpha <- bma_list[[13]]
  betas <- bma_list[[14]]

  if (!is.null(weight)){
    R <- K-1
    forJointness <- bma_list[[6]]
    alpha <- bma_list[[13]]
    numb_of_models <- nrow(forJointness)
    numb_of_betas <- numb_of_models/2
    new_betas <- matrix(0, nrow = numb_of_betas, ncol = R)
    column_with_prob_offset = if (weight=='binomial') {1} else {2}
    alpha1 <- alpha*forJointness[,R+column_with_prob_offset]
    for (k in 1:R){
      j <- 1
      for (i in 1:numb_of_models){
        if (forJointness[i,k]==1){
          new_betas[j,k]=betas[j,k]*forJointness[i,R+1]
          j <- j+1
        }
      }
    }
    alpha <- alpha1
    betas <- new_betas
  }

  # Adding colnames and changing to dataframe
  colnames(alpha) <- x_names[1]
  colnames(betas) <- x_names[-1]
  alpha <- as.data.frame(alpha)
  betas <- as.data.frame(betas)

  histPlots<-list() # Opening a list  for the histogram plots

  # CONDITION for using kernel density or regular histogram
  if (use_kernel==1){
    histPlots[[1]]<-invisible(ggplot2::ggplot(alpha, ggplot2::aes(x = .data[[x_names[1]]])) +
                                ggplot2::geom_density(fill = "skyblue", alpha = 0.7) +
                                ggplot2::labs(
                                  title = paste("Distribution of", x_names[1], "coefficients"),
                                  x = paste0("Coefficients on ",x_names[1]),
                                  y = "Density") +
                                ggplot2::theme_minimal(base_size = 12) +
                                ggplot2::theme(
                                  plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
                                  axis.title = ggplot2::element_text(face = "bold")))
    names(histPlots)[[1]] <-x_names[[1]]
    for (i in 2:K){
      histPlots[[i]]<-invisible(ggplot2::ggplot(betas, ggplot2::aes(x = .data[[x_names[i]]])) +
                                  ggplot2::geom_density(fill = "skyblue", alpha = 0.7) +
                                  ggplot2::labs(
                                    title = paste("Distribution of", x_names[i], "coefficients"),
                                    x = paste0("Coefficients on ",x_names[i]),
                                    y = "Density") +
                                  ggplot2::theme_minimal(base_size = 12) +
                                  ggplot2::theme(
                                    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
                                    axis.title = ggplot2::element_text(face = "bold")))
      names(histPlots)[[i]] <-x_names[[i]]
    }

  } else{# REGULAR HISTOGRAM BELOW

    # CONDITION for graphs plotted with binwidth:
    if (use_bin_count==0){### Rules for bin width
      # 1) Freedman-Diaconis (FD)
      if (bin_method=="FD"){bw<-(stats::IQR(alpha[,1])*2)/(length(alpha[,1])^(1/3))}
      # 2) Scott (SC)
      if (bin_method=="SC"){bw<-(stats::sd(alpha[,1])*3.5)/(length(alpha[,1])^(1/3))}
      # 3) Binwidth sizes
      if(bin_method=="vec"){
        if (is.null(bin_widths)){stop("Please provide a vector with bin width sizes through parameter bin_widths")}
        if (length(bin_widths)!=K){stop("bin_widths is misspecified: bin_widths should have K (number of regressors +1) elements")}
        bw<-bin_widths[1]
      }
      histPlots[[1]] <- invisible(ggplot2::ggplot(alpha, ggplot2::aes(x = .data[[x_names[1]]])) +
                                    ggplot2::geom_histogram(binwidth = bw, fill = "skyblue", color = "skyblue", alpha = 0.8) +
                                    ggplot2::labs(
                                      title = paste("Distribution of", x_names[1], "coefficients"),
                                      x = paste0("Coefficients on ",x_names[1]),
                                      y = "Frequency") +
                                    ggplot2::theme_minimal(base_size = 12) +
                                    ggplot2::theme(
                                      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
                                      axis.title = ggplot2::element_text(face = "bold")))
      names(histPlots)[[1]] <- x_names[[1]]
      for (i in 2:K){ # at this LOOP we go through all the regressors
        # betas has R columns (one per non-lag regressor); the loop index i
        # runs 2..K=R+1 over x_names, so we index betas with i-1.
        beta_col <- betas[, i - 1]
        # 1) Freedman-Diaconis (FD)
        if (bin_method=="FD"){bw<-(stats::IQR(beta_col)*2)/(length(beta_col)^(1/3))}
        # 2) Scott (SC)
        if (bin_method=="SC"){bw<-(stats::sd(beta_col)*3.5)/(length(beta_col)^(1/3))}
        # 3) Binwidth sizes
        if(bin_method=="vec"){
          if (is.null(bin_widths)){stop("Please provide a vector with bin width sizes through parameter bin_widths")}
          if (length(bin_widths)!=K){stop("bin_widths is misspecified: bin_widths should have K elements")}
          bw<-bin_widths[i]
        }
        histPlots[[i]] <- invisible(ggplot2::ggplot(betas, ggplot2::aes(x = .data[[x_names[i]]])) +
                                      ggplot2::geom_histogram(binwidth=bw, fill = "skyblue", color = "skyblue", alpha = 0.8) +
                                      ggplot2::labs(
                                        title = paste("Distribution of", x_names[i], "coefficients"),
                                        x = paste0("Coefficients on ",x_names[i]),
                                        y = "Frequency") +
                                      ggplot2::theme_minimal(base_size = 12) +
                                      ggplot2::theme(
                                        plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
                                        axis.title = ggplot2::element_text(face = "bold")))
        names(histPlots)[[i]] <- x_names[[i]]
      }
    }

    # CONDITION for graphs plotted with bins - through setting the number of bins:
    if (use_bin_count==1){### Rules for bin width
      if (is.null(bin_counts)){stop("Please provide a vector with number of bins through parameter bin_counts")}
      if (length(bin_counts)!=K){stop("bin_counts is misspecified: bin_counts should have K elements")}
      histPlots[[1]]<-invisible(ggplot2::ggplot(alpha, ggplot2::aes(x = .data[[x_names[1]]])) +
                                  ggplot2::geom_histogram(bins=bin_counts[1], fill = "skyblue", color = "skyblue", alpha = 0.8) +
                                  ggplot2::labs(
                                    title = paste("Distribution of", x_names[1], "coefficients"),
                                    x = paste0("Coefficients on ",x_names[1]),
                                    y = "Frequency") +
                                  ggplot2::theme_minimal(base_size = 12) +
                                  ggplot2::theme(
                                    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
                                    axis.title = ggplot2::element_text(face = "bold")))
      names(histPlots)[[1]] <-x_names[[1]]
      for (i in 2:K){
        histPlots[[i]]<-invisible(ggplot2::ggplot(betas, ggplot2::aes(x = .data[[x_names[i]]])) +
                                    ggplot2::geom_histogram(bins=bin_counts[i], fill = "skyblue", color = "skyblue", alpha = 0.8) +
                                    ggplot2::labs(
                                      title = paste("Distribution of", x_names[i], "coefficients"),
                                      x = paste0("Coefficients on ",x_names[i]),
                                      y = "Frequency") +
                                    ggplot2::theme_minimal(base_size = 12) +
                                    ggplot2::theme(
                                      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
                                      axis.title = ggplot2::element_text(face = "bold")))
        names(histPlots)[[i]] <-x_names[[i]]
      }
    }
  }
  return(histPlots)
}
