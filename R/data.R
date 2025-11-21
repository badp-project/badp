#' Economic Growth Data in the original format
#'
#' Data used in Growth Empirics in Panel Data under Model Uncertainty and Weak
#' Exogeneity (Moral-Benito, 2016, Journal of Applied Econometrics).
#'
#' @format ## `original_economic_growth`
#' A data frame with 292 rows and 13 columns
#' (73 countries and 4 periods + extra one for lagged dependent variable):
#' \describe{
#'   \item{year}{Year}
#'   \item{country}{Country ID}
#'   \item{gdp}{Logarithm of GDP per capita (2000 US dollars at PP)}
#'   \item{gdp_lag}{Lagged logarithm of GDP per capita (2000 US dollars at PP)}
#'   \item{ish}{Ratio of real domestic investment to GDP}
#'   \item{sed}{Stock of years of secondary education in the total population}
#'   \item{pgrw}{Average growth rate of population}
#'   \item{pop}{Population in millions of people}
#'   \item{ipr}{Purchasing-power-parity numbers for investment goods}
#'   \item{opem}{Exports plus imports as a share of GDP}
#'   \item{gsh}{Ratio of government consumption to GDP}
#'   \item{lnlex}{Logarithm of the life expectancy at birth}
#'   \item{polity}{Composite index given by the democracy score minus the
#'   autocracy score}
#' }
#' @source <http://qed.econ.queensu.ca/jae/datasets/moral-benito001/>
"original_economic_growth"


#' Economic Growth Data
#'
#' Data used in Growth Empirics in Panel Data under Model Uncertainty and Weak
#' Exogeneity (Moral-Benito, 2016, Journal of Applied Econometrics).
#'
#' @format ## `economic_growth`
#' A data frame with 365 rows and 12 columns
#' (73 countries and 4 periods + extra one for lagged dependent variable):
#' \describe{
#'   \item{year}{Year}
#'   \item{country}{Country ID}
#'   \item{gdp}{Logarithm of GDP per capita (2000 US dollars at PP)}
#'   \item{ish}{Ratio of real domestic investment to GDP}
#'   \item{sed}{Stock of years of secondary education in the total population}
#'   \item{pgrw}{Average growth rate of population}
#'   \item{pop}{Population in millions of people}
#'   \item{ipr}{Purchasing-power-parity numbers for investment goods}
#'   \item{opem}{Exports plus imports as a share of GDP}
#'   \item{gsh}{Ratio of government consumption to GDP}
#'   \item{lnlex}{Logarithm of the life expectancy at birth}
#'   \item{polity}{Composite index given by the democracy score minus the
#'   autocracy score}
#' }
#' @source <http://qed.econ.queensu.ca/jae/datasets/moral-benito001/>
"economic_growth"


#' Example output of the optim_model_space function (small version)
#'
#' A list with two elements: \code{params} and \code{stats} computed using the
#' \code{optim_model_space} function and the \code{economic_growth} dataset, but
#' using only three regressors: \code{ish}, \code{sed} and \code{pgrw}.
#'
#' @format ## `small_model_space`
#' A list with two elements.
#' \describe{
#'    \item{params}{
#'      A double matrix with 40 rows and \code{2^3 = 8} columns with the
#'      parameters for the model space.
#'      Each column represents a different model.
#'    }
#'    \item{stats}{
#'      A matrix representing the statistics computed with
#'      \code{compute_model_space_stats} based on \code{params}.
#'      The first row contains likelihoods for the models.
#'      The second row are almost 1/2 * BIC_k
#'      as in Raftery's Bayesian Model Selection in Social Research, eq. 19.
#'      The rows 3-7 are standard deviations.
#'      Finally, the rows 8-12 are robust standard deviations.
#'    }
#'    \item{reg_names}{
#'      A vector with the names of the variables.
#'    }
#'    \item{observations_num}{
#'      A total number of observations in the panel (292).
#'    }
#'    \item{df}{
#'      Data frame used in the analysis.
#'    }
#' }
"small_model_space"


#' Example output of the optim_model_space function
#'
#' A list with two elements: \code{params} and \code{stats} computed using the
#' \code{optim_model_space} function and the \code{economic_growth} dataset.
#'
#' @format ## `full_model_space`
#' A list with two elements.
#' \describe{
#'    \item{params}{
#'      A double matrix with 40 rows and \code{2^9 = 512} columns with the
#'      parameters for the model space.
#'      Each column represents a different model.
#'    }
#'    \item{stats}{
#'      A matrix representing the statistics computed with
#'      \code{compute_model_space_stats} based on \code{params}.
#'      The first row contains likelihoods for the models.
#'      The second row are almost 1/2 * BIC_k
#'      as in Raftery's Bayesian Model Selection in Social Research, eq. 19.
#'      The rows 3-7 are standard deviations.
#'      Finally, the rows 8-12 are robust standard deviations.
#'    }
#'    \item{reg_names}{
#'      A vector with the names of the variables.
#'    }
#'    \item{observations_num}{
#'      A total number of observations in the panel (292).
#'    }
#'    \item{df}{
#'      Data frame used in the analysis.
#'    }
#' }
"full_model_space"


#' Example output of the bma function
#'
#' A list with multiple elements summarising the BMA analysis
"full_bma_results"
