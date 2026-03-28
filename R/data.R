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


#' Example output of \code{\link{optim_model_space}} (small version)
#'
#' A list created with \code{\link{optim_model_space}} using the
#' \code{\link{economic_growth}} dataset and only three regressors:
#' \code{ish}, \code{sed}, and \code{pgrw}.
#'
#' @format A list with 5 elements:
#' \describe{
#'   \item{params}{
#'     A numeric matrix with 40 rows and 8 columns (corresponding to
#'     \eqn{2^3 = 8} models), containing parameter values for the model space.
#'     Each column represents a different model.
#'   }
#'   \item{stats}{
#'     A numeric matrix of statistics computed by
#'     \code{\link{compute_model_space_stats}} based on \code{params}.
#'     Row 1 contains model likelihoods. Row 2 contains a quantity proportional
#'     to \code{0.5 * BIC} (cf. Raftery, Bayesian Model Selection in Social Research,
#'     Eq. 19). Rows 3--7 contain standard deviations, and rows 8--12 contain
#'     robust standard deviations.
#'   }
#'   \item{reg_names}{
#'     A character vector with the names of the regressors.
#'   }
#'   \item{observations_num}{
#'     The total number of observations in the panel (292).
#'   }
#'   \item{df}{
#'     The data frame used in the analysis.
#'   }
#' }
"small_model_space"


#' Example output of \code{\link{optim_model_space}}
#'
#' A list created with \code{\link{optim_model_space}} using the
#' \code{\link{economic_growth}} dataset.
#'
#' @format A list with 5 elements:
#' \describe{
#'   \item{params}{
#'     A numeric matrix with 40 rows and 512 columns, containing parameter
#'     values for the full model space. Each column represents a different model.
#'   }
#'   \item{stats}{
#'     A numeric matrix of statistics computed by
#'     \code{\link{compute_model_space_stats}} based on \code{params}.
#'     Row 1 contains model likelihoods. Row 2 contains a quantity proportional
#'     to \code{0.5 * BIC} (cf. Raftery, Bayesian Model Selection in Social Research,
#'     Eq. 19). Rows 3--7 contain standard deviations, and rows 8--12 contain
#'     robust standard deviations.
#'   }
#'   \item{reg_names}{
#'     A character vector with the names of the variables.
#'   }
#'   \item{observations_num}{
#'     The total number of observations in the panel (292).
#'   }
#'   \item{df}{
#'     The data frame used in the analysis.
#'   }
#' }
"full_model_space"


#' Example output of \code{\link{optim_model_space}} for non-nested models
#'
#' A list created with \code{\link{optim_model_space}} using the
#' \code{\link{economic_growth}} dataset.
#'
#' @format A list with 5 elements:
#' \describe{
#'   \item{params}{
#'     A numeric matrix with 40 rows and 512 columns, containing parameter
#'     values for the model space. Each column represents a different model.
#'   }
#'   \item{stats}{
#'     A numeric matrix of statistics computed by
#'     \code{\link{compute_model_space_stats}} based on \code{params}.
#'     Row 1 contains model likelihoods. Row 2 contains a quantity proportional
#'     to \code{0.5 * BIC} (cf. Raftery, Bayesian Model Selection in Social Research,
#'     Eq. 19). Rows 3--7 contain standard deviations, and rows 8--12 contain
#'     robust standard deviations.
#'   }
#'   \item{reg_names}{
#'     A character vector with the names of the variables.
#'   }
#'   \item{observations_num}{
#'     The total number of observations in the panel (292).
#'   }
#'   \item{df}{
#'     The data frame used in the analysis.
#'   }
#' }
"model_space_nonnested"


#' Example output of the bma function
#'
#' A list with multiple elements summarising the BMA analysis
"full_bma_results"


#' Migration data in the original format
#'
#' Data used in the manuscript Beck, K., Dubel, M., Wyszyński, M., & Yersh, V. (2025).
#' Most seek higher earnings, some escape unemployment, but none look for a handout:
#' Economic drivers of labor mobility in the European Union.
#' https://doi.org/10.2139/ssrn.5472939
#'
#' @format ## `migration_data`
#' A data frame with 1012 rows and 8 columns
#' (253 country pairs and 4 periods + one additional observation for the lagged dependent variable):
#' \describe{
#'   \item{Time}{Year}
#'   \item{Pair}{Country pair ID}
#'   \item{Mig}{Net migration between two countries}
#'   \item{Mig_lag}{Lagged net migration between two countries}
#'   \item{Earn}{Difference in average real after-tax earnings in PPP}
#'   \item{Unemp}{Difference in the unemployment rate}
#'   \item{Social}{Difference in average social benefits in PPP}
#'   \item{Tax}{Difference in average tax rate}
#' }
#' @source <https://doi.org/10.7910/DVN/M9DADS>
"migration_data"

#' Example output of \code{\link{optim_model_space}} in the case of migration data
#'
#' A list created with \code{\link{optim_model_space}} using the
#' \code{\link{migration_data}} dataset.
#'
#' @format A list with 5 elements:
#' \describe{
#'   \item{params}{
#'     A numeric matrix with 51 rows and 16 columns, containing parameter
#'     values for the full model space. Each column represents a different model.
#'   }
#'   \item{stats}{
#'     A numeric matrix of statistics computed by
#'     \code{\link{compute_model_space_stats}} based on \code{params}.
#'     Row 1 contains model likelihoods. Row 2 contains a quantity proportional
#'     to \code{0.5 * BIC} (cf. Raftery, Bayesian Model Selection in Social Research,
#'     Eq. 19). Rows 3--7 contain standard deviations, and rows 8--12 contain
#'     robust standard deviations.
#'   }
#'   \item{reg_names}{
#'     A character vector with the names of the variables.
#'   }
#'   \item{observations_num}{
#'     The total number of observations in the panel (1012).
#'   }
#'   \item{df}{
#'     The data frame used in the analysis.
#'   }
#' }
"migration_model_space"
