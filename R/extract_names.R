#' Extraction of names of the variables
#'
#' The function extract the names of the variables from the data set used in the analysis and
#' places them in a vector.
#'
#'
#' @param df Data frame with data for the analysis.
#'
#' @return A vector with names of the variables.
#'
#' @export
#'
#' @examples
#' \donttest{
#'  df <- bdsm::economic_growth
#'
#'  reg_names <- extract_names(df)
#'
#' }
extract_names <- function(df){
  reg_names <- colnames(df)
  reg_names <- reg_names[-(1:2)]
  reg_names[1] <- paste0(reg_names[1], "_lag")

  return(reg_names)
}
