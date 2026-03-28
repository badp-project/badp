#' Dataframe with no lagged column
#'
#' This function allows the user to turn data in the format with lagged values for a
#' chosen column (i.e. there are two columns with the same quantity, but one
#' column is lagged in time) into the format with just one column
#'
#' @param df Dataframe with data with a column with lagged values
#' @param col Column with quantity not lagged
#' @param col_lagged Column with the same quantity as \code{col}, but the values
#' are lagged in time
#' @param timestamp_col Column with timestamps (e.g. years)
#' @param entity_col Column with entities (e.g. countries)
#' @param timestep Difference between timestamps (e.g. 10)
#'
#' @return
#' A dataframe with two columns merged, i.e. just one column with the desired
#' quantity is left.
#'
#' @examples
#' df <- data.frame(
#'   year = c(2000, 2001, 2002, 2003, 2004),
#'   country = c("A", "A", "B", "B", "C"),
#'   gdp = c(1, 2, 3, 4, 5),
#'   gdp_lagged = c(NA, 1, 2, 3, 4)
#' )
#'
#' join_lagged_col(df, gdp, gdp_lagged, year, country, 1)
#'
#' @importFrom rlang :=
#'
#' @export
#' @keywords internal
join_lagged_col <- function(df, col, col_lagged, timestamp_col,
                            entity_col, timestep = NULL) {
  non_lagged_df <- df %>%
    dplyr::select(c({{ timestamp_col }}, {{ entity_col }}, {{ col }}))
  lagged_df <- df %>%
    dplyr::select(c({{ timestamp_col }}, {{ entity_col }}, {{ col_lagged }})) %>%
    dplyr::mutate("{{timestamp_col}}" := {{ timestamp_col }} - timestep)

  lagged_df %>% dplyr::full_join(non_lagged_df,
                          by = dplyr::join_by(
                            {{ timestamp_col }} == {{ timestamp_col }},
                            {{ entity_col }} == {{ entity_col }}
                          )) %>%
    dplyr::arrange({{ entity_col }}, {{ timestamp_col }}) %>%
    dplyr::mutate("{{col}}" := dplyr::coalesce({{ col }}, {{ col_lagged }})) %>%
    dplyr::select(!{{ col_lagged }}) %>%
    dplyr::left_join(dplyr::select(df, !{{ col }} & !{{ col_lagged }}),
              by = dplyr::join_by(
                {{ timestamp_col }} == {{ timestamp_col }},
                {{ entity_col }} == {{ entity_col }}
              ))
}


#' Perform feature standardization
#'
#' @description
#' This function performs
#' [feature standardization](https://en.wikipedia.org/wiki/Feature_scaling)
#' (also known as z-score normalization) by centering the features around their
#' mean and scaling by their standard deviation.
#'
#' @param df Data frame with the data.
#' @param excluded_cols Unquoted column names to exclude from standardization.
#'   If missing, all columns are standardized.
#' @param group_by_col Unquoted column names to group the data by
#'   before applying standardization. If missing, no grouping is performed.
#' @param scale Logical. If `TRUE` (default) scales by the standard deviation.
#'
#' @return A data frame with standardized features.
#'
#' @examples
#' df <- data.frame(
#'   year = c(2000, 2001, 2002, 2003, 2004),
#'   country = c("A", "A", "B", "B", "C"),
#'   gdp = c(1, 2, 3, 4, 5),
#'   ish = c(2, 3, 4, 5, 6),
#'   sed = c(3, 4, 5, 6, 7)
#' )
#'
#' # Standardize every column
#' df_with_only_numeric_values <- df[, setdiff(names(df), "country")]
#' feature_standardization(df_with_only_numeric_values)
#'
#' # Standardize all columns except 'country'
#' feature_standardization(df, excluded_cols = country)
#'
#' # Standardize across countries (grouped by 'country')
#' feature_standardization(df, group_by_col = country)
#'
#' # Standardize, excluding 'country' and group-wise by 'year'
#' feature_standardization(df, excluded_cols = country, group_by_col = year)
#'
#' @export
feature_standardization <- function(df, excluded_cols, group_by_col, scale = TRUE) {
  df <- tibble::as_tibble(df)
  if (missing(group_by_col)) {
    # No grouping requested
    if (missing(excluded_cols)) {
      # No columns to exclude => standardize every column
      df %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), function(x) c(scale(x, scale = scale))))
    } else {
      # Exclude specified columns from standardization
      df %>%
        dplyr::mutate(dplyr::across(!{{ excluded_cols }}, function(x) c(scale(x, scale = scale))))
    }
  } else {
    # Grouping is requested
    if (missing(excluded_cols)) {
      # No columns to exclude => standardize every column within group
      df %>%
        dplyr::group_by({{ group_by_col }}) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), function(x) c(scale(x, scale = scale)))) %>%
        dplyr::ungroup()
    } else {
      # Exclude specified columns from standardization, within groups
      df %>%
        dplyr::group_by({{ group_by_col }}) %>%
        dplyr::mutate(dplyr::across(!{{ excluded_cols }}, function(x) c(scale(x, scale = scale)))) %>%
        dplyr::ungroup()
    }
  }
}
