"full_bma_results"

library(magrittr)

data_prepared <- badp::economic_growth %>%
  badp::feature_standardization(
    excluded_cols = c(country, year, gdp)
  ) %>%
  badp::feature_standardization(
    group_by_col  = year,
    excluded_cols = country,
    scale         = FALSE
  )

full_bma_results <- badp::bma(full_model_space, round = 5)

usethis::use_data(full_bma_results, overwrite = TRUE)
