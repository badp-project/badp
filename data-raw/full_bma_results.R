"full_bma_results"

library(magrittr)

data_prepared <- bdsm::economic_growth %>%
  bdsm::feature_standardization(
    excluded_cols = c(country, year, gdp)
  ) %>%
  bdsm::feature_standardization(
    group_by_col  = year,
    excluded_cols = country,
    scale         = FALSE
  )

full_bma_results <- bdsm::bma(full_model_space, df = data_prepared, round = 5)

usethis::use_data(full_bma_results, overwrite = TRUE)
