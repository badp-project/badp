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

# Note: the robust standard deviations (PSDR, PSDRcon) in this object are not
# identified, because every model in full_model_space has more parameters than
# there are entities. See the "Rank of the sandwich covariance" section of
# ?optim_model_space. They are retained because bma() reports them, but they
# should not be used as a reference.
full_bma_results <- badp::bma(badp::full_model_space, round = 5)

usethis::use_data(full_bma_results, overwrite = TRUE)
