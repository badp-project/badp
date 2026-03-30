## code to prepare `economic_growth` dataset goes here

economic_growth <- badp::original_economic_growth %>%
  join_lagged_col(gdp, lag_gdp, year, country, 10)

usethis::use_data(economic_growth, overwrite = TRUE)
