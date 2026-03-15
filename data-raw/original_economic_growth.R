## code to prepare `original_economic_growth` dataset goes here

fpath <- system.file("extdata", "economic_growth_raw.xlsx", package = "badp")

original_economic_growth <- readxl::read_excel(fpath)

usethis::use_data(original_economic_growth, overwrite = TRUE)
