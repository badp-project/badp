test_that(paste("jointness computes correct jointness table"), {

  data_prepared <- badp::economic_growth[,1:6] %>%
    badp::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    badp::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  bma_results <- bma(small_model_space, round= 3, dilution = 0)

  jointness_table <- jointness(bma_results, measure = "HCGHM", rho = 0.5, round= 3)

  expect_equal(nrow(jointness_table), bma_results[[4]])
  expect_equal(ncol(jointness_table), bma_results[[4]])
  expect_equal(class(jointness_table), c("matrix","array"))
  expect_equal(is.na(jointness_table[[1]]), TRUE)
})
