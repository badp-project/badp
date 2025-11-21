test_that(paste("bma computes correct bma_list and all its objects"), {

  data_prepared <- bdsm::economic_growth[,1:6] %>%
    bdsm::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    bdsm::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  bma_results <- bma(small_model_space, round= 3, dilution = 0)

  expect_equal(length(bma_results), 16)
  expect_equal(is.numeric(bma_results[[4]]), TRUE)
  expect_equal(is.numeric(bma_results[[5]]), TRUE)
  expect_equal(length(bma_results[[3]]), bma_results[[4]]+1)
  expect_equal(nrow(bma_results[[1]]), bma_results[[4]]+1)
  expect_equal(ncol(bma_results[[1]]), 8)
  expect_equal(nrow(bma_results[[2]]), bma_results[[4]]+1)
  expect_equal(ncol(bma_results[[2]]), 8)
  expect_equal(ncol(bma_results[[6]]), bma_results[[4]]+2)
  expect_equal(nrow(bma_results[[6]]), bma_results[[5]])
  expect_equal(ncol(bma_results[[7]]), bma_results[[4]]+2+3*(bma_results[[4]]+1))
  expect_equal(nrow(bma_results[[7]]), bma_results[[5]])
  expect_equal(is.numeric(bma_results[[8]]), TRUE)
  expect_equal(ncol(bma_results[[9]]), 2)
  expect_equal(nrow(bma_results[[9]]), bma_results[[4]]+1)
  expect_equal(ncol(bma_results[[10]]), bma_results[[4]]+2)
  expect_equal(nrow(bma_results[[10]]), bma_results[[5]])
  expect_equal(ncol(bma_results[[11]]), 2)
  expect_equal(nrow(bma_results[[11]]), bma_results[[5]])
  expect_equal(is.numeric(bma_results[[12]]), TRUE)
  expect_equal(ncol(bma_results[[13]]), 1)
  expect_equal(nrow(bma_results[[13]]), bma_results[[5]])
  expect_equal(ncol(bma_results[[14]]), bma_results[[4]])
  expect_equal(nrow(bma_results[[14]]), bma_results[[5]]/2)
  expect_equal(ncol(bma_results[[15]]), 1)
  expect_equal(nrow(bma_results[[15]]), bma_results[[5]])
  expect_equal(ncol(bma_results[[16]]), 2)
  expect_equal(nrow(bma_results[[16]]), 2)
})
