test_that(paste("model_sizes creates correct lists with graphs"), {

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

  size_graphs <- model_sizes(bma_results)

  expect_equal(class(size_graphs), "list")
  expect_equal(length(size_graphs), 3)
  expect_equal(class(size_graphs[[1]]), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))
  expect_equal(class(size_graphs[[2]]), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))
  expect_equal(class(size_graphs[[3]]), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg", "ggarrange"))
})
