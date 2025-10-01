test_that(paste("model_pmp creates correct lists with graphs"), {

  data_prepared <- bdsm::economic_growth[,1:6] %>%
    bdsm::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    bdsm::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  bma_results <- bma(small_model_space, df = data_prepared, round= 3, dilution = 0)

  model_graphs <- model_pmp(bma_results, top = 16)

  expect_equal(class(model_graphs), "list")
  expect_equal(length(model_graphs), 3)
  expect_equal(class(model_graphs[[1]]), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))
  expect_equal(class(model_graphs[[2]]), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))
  expect_equal(class(model_graphs[[3]]), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg", "ggarrange"))
})
