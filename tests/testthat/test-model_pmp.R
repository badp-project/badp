test_that(paste("model_pmp creates correct lists with graphs"), {

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

  model_graphs <- model_pmp(bma_results, top = 16)

  # Basic structure
  expect_equal(class(model_graphs), "list")
  expect_equal(length(model_graphs), 3)
  expect_true(inherits(model_graphs[[1]], "ggplot"))
  expect_true(inherits(model_graphs[[2]], "ggplot"))
  # Combined plot is now a patchwork object (which also inherits from
  # ggplot, so it draws when printed at the console). This replaces the
  # previous gtable/TableGrob return type from gridExtra::grid.arrange.
  expect_true(inherits(model_graphs[[3]], "patchwork"))
  expect_true(inherits(model_graphs[[3]], "ggplot"))

  # Individual ggplots have correct data and aesthetics
  g1_data <- ggplot2::ggplot_build(model_graphs[[1]])$data[[1]]
  g2_data <- ggplot2::ggplot_build(model_graphs[[2]])$data[[1]]
  expect_true(nrow(g1_data) > 0)
  expect_true(nrow(g2_data) > 0)

  g1_labels <- model_graphs[[1]]$labels
  expect_equal(g1_labels$y, "Prior, Posterior")
  expect_equal(g1_labels$x, "Model number in the ranking")

  g2_labels <- model_graphs[[2]]$labels
  expect_equal(g2_labels$y, "Prior, Posterior")
  expect_equal(g2_labels$x, "Model number in the ranking")

  # Combined plot still contains two subplots with the labelled titles.
  # patchwork stores the first sub-plot in $patches$plots and the last
  # one at the top level of the patchwork object, so we collect both
  # titles before checking.
  combined <- model_graphs[[3]]
  subplot_titles <- c(
    combined$patches$plots[[1]]$labels$title,
    combined$labels$title
  )
  expect_true(any(grepl("^a\\)", subplot_titles)))
  expect_true(any(grepl("^b\\)", subplot_titles)))
})
