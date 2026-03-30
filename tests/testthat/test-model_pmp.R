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
  expect_equal(class(model_graphs[[1]]), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))
  expect_equal(class(model_graphs[[2]]), c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))
  expect_equal(class(model_graphs[[3]]), c("gtable", "gTree", "grob", "gDesc"))

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

  # Combined plot structure: 3 rows x 1 column (plot, plot, legend)
  combined <- model_graphs[[3]]
  expect_equal(nrow(combined$layout), 3)
  expect_equal(max(combined$layout$l), 1) # single column

  # Panel labels "a)" and "b)" are embedded in the plot titles
  grob_classes <- vapply(combined$grobs, function(g) {
    cl <- class(g)
    if ("gtable" %in% cl) "gtable" else cl[1]
  }, character(1))
  # Two plot gtables + one legend gtable
  expect_equal(sum(grob_classes == "gtable"), 3)

  # Legend grob is present (guide-box)
  grob_names <- vapply(combined$grobs, function(g) g$name, character(1))
  expect_true(any(grepl("guide-box", grob_names)))
})
