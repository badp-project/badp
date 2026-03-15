test_that(paste("coef_hist creates correct lists with graphs"), {

  bma_results <- bma(badp::small_model_space, round= 3, dilution = 0)

  coef_plots <- coef_hist(bma_results, kernel = 1)

  expect_equal(class(coef_plots), "list")
  expect_true(ggplot2::is_ggplot(coef_plots[[1]]))
  expect_true(ggplot2::is_ggplot(coef_plots[[2]]))
  expect_true(ggplot2::is_ggplot(coef_plots[[3]]))
})
