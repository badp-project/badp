test_that(paste("posterior_dens creates correct lists with graphs"), {

  bma_results <- bma(bdsm::small_model_space, round= 3, dilution = 0)

  distPlots <- posterior_dens(bma_results, prior = "binomial", SE = "standard")

  expect_equal(class(distPlots), "list")
  expect_true(ggplot2::is_ggplot(distPlots[[1]]))
  expect_true(ggplot2::is_ggplot(distPlots[[2]]))
  expect_true(ggplot2::is_ggplot(distPlots[[3]]))
})
