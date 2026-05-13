test_that(paste("coef_hist creates correct lists with graphs"), {

  bma_results <- bma(badp::small_model_space, round= 3, dilution = 0)

  coef_plots <- coef_hist(bma_results, use_kernel = 1)

  expect_equal(class(coef_plots), "list")
  expect_true(ggplot2::is_ggplot(coef_plots[[1]]))
  expect_true(ggplot2::is_ggplot(coef_plots[[2]]))
  expect_true(ggplot2::is_ggplot(coef_plots[[3]]))
})

test_that("coef_hist works with default histogram (FD bin method)", {
  bma_results <- bma(badp::small_model_space, round = 3, dilution = 0)

  # Default bin_method = "FD", use_kernel = 0. This exercises the per-regressor
  # IQR computation across all betas columns; regression test for the
  # off-by-one where betas[, K] was accessed but betas only has R columns.
  coef_plots <- coef_hist(bma_results)
  expect_equal(class(coef_plots), "list")
  expect_equal(length(coef_plots), bma_results$R + 1)
  for (p in coef_plots) {
    expect_true(ggplot2::is_ggplot(p))
  }
})

test_that("coef_hist works with Scott (SC) bin method", {
  bma_results <- bma(badp::small_model_space, round = 3, dilution = 0)

  coef_plots <- coef_hist(bma_results, bin_method = "SC")
  expect_equal(length(coef_plots), bma_results$R + 1)
})

test_that("coef_hist works with user-supplied bin widths", {
  bma_results <- bma(badp::small_model_space, round = 3, dilution = 0)
  K <- bma_results$R + 1

  coef_plots <- coef_hist(bma_results, bin_method = "vec", bin_widths = rep(0.05, K))
  expect_equal(length(coef_plots), K)
})

test_that("coef_hist works with user-supplied bin counts", {
  bma_results <- bma(badp::small_model_space, round = 3, dilution = 0)
  K <- bma_results$R + 1

  coef_plots <- coef_hist(bma_results, use_bin_count = 1, bin_counts = rep(20, K))
  expect_equal(length(coef_plots), K)
})

test_that("coef_hist validates its flag arguments", {
  bma_results <- bma(badp::small_model_space, round = 3, dilution = 0)

  expect_error(
    coef_hist(bma_results, use_kernel = 2),
    "use_kernel must be 0 or 1"
  )
  expect_error(
    coef_hist(bma_results, use_bin_count = 2),
    "use_bin_count must be 0 or 1"
  )
  expect_error(
    coef_hist(bma_results, bin_method = "vec"),
    "bin_widths"
  )
  expect_error(
    coef_hist(bma_results, use_bin_count = 1),
    "bin_counts"
  )
})
