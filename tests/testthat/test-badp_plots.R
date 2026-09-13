test_that("print.badp_plots names the panels when `which` is not one of them", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  sizes <- model_sizes(bma_results)

  # A name that is not a panel, and indices that are out of range, zero,
  # fractional, missing or not a single value. `[[` rejects most of these
  # anyway, but with a message about subscripts rather than about the panels
  # this object holds.
  expect_error(print(sizes, which = "nonsense"), "must be a single plot name")
  expect_error(print(sizes, which = 999), "must be a single plot name")
  expect_error(print(sizes, which = 0), "must be a single plot name")
  expect_error(print(sizes, which = 1.5), "must be a single plot name")
  expect_error(print(sizes, which = NA), "must be a single plot name")
  expect_error(print(sizes, which = c(1, 2)), "must be a single plot name")

  # The message lists what the caller could have asked for instead.
  expect_error(print(sizes, which = 999), names(sizes)[1], fixed = TRUE)
})

test_that("print.badp_plots draws a panel chosen by name or by position", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  sizes <- model_sizes(bma_results)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_invisible(print(sizes))
  expect_no_error(print(sizes, which = 1))
  expect_no_error(print(sizes, which = length(sizes)))
  expect_no_error(print(sizes, which = names(sizes)[1]))
})
