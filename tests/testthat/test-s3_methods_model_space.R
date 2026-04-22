test_that("badp_model_space class is properly assigned", {
  expect_s3_class(small_model_space, "badp_model_space")
  expect_s3_class(full_model_space, "badp_model_space")
})

test_that("model_space object has correct structure", {
  # Test structure of small_model_space
  expect_type(small_model_space, "list")
  expect_true("params" %in% names(small_model_space))
  expect_true("stats" %in% names(small_model_space))
  expect_true("reg_names" %in% names(small_model_space))
  expect_true("observations_num" %in% names(small_model_space))
  expect_true("df" %in% names(small_model_space))
  expect_true("is_nested" %in% names(small_model_space))

  # Check types
  expect_true(is.matrix(small_model_space$params))
  expect_true(is.matrix(small_model_space$stats))
  expect_type(small_model_space$reg_names, "character")
  expect_type(small_model_space$observations_num, "integer")
  expect_s3_class(small_model_space$df, "data.frame")
  expect_type(small_model_space$is_nested, "logical")
})

test_that("print.badp_model_space produces expected output", {
  expect_output(print(small_model_space), "Model Space Object")
  expect_output(print(small_model_space), "Number of models:")
  expect_output(print(small_model_space), "Number of parameters in full parameter vector:")
  expect_output(print(small_model_space), "Variables:")
  expect_output(print(small_model_space), "Observations:")
  expect_output(print(small_model_space), "Nested:")
  expect_output(print(small_model_space), "Use with bma")
})

test_that("model_space works with bma function", {
  # Ensure model_space objects can be used with bma
  expect_no_error(bma(small_model_space, round = 3, dilution = 0))
  expect_no_error(bma(full_model_space, round = 3, dilution = 0))
})

test_that("numeric dimensions are correct", {
  # Number of parameters should match number of regressors
  expect_equal(ncol(small_model_space$params), ncol(small_model_space$stats))

  # reg_names length should match what's expected
  expect_true(length(small_model_space$reg_names) > 0)

  # observations_num should be positive
  expect_true(small_model_space$observations_num > 0)
})
