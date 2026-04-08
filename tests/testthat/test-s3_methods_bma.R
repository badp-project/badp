test_that("badp_bma class is properly assigned", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  expect_s3_class(bma_results, "badp_bma")
})

test_that("numeric indexing still works after adding S3 class (backward compatibility)", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Test all 16 components are accessible by numeric index
  expect_equal(length(bma_results), 16)
  expect_true(is.matrix(bma_results[[1]]))  # uniform_table
  expect_true(is.matrix(bma_results[[2]]))  # random_table
  expect_equal(length(bma_results[[3]]), bma_results[[4]] + 1)  # reg_names length = R + 1
  expect_true(is.numeric(bma_results[[4]]))  # R
  expect_true(is.numeric(bma_results[[5]]))  # num_of_models
  expect_true(is.matrix(bma_results[[6]]))  # jointness_data
  expect_true(is.matrix(bma_results[[7]]))  # best_models_data
  expect_true(is.numeric(bma_results[[8]]))  # EMS
  expect_true(is.matrix(bma_results[[9]]))  # size_priors
  expect_true(is.matrix(bma_results[[10]])) # PMPs
  expect_true(is.matrix(bma_results[[11]])) # model_priors
  expect_true(is.numeric(bma_results[[12]])) # dilution
  expect_true(is.matrix(bma_results[[13]])) # alphas
  expect_true(is.matrix(bma_results[[14]])) # betas_nonzero
  expect_true(is.matrix(bma_results[[15]])) # df_free
  expect_true(is.matrix(bma_results[[16]])) # PMS_table
})

test_that("named access works for bma components", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Test named access matches numeric access
  expect_equal(bma_results$uniform_table, bma_results[[1]])
  expect_equal(bma_results$random_table, bma_results[[2]])
  expect_equal(bma_results$reg_names, bma_results[[3]])
  expect_equal(bma_results$R, bma_results[[4]])
  expect_equal(bma_results$num_of_models, bma_results[[5]])
  expect_equal(bma_results$jointness_data, bma_results[[6]])
  expect_equal(bma_results$best_models_data, bma_results[[7]])
  expect_equal(bma_results$EMS, bma_results[[8]])
  expect_equal(bma_results$size_priors, bma_results[[9]])
  expect_equal(bma_results$PMPs, bma_results[[10]])
  expect_equal(bma_results$model_priors, bma_results[[11]])
  expect_equal(bma_results$dilution, bma_results[[12]])
  expect_equal(bma_results$alphas, bma_results[[13]])
  expect_equal(bma_results$betas_nonzero, bma_results[[14]])
  expect_equal(bma_results$df_free, bma_results[[15]])
  expect_equal(bma_results$PMS_table, bma_results[[16]])
})

test_that("existing helper functions work with classed objects", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # All existing functions should work without error
  expect_no_error(best_models(bma_results, criterion = 1, best = 5))
  expect_no_error(jointness(bma_results))
  expect_no_error(model_pmp(bma_results))
  expect_no_error(model_sizes(bma_results))
  expect_no_error(coef_hist(bma_results))
  expect_no_error(posterior_dens(bma_results))
})

test_that("print.badp_bma produces expected output", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  expect_output(print(bma_results), "Bayesian Model Averaging Results")
  expect_output(print(bma_results), "Model Space:")
  expect_output(print(bma_results), "Regressors:")
  expect_output(print(bma_results), "Expected Model Size:")
  expect_output(print(bma_results), "Dilution Prior:")
  expect_output(print(bma_results), "Binomial Model Prior Results:")
  expect_output(print(bma_results), "Binomial-Beta Model Prior Results:")
  expect_output(print(bma_results), "Prior and Posterior Model Sizes:")
})

test_that("summary.badp_bma returns correct structure", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Test binomial prior summary
  summ <- summary(bma_results)
  expect_s3_class(summ, "summary.badp_bma")
  expect_true("model_space_size" %in% names(summ))
  expect_true("num_regressors" %in% names(summ))
  expect_true("expected_model_size" %in% names(summ))
  expect_true("dilution_applied" %in% names(summ))
  expect_true("prior_type" %in% names(summ))
  expect_true("results" %in% names(summ))
  expect_true("model_sizes" %in% names(summ))
  expect_true("reg_names" %in% names(summ))

  expect_equal(summ$prior_type, "binomial")
  expect_equal(summ$model_space_size, bma_results$num_of_models)
  expect_equal(summ$num_regressors, bma_results$R)

  # Test beta prior summary
  summ_beta <- summary(bma_results, prior = "beta")
  expect_equal(summ_beta$prior_type, "beta")
  expect_equal(summ_beta$results, bma_results$random_table)
})

test_that("print.summary.badp_bma produces expected output", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  summ <- summary(bma_results)

  expect_output(print(summ), "Bayesian Model Averaging Summary")
  expect_output(print(summ), "Model Space Information:")
  expect_output(print(summ), "Total models:")
  expect_output(print(summ), "Coefficient Estimates")
})

test_that("coef.badp_bma extracts coefficients without standard errors", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Test binomial prior
  coefs <- coef(bma_results)
  expect_true(is.numeric(coefs))
  expect_equal(length(coefs), length(bma_results$reg_names))
  expect_true(!is.null(names(coefs)))
  expect_equal(names(coefs), bma_results$reg_names)

  # Test beta prior
  coefs_beta <- coef(bma_results, prior = "beta")
  expect_true(is.numeric(coefs_beta))
  expect_equal(length(coefs_beta), length(bma_results$reg_names))
})

test_that("coef.badp_bma extracts coefficients with standard errors", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # With standard errors
  coefs_se <- coef(bma_results, se = TRUE)
  expect_s3_class(coefs_se, "data.frame")
  expect_true("estimate" %in% names(coefs_se))
  expect_true("std.error" %in% names(coefs_se))
  expect_true("PIP" %in% names(coefs_se))
  expect_equal(nrow(coefs_se), length(bma_results$reg_names))
  expect_equal(rownames(coefs_se), bma_results$reg_names)

  # Test conditional mean with SE
  coefs_cond <- coef(bma_results, type = "conditional_mean", se = TRUE)
  expect_s3_class(coefs_cond, "data.frame")
  expect_true("estimate" %in% names(coefs_cond))
  expect_true("std.error" %in% names(coefs_cond))
})

test_that("coef.badp_bma type argument works correctly", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  coefs_pm <- coef(bma_results, type = "posterior_mean")
  coefs_cm <- coef(bma_results, type = "conditional_mean")

  expect_true(is.numeric(coefs_pm))
  expect_true(is.numeric(coefs_cm))
  expect_equal(length(coefs_pm), length(coefs_cm))

  # Values should be different for posterior_mean vs conditional_mean
  expect_false(all(coefs_pm == coefs_cm))
})

test_that("plot.badp_bma dispatches correctly", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Default plot (model_sizes)
  expect_no_error(plot(bma_results))

  # Test all plot types
  expect_no_error(plot(bma_results, which = "model_sizes"))
  expect_no_error(plot(bma_results, which = "best_models", criterion = 1, best = 5))
  expect_no_error(plot(bma_results, which = "jointness"))
  expect_no_error(plot(bma_results, which = "coef_hist"))
  expect_no_error(plot(bma_results, which = "posterior_dens"))
  expect_no_error(plot(bma_results, which = "model_pmp"))
})

test_that("plot.badp_bma returns appropriate objects", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # model_sizes returns a list
  p1 <- plot(bma_results, which = "model_sizes")
  expect_type(p1, "list")

  # coef_hist returns a list with ggplot objects
  p2 <- plot(bma_results, which = "coef_hist")
  expect_type(p2, "list")
})

test_that("invalid arguments produce errors", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Invalid prior
  expect_error(summary(bma_results, prior = "invalid"))
  expect_error(coef(bma_results, prior = "invalid"))

  # Invalid type
  expect_error(coef(bma_results, type = "invalid"))

  # Invalid which
  expect_error(plot(bma_results, which = "invalid"))
})
