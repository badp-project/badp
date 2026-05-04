test_that("badp_bma class is properly assigned", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  expect_s3_class(bma_results, "badp_bma")
})

test_that("numeric indexing still works after adding S3 class (backward compatibility)", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Test all 17 components are accessible by numeric index
  expect_equal(length(bma_results), 17)
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
  expect_true(is.numeric(bma_results[[17]])) # dil.Par
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

  # print.badp_bma delegates to print(summary(x)), so the output should
  # match the summary printer.
  expect_output(print(bma_results), "Bayesian Model Averaging Summary")
  expect_output(print(bma_results), "Model Space Information:")
  expect_output(print(bma_results), "Total models:")
  expect_output(print(bma_results), "Number of regressors:")
  expect_output(print(bma_results), "Expected model size:")
  expect_output(print(bma_results), "Dilution prior:")
  expect_output(print(bma_results), "BMA statistics")
  expect_output(print(bma_results), "binomial prior")
  expect_output(print(bma_results), "binomial-beta prior")
  expect_output(print(bma_results), "Prior and Posterior Model Sizes:")
})

test_that("print.badp_bma matches print.summary.badp_bma output", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  out_print   <- utils::capture.output(print(bma_results))
  out_summary <- utils::capture.output(print(summary(bma_results)))
  expect_identical(out_print, out_summary)
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
  expect_true("results_binomial" %in% names(summ))
  expect_true("results_beta" %in% names(summ))
  expect_true("model_sizes" %in% names(summ))
  expect_true("reg_names" %in% names(summ))

  expect_equal(summ$prior_type, "binomial")
  expect_equal(summ$model_space_size, bma_results$num_of_models)
  expect_equal(summ$num_regressors, bma_results$R)
  expect_equal(summ$results_binomial, bma_results$uniform_table)
  expect_equal(summ$results_beta, bma_results$random_table)

  # Test beta prior summary
  summ_beta <- summary(bma_results, prior = "beta")
  expect_equal(summ_beta$prior_type, "beta")
  expect_equal(summ_beta$results, bma_results$random_table)
  expect_equal(summ_beta$results_binomial, bma_results$uniform_table)
  expect_equal(summ_beta$results_beta, bma_results$random_table)
})

test_that("print.summary.badp_bma produces expected output", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  summ <- summary(bma_results)

  expect_output(print(summ), "Bayesian Model Averaging Summary")
  expect_output(print(summ), "Model Space Information:")
  expect_output(print(summ), "Total models:")
  expect_output(print(summ), "BMA statistics")
  expect_output(print(summ), "binomial prior")
  expect_output(print(summ), "binomial-beta prior")
  expect_output(print(summ), "Model prior: binomial, binomial-beta")
})

test_that("coef.badp_bma default returns both priors with PIP", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  coefs <- coef(bma_results)
  expect_s3_class(coefs, "badp_bma_coef")
  expect_s3_class(coefs, "data.frame")
  expect_equal(nrow(coefs), length(bma_results$reg_names))
  expect_equal(rownames(coefs), bma_results$reg_names)

  # Column names mirror the bma summary table (PM, PIP); SE off by default.
  expect_true("binom_PM"  %in% colnames(coefs))
  expect_true("binom_PIP" %in% colnames(coefs))
  expect_true("beta_PM"   %in% colnames(coefs))
  expect_true("beta_PIP"  %in% colnames(coefs))
  expect_false(any(grepl("PSD", colnames(coefs))))

  expect_equal(coefs[["binom_PM"]],
               unname(bma_results$uniform_table[, "PM"]))
  expect_equal(coefs[["beta_PM"]],
               unname(bma_results$random_table[, "PM"]))
})

test_that("coef.badp_bma PIP toggle controls inclusion of PIP columns", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  no_pip <- coef(bma_results, PIP = FALSE)
  expect_s3_class(no_pip, "badp_bma_coef")
  expect_false("binom_PIP" %in% colnames(no_pip))
  expect_false("beta_PIP"  %in% colnames(no_pip))
  expect_true("binom_PM"   %in% colnames(no_pip))
  expect_true("beta_PM"    %in% colnames(no_pip))
})

test_that("coef.badp_bma single-prior options preserve legacy return shape", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Numeric vector requires PIP = FALSE (and se = FALSE)
  coefs_bin <- coef(bma_results, prior = "binomial", PIP = FALSE)
  expect_true(is.numeric(coefs_bin))
  expect_equal(length(coefs_bin), length(bma_results$reg_names))
  expect_equal(names(coefs_bin), bma_results$reg_names)

  coefs_beta <- coef(bma_results, prior = "beta", PIP = FALSE)
  expect_true(is.numeric(coefs_beta))
  expect_equal(length(coefs_beta), length(bma_results$reg_names))

  # Default single-prior call returns a data frame with PM and PIP columns
  coefs_bin_pip <- coef(bma_results, prior = "binomial")
  expect_s3_class(coefs_bin_pip, "data.frame")
  expect_true("PM"  %in% colnames(coefs_bin_pip))
  expect_true("PIP" %in% colnames(coefs_bin_pip))
})

test_that("coef.badp_bma se = TRUE adds standard error columns", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Both priors: PSD column is added
  coefs_se <- coef(bma_results, se = TRUE)
  expect_s3_class(coefs_se, "badp_bma_coef")
  for (col in c("binom_PM", "binom_PSD", "binom_PIP",
                "beta_PM",  "beta_PSD",  "beta_PIP")) {
    expect_true(col %in% colnames(coefs_se))
  }

  # Single prior
  coefs_se_bin <- coef(bma_results, prior = "binomial", se = TRUE)
  expect_s3_class(coefs_se_bin, "data.frame")
  expect_true(all(c("PM", "PSD", "PIP") %in% colnames(coefs_se_bin)))
})

test_that("coef.badp_bma robustSE switches between PSD and PSDR columns", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  normal <- coef(bma_results, se = TRUE, robustSE = FALSE)
  robust <- coef(bma_results, se = TRUE, robustSE = TRUE)

  # Column names switch depending on robustSE
  expect_true("binom_PSD"   %in% colnames(normal))
  expect_true("binom_PSDR"  %in% colnames(robust))
  expect_false("binom_PSDR" %in% colnames(normal))
  expect_false("binom_PSD"  %in% colnames(robust))

  # Values match the corresponding bma table columns
  expect_equal(normal[["binom_PSD"]],
               unname(bma_results$uniform_table[, "PSD"]))
  expect_equal(normal[["beta_PSD"]],
               unname(bma_results$random_table[, "PSD"]))
  expect_equal(robust[["binom_PSDR"]],
               unname(bma_results$uniform_table[, "PSDR"]))
  expect_equal(robust[["beta_PSDR"]],
               unname(bma_results$random_table[, "PSDR"]))

  # robustSE without se warns and is otherwise ignored
  expect_warning(coef(bma_results, se = FALSE, robustSE = TRUE),
                 "robustSE")
})

test_that("coef.badp_bma conditional switches to PMcon / PSDcon columns", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  uncond <- coef(bma_results, conditional = FALSE)
  cond   <- coef(bma_results, conditional = TRUE)

  expect_true("binom_PM"    %in% colnames(uncond))
  expect_true("binom_PMcon" %in% colnames(cond))
  expect_equal(uncond[["binom_PM"]],
               unname(bma_results$uniform_table[, "PM"]))
  expect_equal(cond[["binom_PMcon"]],
               unname(bma_results$uniform_table[, "PMcon"]))

  # conditional + se uses PSDcon
  cond_se <- coef(bma_results, conditional = TRUE, se = TRUE)
  expect_true("binom_PSDcon" %in% colnames(cond_se))
  expect_equal(cond_se[["binom_PSDcon"]],
               unname(bma_results$uniform_table[, "PSDcon"]))

  # conditional + robustSE uses PSDRcon
  cond_robust <- coef(bma_results, conditional = TRUE, se = TRUE,
                      robustSE = TRUE)
  expect_true("binom_PSDRcon" %in% colnames(cond_robust))
  expect_equal(cond_robust[["binom_PSDRcon"]],
               unname(bma_results$uniform_table[, "PSDRcon"]))
})

test_that("print.badp_bma_coef adapts to the requested view", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Default (PIP, no SE): two-panel form mentioning PIP
  expect_output(print(coef(bma_results)), "Posterior mean with PIP")
  expect_output(print(coef(bma_results)), "Binomial prior:")
  expect_output(print(coef(bma_results)), "Binomial-beta prior:")
  expect_output(print(coef(bma_results)), "PM")
  expect_output(print(coef(bma_results)), "PIP")

  # Estimates only: side-by-side form
  expect_output(print(coef(bma_results, PIP = FALSE)),
                "Posterior mean \\(both priors\\)")
  expect_output(print(coef(bma_results, PIP = FALSE)), "binomial-beta")

  # SE: panels include PSD
  out_se <- coef(bma_results, se = TRUE)
  expect_output(print(out_se), "Posterior mean with std\\. errors and PIP")
  expect_output(print(out_se), "PSD")

  # Robust SE: panels include PSDR and the header mentions robust
  out_rob <- coef(bma_results, se = TRUE, robustSE = TRUE)
  expect_output(print(out_rob), "robust std\\. errors")
  expect_output(print(out_rob), "PSDR")

  # Conditional: header and column name reflect this
  expect_output(print(coef(bma_results, conditional = TRUE)),
                "Conditional posterior mean")
  expect_output(print(coef(bma_results, conditional = TRUE)),
                "PMcon")
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

  # Non-logical flags
  expect_error(coef(bma_results, conditional = "yes"))
  expect_error(coef(bma_results, se = "yes"))
  expect_error(coef(bma_results, robustSE = "yes"))
  expect_error(coef(bma_results, PIP = "yes"))

  # Invalid which
  expect_error(plot(bma_results, which = "invalid"))
})

