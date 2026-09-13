test_that("badp_bma class is properly assigned", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  expect_s3_class(bma_results, "badp_bma")
})

test_that("the components of a badp_bma object are reachable by name", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  expect_equal(length(bma_results), 18)
  expect_true(is.matrix(bma_results$uniform_table))
  expect_true(is.matrix(bma_results$random_table))
  expect_equal(length(bma_results$reg_names), bma_results$R + 1)
  expect_true(is.numeric(bma_results$R))
  expect_true(is.numeric(bma_results$num_of_models))
  expect_true(is.matrix(bma_results$jointness_data))
  expect_true(is.matrix(bma_results$best_models_data))
  expect_true(is.numeric(bma_results$EMS))
  expect_true(is.matrix(bma_results$size_priors))
  expect_true(is.matrix(bma_results$PMPs))
  expect_true(is.matrix(bma_results$model_priors))
  expect_true(is.numeric(bma_results$dilution))
  expect_true(is.matrix(bma_results$alphas))
  expect_true(is.matrix(bma_results$betas_nonzero))
  expect_true(is.matrix(bma_results$PMS_table))
  expect_true(is.numeric(bma_results$omega))
  expect_true(is.character(bma_results$weighting))
  expect_true(is.numeric(bma_results$eta))
})

test_that("bma components are reached by name", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # The object is addressed by name throughout the package, the accessors and
  # the manuscript. Pinning the names rather than the positions is what
  # protects callers: adding or removing a component must never silently
  # change what an existing name refers to.
  expect_equal(
    names(bma_results),
    c("uniform_table", "random_table", "reg_names", "R", "num_of_models",
      "jointness_data", "best_models_data", "EMS", "size_priors", "PMPs",
      "model_priors", "dilution", "alphas", "betas_nonzero", "PMS_table",
      "omega", "weighting", "eta")
  )
  expect_equal(anyDuplicated(names(bma_results)), 0L)
  expect_equal(bma_results$PMS_table, bma_results[["PMS_table"]])

  # df_free was removed in 0.7.0
  expect_null(bma_results$df_free)
})

test_that("existing helper functions work with classed objects", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # All existing functions should work without error
  expect_no_error(best_models(bma_results, prior = "binomial", best = 5))
  expect_no_error(jointness(bma_results))
  expect_no_error(model_pmp(bma_results))
  expect_no_error(model_sizes(bma_results))
  expect_no_error(coef_hist(bma_results))
  expect_no_error(posterior_dens(bma_results))
})

test_that("print.badp_bma gives a compact overview", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  expect_output(print(bma_results),
                "Bayesian model averaging for dynamic panels")
  expect_output(print(bma_results), "Model space:")
  expect_output(print(bma_results), "Expected model size:")
  expect_output(print(bma_results), "Model priors:")
  expect_output(print(bma_results), "Weighting:")
  expect_output(print(bma_results), "posterior inclusion probability")
  expect_output(print(bma_results), "enters every model by construction")
  expect_invisible(print(bma_results))

  # the lagged dependent variable is listed first, not sorted to the bottom
  # by its missing inclusion probability
  out <- capture.output(print(bma_results))
  table_start <- grep("^ *PIP", out)[1]
  first_row <- out[table_start + 1]
  expect_true(startsWith(first_row, bma_results$reg_names[1]))
})

test_that("print and summary of a badp_bma object differ", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  out_print <- capture.output(print(bma_results))
  out_summary <- capture.output(print(summary(bma_results)))

  expect_false(identical(out_print, out_summary))

  # summary says strictly more than print
  expect_true(length(out_summary) > length(out_print))
  expect_true(any(grepl("BMA statistics", out_summary)))
  expect_false(any(grepl("BMA statistics", out_print)))
})

test_that("summary.badp_bma returns correct structure", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  summ <- summary(bma_results)
  expect_s3_class(summ, "summary.badp_bma")
  expect_true("model_space_size" %in% names(summ))
  expect_true("num_regressors" %in% names(summ))
  expect_true("expected_model_size" %in% names(summ))
  expect_true("dilution_applied" %in% names(summ))
  expect_true("results_binomial" %in% names(summ))
  expect_true("results_beta" %in% names(summ))
  expect_true("model_sizes" %in% names(summ))
  expect_true("reg_names" %in% names(summ))

  expect_equal(summ$model_space_size, bma_results$num_of_models)
  expect_equal(summ$num_regressors, bma_results$R)
  expect_equal(summ$results_binomial, bma_results$uniform_table)
  expect_equal(summ$results_beta, bma_results$random_table)
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

test_that("print.badp_bma validates n before it reaches seq_len()", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  expect_error(print(bma_results, n = -1), "non-negative")
  expect_error(print(bma_results, n = NA), "non-negative")
  expect_error(print(bma_results, n = c(1, 2)), "non-negative")
  expect_error(print(bma_results, n = "5"), "non-negative")

  # 0 lists no regressors and Inf lists all of them; both still print the
  # overview and the line about the lagged dependent variable
  expect_output(print(bma_results, n = 0), "Bayesian model averaging")
  expect_output(print(bma_results, n = 0), "enters every model")
  expect_output(print(bma_results, n = Inf), "Bayesian model averaging")
})

test_that("plot.badp_bma dispatches correctly", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # Default plot (model_sizes)
  expect_no_error(plot(bma_results))

  # Test all plot types
  expect_no_error(plot(bma_results, which = "model_sizes"))
  expect_no_error(plot(bma_results, which = "best_models", prior = "binomial", best = 5))
  expect_no_error(plot(bma_results, which = "jointness"))
  expect_no_error(plot(bma_results, which = "coef_hist"))
  expect_no_error(plot(bma_results, which = "posterior_dens"))
  expect_no_error(plot(bma_results, which = "model_pmp"))
})

test_that("plot.badp_bma routes arguments to the function that declares them", {
  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # `robust` belongs to plot.badp_best_models() and `best` to best_models();
  # the whole of `...` used to go to best_models(), which has no `robust`
  expect_no_error(plot(bma_results, which = "best_models", robust = TRUE))
  expect_no_error(plot(bma_results, which = "best_models", best = 3,
                       robust = TRUE))

  # the selection arguments still reach best_models()
  out <- plot(bma_results, which = "best_models", best = 2)
  expect_s3_class(out, "badp_best_models")
  expect_length(out, 2)
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

  # Invalid prior (only coef has prior argument; summary always shows both)
  expect_error(coef(bma_results, prior = "invalid"))

  # Non-logical flags
  expect_error(coef(bma_results, conditional = "yes"))
  expect_error(coef(bma_results, se = "yes"))
  expect_error(coef(bma_results, robustSE = "yes"))
  expect_error(coef(bma_results, PIP = "yes"))

  # Invalid which
  expect_error(plot(bma_results, which = "invalid"))
})

