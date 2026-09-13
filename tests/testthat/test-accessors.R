test_that("bma_table returns the statistics under each model prior", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  binom <- bma_table(bma_results)
  beta <- bma_table(bma_results, prior = "beta")

  expect_true(is.matrix(binom))
  expect_true(is.matrix(beta))
  expect_equal(binom, bma_results$uniform_table)
  expect_equal(beta, bma_results$random_table)
  expect_false(identical(binom, beta))

  expect_equal(ncol(binom), 8)
  expect_equal(nrow(binom), bma_results$R + 1)
  expect_true(all(c("PIP", "PM", "PSD", "PSDR") %in% colnames(binom)))

  expect_error(bma_table(bma_results, prior = "nonsense"))
})


test_that("pip returns inclusion probabilities without the lagged term", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  p <- pip(bma_results)
  expect_true(is.numeric(p))
  expect_equal(length(p), bma_results$R)
  expect_false(anyNA(p))
  expect_true(all(p >= 0 & p <= 1))
  expect_equal(names(p), regressors(bma_results))

  # the lagged dependent variable has no inclusion probability
  with_lag <- pip(bma_results, include_lagged = TRUE)
  expect_equal(length(with_lag), bma_results$R + 1)
  expect_equal(names(with_lag)[1], bma_results$reg_names[1])

  # both priors are reachable and differ
  expect_false(identical(pip(bma_results), pip(bma_results, prior = "beta")))
})


test_that("pmp returns posterior model probabilities", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  probs <- pmp(bma_results)
  expect_true(is.numeric(probs))
  expect_equal(length(probs), n_models(bma_results))
  expect_true(all(probs >= 0))
  expect_equal(sum(probs), 1, tolerance = 1e-6)

  top3 <- pmp(bma_results, top = 3)
  expect_equal(length(top3), 3)
  expect_false(is.unsorted(rev(top3)))
  expect_equal(max(probs), top3[1])

  expect_message(all_of_them <- pmp(bma_results, top = 1000))
  expect_equal(length(all_of_them), n_models(bma_results))
})


test_that("model_size_table returns the prior and posterior model sizes", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  tab <- model_size_table(bma_results)
  expect_true(is.matrix(tab))
  expect_equal(dim(tab), c(2, 2))
  expect_equal(tab, bma_results$PMS_table)
})


test_that("regressors and n_models work on both classed objects", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  expect_true(is.character(regressors(bma_results)))
  expect_equal(length(regressors(bma_results)), bma_results$R)
  expect_equal(length(regressors(bma_results, include_lagged = TRUE)),
               bma_results$R + 1)

  expect_equal(regressors(small_model_space, include_lagged = TRUE),
               as.character(small_model_space$reg_names))
  expect_equal(regressors(small_model_space),
               as.character(small_model_space$reg_names)[-1])

  expect_equal(n_models(bma_results), as.integer(bma_results$num_of_models))
  expect_equal(n_models(small_model_space), ncol(small_model_space$params))
  expect_equal(n_models(bma_results), n_models(small_model_space))
})


test_that("weighting and learning_rate report the marginal-likelihood choice", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  expect_identical(weighting(bma_results), "mb2016")
  expect_true(is.numeric(learning_rate(bma_results)))

  mb2012 <- bma(small_model_space, round = 3, weighting = "mb2012")
  expect_identical(weighting(mb2012), "mb2012")
  expect_equal(learning_rate(mb2012), 1)

  # "uip" alters the penalty rather than the rate, so it has no learning rate
  uip <- bma(small_model_space, round = 3, weighting = "uip")
  expect_identical(weighting(uip), "uip")
  expect_true(is.na(learning_rate(uip)))
})


test_that("convergence returns the per-model diagnostics", {

  diagnostics <- convergence(small_model_space)

  expect_true(is.matrix(diagnostics))
  expect_equal(ncol(diagnostics), n_models(small_model_space))
  expect_equal(diagnostics, small_model_space$convergence)
})


test_that("the accessors are registered as methods, not bare functions", {

  registered <- as.character(methods(class = "badp_bma"))

  for (generic in c("bma_table", "pip", "pmp", "model_size_table",
                    "regressors", "n_models", "weighting", "learning_rate")) {
    expect_true(
      any(grepl(paste0("^", generic, "\\.badp_bma$"), registered)),
      info = paste(generic, "is not registered for badp_bma")
    )
  }

  space_methods <- as.character(methods(class = "badp_model_space"))
  for (generic in c("regressors", "n_models", "convergence")) {
    expect_true(
      any(grepl(paste0("^", generic, "\\.badp_model_space$"), space_methods)),
      info = paste(generic, "is not registered for badp_model_space")
    )
  }
})


test_that("the accessors reproduce what direct component access gave", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)

  # every value the manuscript used to reach by name is now reachable
  # through an accessor
  expect_equal(bma_table(bma_results, "binomial"), bma_results$uniform_table)
  expect_equal(bma_table(bma_results, "beta"), bma_results$random_table)
  expect_equal(model_size_table(bma_results), bma_results$PMS_table)
  expect_equal(unname(pip(bma_results, include_lagged = TRUE)),
               unname(bma_results$uniform_table[, "PIP"]))
})
