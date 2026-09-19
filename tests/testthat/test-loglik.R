test_that("logLik on a model space returns a logLik object", {

  ms <- small_model_space
  full <- n_models(ms)
  ll <- logLik(ms, model = full)

  expect_s3_class(ll, "logLik")
  expect_equal(as.numeric(ll), as.numeric(ms$stats[1, full]))
  expect_equal(attr(ll, "df"), sum(!is.na(ms$params[, full])))
  expect_equal(attr(ll, "nobs"), ms$observations_num)

  # the stats generics apply
  expect_equal(AIC(ll), -2 * as.numeric(ll) + 2 * attr(ll, "df"))
  expect_equal(BIC(ll),
               -2 * as.numeric(ll) + log(attr(ll, "nobs")) * attr(ll, "df"))

  # agrees with model_table
  tab <- model_table(ms)
  expect_equal(
    vapply(tab$model, function(j) as.numeric(logLik(ms, model = j)), 1),
    tab$loglik
  )
})


test_that("logLik on a model space validates the model", {
  expect_error(logLik(small_model_space), "Specify the model")
  expect_error(logLik(small_model_space, model = 0), "single integer")
  expect_error(logLik(small_model_space, model = 1.5), "single integer")
  expect_error(logLik(small_model_space, model = 1:2), "single integer")
  expect_error(logLik(small_model_space,
                      model = n_models(small_model_space) + 1),
               "single integer")
})


test_that("logLik on a best model matches its column of the model space", {

  results <- bma(small_model_space, round = 3, dilution = 0)
  best <- best_models(results, best = 3)

  for (m in best) {
    expect_s3_class(logLik(m), "logLik")
    expect_equal(logLik(m), logLik(small_model_space, model = m$model))
    # the stored position points at the model with these regressors
    tab <- model_table(small_model_space)
    expect_equal(tab$regressors[m$model],
                 if (length(m$included)) paste(m$included, collapse = ", ")
                 else "(none)")
  }

  expect_output(print(best[[1]]), "Log-likelihood")
})


test_that("logLik on a best model from an old bma object fails clearly", {
  results <- bma(small_model_space, round = 3, dilution = 0)
  results$loglik <- NULL
  results$n_params <- NULL
  results$nobs <- NULL
  best <- best_models(results, best = 1)
  expect_error(logLik(best[[1]]), "recompute it with bma")
  expect_no_error(print(best[[1]]))
})
