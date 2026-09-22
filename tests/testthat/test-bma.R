test_that("bma computes the badp_bma object and all its components", {

  data_prepared <- badp::economic_growth[,1:6] %>%
    badp::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    badp::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  bma_results <- bma(small_model_space, round= 3, dilution = 0)

  expect_equal(length(bma_results), 18)
  expect_identical(
    names(bma_results),
    c("uniform_table", "random_table", "reg_names", "R", "num_of_models",
      "jointness_data", "best_models_data", "EMS", "size_priors", "PMPs",
      "model_priors", "dilution", "alphas", "betas_nonzero", "PMS_table",
      "omega", "weighting", "eta")
  )

  R <- bma_results$R
  M <- bma_results$num_of_models

  expect_true(is.numeric(R))
  expect_true(is.numeric(M))
  expect_equal(length(bma_results$reg_names), R + 1)

  expect_equal(dim(bma_results$uniform_table), c(R + 1, 8))
  expect_equal(dim(bma_results$random_table), c(R + 1, 8))
  expect_equal(dim(bma_results$jointness_data), c(M, R + 2))
  expect_equal(dim(bma_results$best_models_data), c(M, R + 2 + 3 * (R + 1)))
  expect_true(is.numeric(bma_results$EMS))
  expect_equal(dim(bma_results$size_priors), c(R + 1, 2))
  expect_equal(dim(bma_results$PMPs), c(M, R + 2))
  expect_equal(dim(bma_results$model_priors), c(M, 2))
  expect_true(is.numeric(bma_results$dilution))
  expect_equal(dim(bma_results$alphas), c(M, 1))
  expect_equal(dim(bma_results$betas_nonzero), c(M / 2, R))
  expect_equal(dim(bma_results$PMS_table), c(2, 2))
  expect_true(is.numeric(bma_results$omega))
  expect_identical(bma_results$weighting, "mb2016")
  expect_true(is.numeric(bma_results$eta))
})


test_that("the weighting argument selects the marginal-likelihood approximation", {
  ms <- badp::small_model_space

  res_default <- bma(ms, round = 6)
  res_mb2016  <- bma(ms, round = 6, weighting = "mb2016")
  res_mb2012  <- bma(ms, round = 6, weighting = "mb2012")
  res_uip     <- bma(ms, round = 6, weighting = "uip")
  res_nt      <- bma(ms, round = 6, weighting = "nt")

  # the default is the reference implementation
  expect_identical(res_default$weighting, "mb2016")
  expect_equal(res_default$uniform_table, res_mb2016$uniform_table)

  # the choice is recorded
  expect_identical(res_mb2012$weighting, "mb2012")
  expect_identical(res_uip$weighting, "uip")

  # posterior model probabilities remain a probability distribution throughout
  for (res in list(res_mb2016, res_mb2012, res_uip, res_nt)) {
    R <- res$R
    for (col in c(R + 1, R + 2)) {
      pmp <- res$PMPs[, col]
      expect_true(all(is.finite(pmp)))
      expect_true(all(pmp >= 0))
      expect_equal(sum(pmp), 1, tolerance = 1e-8)
    }
    expect_true(all(res$uniform_table[-1, "PIP"] >= 0))
    expect_true(all(res$uniform_table[-1, "PIP"] <= 1))
  }

  # mb2012 undoes the 1/N scaling, so it must concentrate at least as sharply
  R <- res_mb2016$R
  conc <- function(res) sum(res$PMPs[, R + 1]^2)   # Herfindahl over models
  expect_gte(conc(res_mb2012), conc(res_mb2016))

  # the three are genuinely different unless the model space is degenerate
  expect_false(isTRUE(all.equal(res_mb2016$PMPs[, R + 1],
                                res_mb2012$PMPs[, R + 1])))

  # learning rates are recorded and ordered as the algebra requires
  expect_equal(res_mb2012$eta, 1)
  expect_lt(res_nt$eta, res_mb2016$eta)     # 1/(NT) < 1/N
  expect_true(is.na(res_uip$eta))           # uip changes the penalty, not the rate

  # stronger tempering must give a less concentrated posterior
  conc_nt <- sum(res_nt$PMPs[, R + 1]^2)
  expect_lte(conc_nt, conc(res_mb2016))

  expect_error(bma(ms, weighting = "nonsense"), "should be one of")
})
