test_that("coef on a model space labels every parameter", {

  ms <- migration_model_space
  params <- coef(ms)

  expect_true(is.matrix(params))
  expect_equal(dim(params), dim(ms$params))
  expect_equal(unname(params), unname(ms$params))
  expect_false(any(rownames(params) == ""))
  expect_equal(anyDuplicated(rownames(params)), 0L)
  expect_equal(colnames(params), paste0("model_", seq_len(n_models(ms))))

  # the named rows are kept as they are
  named <- rownames(ms$params) != ""
  expect_equal(rownames(params)[named], rownames(ms$params)[named])
  expect_true(all(c("alpha", "beta_Earn", "phis_1", "psis_1") %in%
                    rownames(params)))
})


test_that("coef on a model space returns a single model", {

  ms <- small_model_space
  full <- n_models(ms)
  one <- coef(ms, model = full)

  expect_true(is.numeric(one))
  expect_false(anyNA(one))
  expect_equal(length(one), sum(!is.na(ms$params[, full])))
  expect_equal(one[["alpha"]], unname(ms$params["alpha", full]))
  expect_equal(length(one), attr(logLik(ms, model = full), "df"))

  # the model without regressors has no beta
  expect_false(any(grepl("^beta_", names(coef(ms, model = 1)))))

  expect_error(coef(ms, model = 0), "single integer")
  expect_error(coef(ms, model = "1"), "single integer")
})


test_that("model_stats labels the rows of the statistics", {

  ms <- migration_model_space
  st <- model_stats(ms)
  vars <- regressors(ms, include_lagged = TRUE)

  expect_equal(unname(st), unname(ms$stats))
  expect_equal(
    rownames(st),
    c("loglik", "marg_lik", paste0("se_", vars), paste0("robust_se_", vars),
      "trace_HinvJ", "n_params", "rank_J")
  )
  expect_equal(colnames(st), colnames(coef(ms)))

  expect_equal(unname(st["loglik", ]), model_table(ms)$loglik)
  expect_equal(unname(st["n_params", ]),
               unname(colSums(!is.na(ms$params))))
})


test_that("model_stats copes with model spaces without the rank rows", {

  ms <- small_model_space
  K <- length(ms$reg_names)
  ms$stats <- ms$stats[seq_len(2 + 2 * K), ]
  st <- model_stats(ms)
  expect_equal(nrow(st), 2 + 2 * K)
  expect_equal(tail(rownames(st), 1),
               paste0("robust_se_", tail(as.character(ms$reg_names), 1)))
})
