test_that("model_table describes every model of the model space", {

  tab <- model_table(small_model_space)

  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), n_models(small_model_space))
  expect_equal(tab$model, seq_len(n_models(small_model_space)))
  expect_equal(tab$loglik, as.numeric(small_model_space$stats[1, ]))
  expect_true(all(c("model", "size", "regressors", "loglik", "converged") %in%
                    names(tab)))

  # the model space is complete: every size from 0 to R appears
  R <- length(regressors(small_model_space))
  expect_equal(as.integer(table(tab$size)), choose(R, 0:R))
  expect_equal(tab$regressors[tab$size == 0], "(none)")
  expect_equal(tab$regressors[tab$size == R],
               paste(regressors(small_model_space), collapse = ", "))
})


test_that("model_table size agrees with the parameter matrix", {

  params <- migration_model_space$params
  betas <- grep("^beta_", rownames(params))
  n_reg <- colSums(!is.na(params[betas, ]))

  expect_equal(model_table(migration_model_space)$size, unname(n_reg))
})


test_that("model_table sorts and truncates", {

  by_lik <- model_table(small_model_space, sort_by = "loglik")
  expect_false(is.unsorted(rev(by_lik$loglik)))
  expect_equal(by_lik$model[1], which.max(small_model_space$stats[1, ]))
  expect_equal(rownames(by_lik), as.character(seq_len(nrow(by_lik))))

  by_size <- model_table(small_model_space, sort_by = "size")
  expect_false(is.unsorted(by_size$size))

  top3 <- model_table(small_model_space, sort_by = "loglik", top = 3)
  expect_equal(nrow(top3), 3)
  expect_equal(top3, by_lik[1:3, ])

  expect_error(model_table(small_model_space, sort_by = "nonsense"))
})


test_that("model_table works without convergence diagnostics", {

  ms <- small_model_space
  ms$convergence <- NULL
  expect_false("converged" %in% names(model_table(ms)))
})
