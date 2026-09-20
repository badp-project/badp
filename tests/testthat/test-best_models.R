test_that("best_models returns a list of per-model objects", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  best <- 5

  best_5_models <- best_models(bma_results, prior = "beta", best = best)

  # container
  expect_s3_class(best_5_models, "badp_best_models")
  expect_equal(class(best_5_models), c("badp_best_models", "list"))
  expect_equal(length(best_5_models), best)
  expect_equal(attr(best_5_models, "prior"), "beta")
  expect_equal(attr(best_5_models, "n_models"), bma_results$num_of_models)
  expect_equal(attr(best_5_models, "digits"), 3)

  # each element is a single model
  for (m in best_5_models) expect_s3_class(m, "badp_model")

  one <- best_5_models[[1]]
  expect_equal(one$rank, 1)
  expect_equal(one$prior, "beta")
  expect_true(is.numeric(one$pmp) && length(one$pmp) == 1)
  expect_true(is.logical(one$inclusion))
  expect_true(all(one$included %in% names(one$inclusion)))
  expect_equal(one$included, names(one$inclusion)[one$inclusion])

  # the parameter vectors are named and of equal length
  K <- length(attr(best_5_models, "reg_names"))
  expect_equal(length(one$coefficients), K)
  expect_equal(length(one$se), K)
  expect_equal(length(one$robust_se), K)
  expect_equal(names(one$coefficients), attr(best_5_models, "reg_names"))
  expect_equal(names(one$se), names(one$coefficients))
  expect_equal(names(one$robust_se), names(one$coefficients))

  # excluded parameters are NA, not zero
  expect_true(all(is.na(one$coefficients) | one$coefficients != 0))
})


test_that("the models are ordered by decreasing posterior model probability", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  best_models_obj <- best_models(bma_results, best = 5)

  pmps <- vapply(best_models_obj, function(m) m$pmp, numeric(1))
  expect_false(is.unsorted(rev(pmps)))
  expect_equal(vapply(best_models_obj, function(m) m$rank, numeric(1)),
               setNames(as.numeric(1:5), names(best_models_obj)))
})


test_that("best is capped at the size of the model space", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  M <- bma_results$num_of_models

  expect_message(too_many <- best_models(bma_results, best = M + 10))
  expect_equal(length(too_many), M)
})


test_that("coef method extracts estimates and standard errors", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  one <- best_models(bma_results, best = 3)[[1]]

  estimates <- coef(one)
  expect_true(is.numeric(estimates))
  expect_false(anyNA(estimates))
  expect_true(length(estimates) <= length(one$coefficients))

  with_absent <- coef(one, include_absent = TRUE)
  expect_equal(length(with_absent), length(one$coefficients))

  tab <- coef(one, se = TRUE)
  expect_true(is.matrix(tab))
  expect_equal(ncol(tab), 5)
  expect_equal(nrow(tab), length(estimates))
  expect_equal(colnames(tab)[1], "Estimate")

  # rounding is opt-in and does not otherwise change the result
  expect_equal(coef(one, se = TRUE, digits = 3), round(tab, 3))
  expect_equal(coef(one, digits = 3), round(estimates, 3))
  expect_false(identical(coef(one, se = TRUE, digits = 3), tab))
})


test_that("subsetting preserves the class and the attributes", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  best_models_obj <- best_models(bma_results, best = 5)

  subset <- best_models_obj[1:2]
  expect_s3_class(subset, "badp_best_models")
  expect_equal(length(subset), 2)
  expect_equal(attr(subset, "prior"), attr(best_models_obj, "prior"))
  expect_equal(attr(subset, "reg_names"), attr(best_models_obj, "reg_names"))
  expect_equal(attr(subset, "n_models"), attr(best_models_obj, "n_models"))

  expect_s3_class(best_models_obj[[2]], "badp_model")
})


test_that("print methods write to the console and return invisibly", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  best_models_obj <- best_models(bma_results, best = 3)

  expect_output(print(best_models_obj), "Best 3 of")
  expect_output(print(best_models_obj), "PMP")
  expect_invisible(print(best_models_obj))

  expect_output(print(best_models_obj[[1]]), "Model No. 1")
  expect_output(print(best_models_obj[[1]]), "Estimate")
  expect_invisible(print(best_models_obj[[1]]))

  # robust standard errors are selectable at print time
  expect_output(print(best_models_obj[[1]], robust = TRUE), "robust")
})


test_that("summary methods return their own classed objects", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  best_models_obj <- best_models(bma_results, best = 4)

  s <- summary(best_models_obj)
  expect_s3_class(s, "summary.badp_best_models")
  expect_equal(s$n_best, 4)
  expect_equal(s$prior, "binomial")
  expect_false(s$robust)
  expect_true(is.matrix(s$inclusion))
  expect_true(is.matrix(s$estimates))
  expect_equal(ncol(s$inclusion), 4)
  expect_equal(ncol(s$estimates), 4)
  expect_equal(rownames(s$inclusion),
               c(attr(best_models_obj, "reg_names"), "PMP"))
  expect_output(print(s), "Best Models Summary")

  s_robust <- summary(best_models_obj, robust = TRUE)
  expect_true(s_robust$robust)
  expect_false(identical(s$estimates, s_robust$estimates))

  sm <- summary(best_models_obj[[1]])
  expect_s3_class(sm, "summary.badp_model")
  expect_equal(sm$rank, 1)
  expect_true(is.matrix(sm$coefficients))
  expect_output(print(sm), "Model No. 1")
})


test_that("summary and print of a badp_best_models object differ", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  best_models_obj <- best_models(bma_results, best = 3)

  plain <- capture.output(print(best_models_obj))
  detailed <- capture.output(print(summary(best_models_obj)))
  expect_false(identical(plain, detailed))
})


test_that("plot method draws without error and returns the table invisibly", {

  bma_results <- bma(small_model_space, round = 3, dilution = 0)
  best_models_obj <- best_models(bma_results, best = 3)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_invisible(plot(best_models_obj))
  expect_silent(tab <- plot(best_models_obj, which = "inclusion"))
  expect_true(is.matrix(tab))
  expect_equal(ncol(tab), 3)

  expect_error(plot(best_models_obj, which = "nonsense"))
})


test_that("the methods expected by the JSS review are registered", {

  for (generic in c("print", "summary", "plot")) {
    expect_true(
      any(grepl(paste0("^", generic, "\\.badp_best_models$"),
                as.character(methods(class = "badp_best_models"))))
    )
  }

  for (generic in c("print", "summary", "coef")) {
    expect_true(
      any(grepl(paste0("^", generic, "\\.badp_model$"),
                as.character(methods(class = "badp_model"))))
    )
  }
})


test_that("a coefficient estimated at exactly zero is kept in a model", {

  # Absent parameters are stored as zero; which ones are absent is decided by
  # the model's inclusion vector, so an estimate that happens to be exactly
  # zero must survive as an estimate rather than become NA.
  results <- bma(small_model_space, round = 3, dilution = 0)

  zeroed <- results
  best_row <- which.max(zeroed$best_models_data[, ncol(zeroed$best_models_data) - 1])
  zeroed$best_models_data[best_row, "ish_coef"] <- 0

  model <- best_models(zeroed, best = 1)[[1]]

  expect_true(model$inclusion[["ish"]])
  expect_false(is.na(model$coefficients[["ish"]]))
  expect_equal(unname(model$coefficients[["ish"]]), 0)
  expect_equal(names(model$coefficients)[is.na(model$coefficients)],
               names(model$inclusion)[!model$inclusion])
})
