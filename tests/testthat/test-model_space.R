compare_matrices <- function(actual, expected, tols = NULL) {
  if (is.null(tols)) {
    expect_equal(actual, expected)
  } else {
    identical(rownames(actual), rownames(expected))
    identical(colnames(actual), colnames(expected))

    if (ncol(actual) != length(tols)) {
      stop("#tols != #columns in actual")
    }

    expect_equal(is.na(actual), is.na(expected))

    within_tolerance <-
      sapply(
        1:length(tols),
        function(x) abs(actual[, x] - expected[, x]) < tols[x]
      )

    if (!all(within_tolerance, na.rm = TRUE)) {
      got   <- paste(capture.output(print(within_tolerance)), collapse = "\n")
      expct <- paste(capture.output(print(expected)),         collapse = "\n")
      act   <- paste(capture.output(print(actual)),           collapse = "\n")

      msg <- paste(
        "Discrepancies between matrices exceed given tolerances.",
        "- FALSE represents a mismatch",
        "- NA is ok (it was checked that they are in the same place)",
        "Discrepancies at:",
        got,
        "Expected:",
        expct,
        "Actual:",
        act,
        sep = "\n"
      )

      testthat::fail(msg)
    }
  }
}


test_that("optim_model_space_params correctly computes small_economic_growth_ms", {
  set.seed(23)

  data_prepared <- badp::economic_growth[,1:6] %>%
    badp::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    badp::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  params <- optim_model_space_params(
    df            = data_prepared,
    dep_var_col   = gdp,
    timestamp_col = year,
    entity_col    = country,
    init_value    = function(n) rep(0.5, n),
    nested        = TRUE
  )

  compare_matrices(params, small_model_space$params, tols = rep(0.01, 8))
})

non_zero_stats_mask_generator <- function(n_lin_features) {
  lin_features_mask <- t(rje::powerSetMat(n_lin_features))
  # One entry per model, i.e. one per column of lin_features_mask, so that
  # rbind() below does not have to recycle these rows.
  n_models <- ncol(lin_features_mask)
  ones <- rep(1, n_models)
  mask_where_nonzero <- rbind(
    ones, ones, ones,
    lin_features_mask,
    ones,
    lin_features_mask
  )
  zeros <- rep(0, n_models)
  mask_where_greater_than_zero <- rbind(
    zeros, zeros, zeros,
    lin_features_mask,
    zeros,
    lin_features_mask
  )
  list(
    nonzero = mask_where_nonzero,
    greater_than_zero = mask_where_greater_than_zero
  )
}

test_that(
  paste(
    "compute_model_space_stats computes correct likelihoods and standard",
    "deviations based on small_model_space"
    ),
  {
    set.seed(23)

    n_lin_features <- 3

    data_prepared <- badp::economic_growth[, 1:(3+n_lin_features)] %>%
      badp::feature_standardization(
        excluded_cols = c(country, year, gdp)
      ) %>%
      badp::feature_standardization(
        group_by_col  = year,
        excluded_cols = country,
        scale         = FALSE
      )

    model_space_stats <- compute_model_space_stats(
      df            = data_prepared,
      dep_var_col   = gdp,
      timestamp_col = year,
      entity_col    = country,
      params        = small_model_space$params
    )

    masks <- non_zero_stats_mask_generator(n_lin_features)

    expect_equal(model_space_stats, small_model_space$stats)
    expect_true(all(model_space_stats[masks$non_zero == 1] != 0))
    expect_true(all(model_space_stats[masks$greater_than_zero == 1] > 0))
  }
)


test_that(paste("model_space computes correct model_space list"), {

  data_prepared <- badp::economic_growth[,1:5] %>%
    badp::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    badp::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  model_space <- optim_model_space(
    df            = data_prepared,
    dep_var_col   = gdp,
    timestamp_col = year,
    entity_col    = country,
    init_value    = function(n) rep(0.5, n)
  )

  expect_equal(length(model_space), 7)
  expect_s3_class(model_space, "badp_model_space")
  expect_equal(class(model_space[[1]]), c("matrix","array"))
  expect_equal(class(model_space[[2]]), c("matrix","array"))

  convergence <- model_space$convergence
  expect_equal(
    rownames(convergence),
    c("converged", "optim_code", "n_restarts", "max_abs_gradient",
      "n_init_draws")
  )
  expect_equal(ncol(convergence), ncol(model_space$params))
  expect_true(all(convergence["converged", ] %in% c(0, 1)))
  expect_true(all(convergence["n_init_draws", ] >= 1))
})


test_that("Moral-Benito BMA results are replicated (main branch only)", {
  skip_on_cran()
  skip_on_os(c("linux", "windows"))
  skip_if(Sys.getenv("RUN_BMA_FULL_TEST") != "true", "Skipping full BMA test except on main branch")

  set.seed(20)

  Sys.setenv("_R_CHECK_LIMIT_CORES_" = FALSE)
  library(parallel)
  cores <- parallel::detectCores(logical = FALSE)
  cl <- makeCluster(cores)

  data_prepared <- badp::economic_growth %>%
    badp::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    badp::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  model_space <- badp::optim_model_space(
    df             = data_prepared,
    timestamp_col  = year,
    entity_col     = country,
    dep_var_col    = gdp,
    init_value     = function(n) rep(0.5, n),
    cl             = cl
  )

  stopCluster(cl)

  bma_results <- badp::bma(model_space, round = 5)

  actual <- bma_results[[1]]
  expected <- badp::full_bma_results[[1]]

  # define per-column tolerances
  tols <- rep(0.003, ncol(expected))
  tols[4] <- 0.006
  tols[6] <- 0.004
  tols[7] <- 0.006
  tols[ncol(expected)] <- 0.8

  n_lin_features <- 9
  masks <- non_zero_stats_mask_generator(n_lin_features)

  compare_matrices(actual, expected, tols = tols)
  expect_true(all(model_space$stats[masks$non_zero == 1] != 0))
  expect_true(all(model_space$stats[masks$greater_than_zero == 1] > 0))
})


test_that("init_model_space_params draws starting values from init_value", {
  df <- badp::economic_growth[, 1:5]

  constant_params <- init_model_space_params(df, year, country, gdp,
                                             init_value = function(n) rep(0.7, n))

  set.seed(42)
  random_params <- init_model_space_params(
    df, year, country, gdp, init_value = function(n) runif(n, 0.1, 1))

  # same shape and the same exclusion (NA) pattern as the constant version
  expect_equal(dim(random_params), dim(constant_params))
  expect_equal(is.na(random_params), is.na(constant_params))

  values <- random_params[!is.na(random_params)]
  expect_true(all(values >= 0.1 & values <= 1))
  expect_gt(length(unique(values)), 1)

  # reproducible under a seed
  set.seed(42)
  random_params_again <- init_model_space_params(
    df, year, country, gdp, init_value = function(n) runif(n, 0.1, 1))
  expect_equal(random_params, random_params_again)
})


test_that("init_value accepts a single number as a constant generator", {
  df <- badp::economic_growth[, 1:5]

  from_generator <- init_model_space_params(
    df, year, country, gdp, init_value = function(n) rep(0.5, n))
  from_scalar <- init_model_space_params(
    df, year, country, gdp, init_value = 0.5)

  expect_equal(from_scalar, from_generator)

  # an integer is fine too
  expect_equal(
    init_model_space_params(df, year, country, gdp, init_value = 2L),
    init_model_space_params(df, year, country, gdp,
                            init_value = function(n) rep(2L, n))
  )
})


test_that("init_value rejects zero and other invalid inputs", {
  df <- badp::economic_growth[, 1:5]

  # 0 is reserved to mark an excluded parameter
  expect_error(
    init_model_space_params(df, year, country, gdp, init_value = 0),
    "cannot be 0"
  )
  expect_error(
    init_model_space_params(df, year, country, gdp,
                            init_value = function(n) rep(0, n)),
    "reserved"
  )

  # neither a function nor a single finite number
  expect_error(
    init_model_space_params(df, year, country, gdp, init_value = c(0.5, 0.7)),
    "must be a function"
  )
  expect_error(
    init_model_space_params(df, year, country, gdp, init_value = "0.5"),
    "must be a function"
  )
  expect_error(
    init_model_space_params(df, year, country, gdp, init_value = NA_real_),
    "must be a function"
  )
})


test_that("starting points at which the likelihood is undefined are redrawn", {
  # stand-in for an infeasible point: the likelihood is undefined (NA)
  # wherever the first parameter is negative
  local_mocked_bindings(
    sem_likelihood = function(params, ...) if (params[1] < 0) NA_real_ else 1
  )

  n_calls <- 0
  init_value <- function(n) {
    n_calls <<- n_calls + 1
    rep(if (n_calls < 3) -1 else 0.5, n)
  }

  init <- feasible_init_params(
    c(-1, -1, -1), data = NULL, exact_value = FALSE, init_value = init_value,
    max_init_attempts = 100, regressors_subset = c("ish", "sed"))

  expect_equal(init$par, c(0.5, 0.5, 0.5))
  # the point passed in, plus the three redraws it took to find a feasible one
  expect_equal(init$n_init_draws, 4)
})


test_that("drawing feasible starting points gives up after max_init_attempts", {
  local_mocked_bindings(sem_likelihood = function(params, ...) NA_real_)

  expect_error(
    feasible_init_params(
      c(-1, -1, -1), data = NULL, exact_value = FALSE,
      init_value = function(n) rep(-1, n), max_init_attempts = 5,
      regressors_subset = c("ish", "sed")),
    "Could not draw a feasible starting point for the model with regressors: ish, sed in 5 attempts"
  )
})


test_that("BFGS stepping out of the region where the likelihood is defined does not abort", {
  # Stand-in for the SEM likelihood: chol() throws once the parameter stops
  # being positive, and the maximum lies just inside that edge, so from this
  # starting point the line search is bound to step across it.
  lik_tape <- RTMB::MakeTape(
    function(p) {
      "[<-" <- RTMB::ADoverload("[<-")
      m <- matrix(0, 1, 1)
      m[1, 1] <- p[1]
      -100 * (p[1] + 5)^2 + sum(log(diag(chol(m))))
    },
    1
  )
  gr <- function(p) as.numeric(lik_tape$jacobian(p))

  # handed to optim() unguarded, the error escapes and would abort the whole
  # model space
  expect_error(
    stats::optim(1, lik_tape, gr = gr, method = "BFGS",
                 control = list(fnscale = -1)),
    "leading minor"
  )

  fit <- optim_with_restarts(1, lik_tape,
                             control = list(fnscale = -1, maxit = 200),
                             max_restarts = 2, restart_tol = 1e-3)

  # the maximizer of -100 (p + 5)^2 + log(p) / 2 over p > 0
  expect_equal(fit$par, 5e-4, tolerance = 0.01)
  expect_equal(unname(fit$diagnostics["converged"]), 1)
})


test_that("usable_solution accepts only invertible observed information", {
  # a proper maximum: positive definite and well conditioned
  expect_true(usable_solution(diag(c(2, 3))))
  # a stationary point that is not a maximum
  expect_false(usable_solution(diag(c(2, -3))))
  # positive definite in theory, singular in floating point
  expect_false(usable_solution(diag(c(1, 1e-20))))
})



test_that("a solution no standard errors can be computed from is re-optimized", {
  # -(x - 1)^2 - (x y)^2 is taped for real, so the observed information below
  # is the genuine Hessian of it. At (0, 0) it is singular in the second
  # coordinate; at (1, 0) it is an ordinary maximum. Written with explicit
  # multiplication rather than ^2: CppAD's pow() evaluates x^y as
  # exp(y * log(x)) for non-integer-literal y, so its second derivative is
  # NaN wherever the tape meets log(0), even where the true derivative is
  # finite - which both (0, 0) and (1, 0) do here, through the x * y term.
  local_mocked_bindings(
    sem_likelihood = function(params, ...) {
      -(params[1] - 1) * (params[1] - 1) -
        (params[1] * params[2]) * (params[1] * params[2])
    }
  )

  attempts <- 0
  solutions <- list(c(0, 0), c(1, 0))
  local_mocked_bindings(
    optim_with_restarts = function(par, lik_tape, ...) {
      attempts <<- attempts + 1
      list(par = solutions[[min(attempts, length(solutions))]],
           diagnostics = c(converged = 1, optim_code = 0, n_restarts = 0,
                           max_abs_gradient = 0))
    }
  )

  control <- list(trace = 0, maxit = 1000, fnscale = -1, scale = 0.05)
  fit <- optim_from_usable_start(
    c(2, 2), data = NULL, exact_value = FALSE,
    init_value = function(n) rep(2, n), max_init_attempts = 10,
    control = control, max_restarts = 2, restart_tol = 1e-3,
    max_reoptimizations = 3, regressors_subset = "x")

  # the first solution was discarded and the second one kept
  expect_equal(attempts, 2)
  expect_equal(fit$par, c(1, 0))
  expect_equal(unname(fit$diagnostics["converged"]), 1)
})


test_that("re-optimization gives up after max_reoptimizations without erroring", {
  local_mocked_bindings(
    sem_likelihood = function(params, ...) {
      -(params[1] - 1) * (params[1] - 1) -
        (params[1] * params[2]) * (params[1] * params[2])
    }
  )

  attempts <- 0
  local_mocked_bindings(
    optim_with_restarts = function(par, lik_tape, ...) {
      attempts <<- attempts + 1
      list(par = c(0, 0),
           diagnostics = c(converged = 1, optim_code = 0, n_restarts = 0,
                           max_abs_gradient = 0))
    }
  )

  control <- list(trace = 0, maxit = 1000, fnscale = -1, scale = 0.05)
  fit <- optim_from_usable_start(
    c(2, 2), data = NULL, exact_value = FALSE,
    init_value = function(n) rep(2, n), max_init_attempts = 10,
    control = control, max_restarts = 2, restart_tol = 1e-3,
    max_reoptimizations = 2, regressors_subset = "x")

  # the first attempt plus the two allowed re-optimizations
  expect_equal(attempts, 3)
  # the model is kept, but is not passed off as converged
  expect_equal(fit$par, c(0, 0))
  expect_equal(unname(fit$diagnostics["converged"]), 0)
})
