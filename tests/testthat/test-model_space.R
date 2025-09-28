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

  data_prepared <- bdsm::economic_growth[,1:6] %>%
    bdsm::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    bdsm::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  params <- optim_model_space_params(
    df            = data_prepared,
    dep_var_col   = gdp,
    timestamp_col = year,
    entity_col    = country,
    init_value    = 0.5
  )

  compare_matrices(params, small_model_space$params, tols = rep(0.001, 8))
})

non_zero_stats_mask_generator <- function(lin_features_n) {
  ones <- rep(1, lin_features_n)
  lin_features_mask <- t(rje::powerSetMat(lin_features_n))
  mask_where_nonzero <- rbind(
    ones, ones, ones,
    lin_features_mask,
    ones,
    lin_features_mask
  )
  zeros <- rep(0, lin_features_n)
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

    lin_features_n <- 3

    data_prepared <- bdsm::economic_growth[, 1:(3+lin_features_n)] %>%
      bdsm::feature_standardization(
        excluded_cols = c(country, year, gdp)
      ) %>%
      bdsm::feature_standardization(
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

    masks <- non_zero_stats_mask_generator(lin_features_n)

    expect_equal(model_space_stats, small_model_space$stats)
    expect_true(all(model_space_stats[masks$non_zero == 1] != 0))
    expect_true(all(model_space_stats[masks$greater_than_zero == 1] > 0))
  }
)


test_that(paste("model_space computes correct model_space list"), {

  data_prepared <- bdsm::economic_growth[,1:5] %>%
    bdsm::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    bdsm::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  model_space <- optim_model_space(
    df            = data_prepared,
    dep_var_col   = gdp,
    timestamp_col = year,
    entity_col    = country,
    init_value    = 0.5
  )

  expect_equal(length(model_space), 2)
  expect_equal(class(model_space), "list")
  expect_equal(class(model_space[[1]]), c("matrix","array"))
  expect_equal(class(model_space[[2]]), c("matrix","array"))
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

  data_prepared <- bdsm::economic_growth %>%
    bdsm::feature_standardization(
      excluded_cols = c(country, year, gdp)
    ) %>%
    bdsm::feature_standardization(
      group_by_col  = year,
      excluded_cols = country,
      scale         = FALSE
    )

  model_space <- bdsm::optim_model_space(
    df             = data_prepared,
    timestamp_col  = year,
    entity_col     = country,
    dep_var_col    = gdp,
    init_value     = 0.5,
    cl             = cl
  )

  stopCluster(cl)

  bma_results <- bdsm::bma(model_space, df = data_prepared, round = 5)

  actual <- bma_results[[1]]
  expected <- bdsm::full_bma_results[[1]]

  # define per-column tolerances
  tols <- rep(0.003, ncol(expected))
  tols[4] <- 0.005
  tols[7] <- 0.006
  tols[ncol(expected)] <- 0.8

  lin_features_n <- 9
  masks <- non_zero_stats_mask_generator(lin_features_n)

  compare_matrices(actual, expected, tols = tols)
  expect_true(all(model_space$stats[masks$non_zero == 1] != 0))
  expect_true(all(model_space$stats[masks$greater_than_zero == 1] > 0))
})
