test_that("feature_standardization returns data frame for not cross sectional", {
  set.seed(1)
  df <- feature_standardization(
    df            = generate_test_data(),
    group_by_col  = times,
    excluded_cols = entities
  )
  expect_s3_class(df, "data.frame")
})

test_that("feature_standardization returns tibble for cross-sectional demeaning (time effects)", {
  set.seed(1)
  df <- feature_standardization(
    df            = generate_test_data(),
    group_by_col  = times,
    excluded_cols = entities
  )
  expect_s3_class(df, "tbl")
})

test_that("feature_standardization returns all columns provided", {
  set.seed(1)
  df <- feature_standardization(
    df            = generate_test_data(),
    group_by_col  = times,
    excluded_cols = entities
  )
  expect_equal(length(df), length(generate_test_data()))
  expect_equal(colnames(df), colnames(generate_test_data()))
})

test_that("feature_standardization returns all rows provided", {
  set.seed(1)
  df <- feature_standardization(
    df            = generate_test_data(),
    group_by_col  = times,
    excluded_cols = entities
  )
  expect_equal(nrow(df), nrow(generate_test_data()))
})


test_that("join_lagged_col merges the lagged column into a single column", {

  df <- data.frame(
    year    = c(2000, 2001, 2002, 2000, 2001, 2002),
    country = c("A", "A", "A", "B", "B", "B"),
    gdp     = c(2, 3, 4, 20, 30, 40),
    gdp_lag = c(1, 2, 3, 10, 20, 30),
    x       = c(5, 6, 7, 50, 60, 70)
  )

  out <- join_lagged_col(df, gdp, gdp_lag, year, country, timestep = 1)

  expect_s3_class(out, "data.frame")

  # the lagged column is consumed, the quantity is left in one column
  expect_false("gdp_lag" %in% names(out))
  expect_true(all(c("year", "country", "gdp", "x") %in% names(out)))

  # the earliest lagged value becomes an extra period for each entity
  expect_equal(nrow(out), nrow(df) + length(unique(df$country)))
  expect_equal(anyDuplicated(out[, c("year", "country")]), 0L)

  # the series is continuous and in the original units
  a <- out[out$country == "A", ]
  a <- a[order(a$year), ]
  expect_equal(a$year, c(1999, 2000, 2001, 2002))
  expect_equal(a$gdp, c(1, 2, 3, 4))

  # the regressors are not invented for the period that did not exist
  expect_true(is.na(a$x[1]))
  expect_equal(a$x[-1], c(5, 6, 7))
})


test_that("join_lagged_col requires the lagged column it is named for", {

  # data that hold the quantity once are already in the expected shape, which
  # is why the step is documented as conditional: there is nothing to join,
  # and asking for it anyway is an error rather than a silent no-op
  already <- data.frame(
    year    = c(2000, 2001),
    country = c("A", "A"),
    gdp     = c(2, 3)
  )

  expect_false(any(grepl("_lag$", names(already))))
  expect_error(
    join_lagged_col(already, gdp, gdp_lag, year, country, timestep = 1)
  )
})
