non_zero_stats_mask_generator <- function(n_lin_features) {
  ones <- rep(1, n_lin_features)
  lin_features_mask <- t(rje::powerSetMat(n_lin_features))
  mask_where_nonzero <- rbind(
    ones, ones, ones,
    lin_features_mask,
    ones,
    lin_features_mask
  )
  zeros <- rep(0, n_lin_features)
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
  paste("small_model_space has a correct structure"),
  {
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

    masks <- non_zero_stats_mask_generator(n_lin_features)

    expect_true(all(small_model_space$stats[masks$non_zero == 1] != 0))
    expect_true(all(small_model_space$stats[masks$greater_than_zero == 1] > 0))
  }
)

test_that(
  paste("full_model_space has a correct structure"),
  {
    n_lin_features <- 9

    data_prepared <- badp::economic_growth[, 1:(3+n_lin_features)] %>%
      badp::feature_standardization(
        excluded_cols = c(country, year, gdp)
      ) %>%
      badp::feature_standardization(
        group_by_col  = year,
        excluded_cols = country,
        scale         = FALSE
      )

    masks <- non_zero_stats_mask_generator(n_lin_features)

    expect_true(all(full_model_space$stats[masks$non_zero == 1] != 0))
    expect_true(all(full_model_space$stats[masks$greater_than_zero == 1] > 0))
  }
)
