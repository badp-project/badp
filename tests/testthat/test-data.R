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
  paste("small_model_space has a correct structure"),
  {
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

    masks <- non_zero_stats_mask_generator(lin_features_n)

    expect_true(all(small_model_space$stats[masks$non_zero == 1] != 0))
    expect_true(all(small_model_space$stats[masks$greater_than_zero == 1] > 0))
  }
)

test_that(
  paste("full_model_space has a correct structure"),
  {
    lin_features_n <- 9

    data_prepared <- bdsm::economic_growth[, 1:(3+lin_features_n)] %>%
      bdsm::feature_standardization(
        excluded_cols = c(country, year, gdp)
      ) %>%
      bdsm::feature_standardization(
        group_by_col  = year,
        excluded_cols = country,
        scale         = FALSE
      )

    masks <- non_zero_stats_mask_generator(lin_features_n)

    expect_true(all(full_model_space$stats[masks$non_zero == 1] != 0))
    expect_true(all(full_model_space$stats[masks$greater_than_zero == 1] > 0))
  }
)
