non_zero_stats_mask_generator <- function(n_lin_features, n_rows = NULL) {
  lin_features_mask <- t(rje::powerSetMat(n_lin_features))
  # One entry per model, i.e. one per column of lin_features_mask, so that
  # rbind() below does not have to recycle these rows.
  n_models <- ncol(lin_features_mask)
  ones <- rep(1, n_models)
  zeros <- rep(0, n_models)

  mask_where_nonzero <- rbind(
    ones, ones, ones,
    lin_features_mask,
    ones,
    lin_features_mask
  )
  mask_where_greater_than_zero <- rbind(
    zeros, zeros, zeros,
    lin_features_mask,
    zeros,
    lin_features_mask
  )

  # Model spaces fitted from badp 0.6.0 onwards carry two further rows,
  # tr(H^-1 J) and dim(theta), both of which are strictly positive. Pad the
  # masks when the matrix under test has them, so that the same helper works
  # for stored objects fitted before the change and for freshly computed ones.
  if (!is.null(n_rows) && n_rows > nrow(mask_where_nonzero)) {
    extra <- n_rows - nrow(mask_where_nonzero)
    mask_where_nonzero <- rbind(mask_where_nonzero, matrix(1, extra, n_models))
    mask_where_greater_than_zero <-
      rbind(mask_where_greater_than_zero, matrix(1, extra, n_models))
  }

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

    masks <- non_zero_stats_mask_generator(n_lin_features,

                                           n_rows = nrow(small_model_space$stats))

    expect_true(all(small_model_space$stats[masks$nonzero == 1] != 0))
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

    masks <- non_zero_stats_mask_generator(n_lin_features,

                                           n_rows = nrow(full_model_space$stats))

    expect_true(all(full_model_space$stats[masks$nonzero == 1] != 0))
    expect_true(all(full_model_space$stats[masks$greater_than_zero == 1] > 0))
  }
)
