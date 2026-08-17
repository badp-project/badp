"full_model_space"

library(magrittr)

data_prepared <- badp::economic_growth %>%
  badp::feature_standardization(
    excluded_cols = c(country, year, gdp)
  ) %>%
  badp::feature_standardization(
    group_by_col  = year,
    excluded_cols = country,
    scale         = FALSE
  )

library(parallel)

# Choose an appropriate number of cores, taking into account system-level limits
cores <- as.integer(Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA))
if (is.na(cores)) {
  cores <- detectCores()
} else {
  cores <- min(cores, detectCores())
}
cl <- makeCluster(cores)
clusterEvalQ(cl, devtools::load_all())

# Reproducibility for the random starting points below. set.seed() alone does
# not reach PSOCK workers, which is where the per-model optimization runs.
# The seed is an arbitrary constant; its value has no significance.
clusterSetRNGStream(cl, 12345)

# A constant starting point makes the recovery mechanism inside
# optim_model_space() inert: when a model ends at a solution whose observed
# information matrix is not positive definite, the starting point is redrawn
# from init_value and the model re-optimized, but redrawing a constant returns
# the same point and the same solution. With nine regressors, four of the 512
# models failed this way. A random generator makes the redraws effective.
full_model_space <- optim_model_space(
  df            = data_prepared,
  dep_var_col   = gdp,
  timestamp_col = year,
  entity_col    = country,
  init_value    = function(n) runif(n, 0.1, 1),
  max_reoptimizations = 20,
  cl = cl
)

stopCluster(cl)

if (any(full_model_space$convergence["converged", ] == 0)) {
  warning(sum(full_model_space$convergence["converged", ] == 0),
          " models still did not converge; do not overwrite the shipped ",
          "data until this is resolved.")
}

usethis::use_data(full_model_space, overwrite = TRUE)
