"model_space_nonnested"

# The economic growth model space estimated under the non-nested approach,
# i.e. optim_model_space(nested = FALSE). Compare data-raw/full_model_space.R,
# which is the same data under the nested approach of Moral-Benito (2016).

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

# The workers must load the development version of the package, not whatever
# is installed; otherwise the model space is computed by the installed code
# and silently differs from the one the master session would produce.
clusterEvalQ(cl, devtools::load_all())

# Reproducibility for the random starting points below. set.seed() alone does
# not reach PSOCK workers, which is where the per-model optimization runs.
clusterSetRNGStream(cl, 20240812)

# A constant starting point makes the recovery mechanism inside
# optim_model_space() inert: when a model ends at a solution whose observed
# information matrix is not positive definite, the starting point is redrawn
# from init_value and the model re-optimized, but redrawing a constant returns
# the same point and the same solution. A random generator makes the redraws
# effective.
model_space_nonnested <- optim_model_space(
  df            = data_prepared,
  dep_var_col   = gdp,
  timestamp_col = year,
  entity_col    = country,
  init_value    = function(n) runif(n, 0.1, 1),
  max_reoptimizations = 20,
  nested        = FALSE,
  cl            = cl
)

stopCluster(cl)

if (any(model_space_nonnested$convergence["converged", ] == 0)) {
  warning(sum(model_space_nonnested$convergence["converged", ] == 0),
          " models still did not converge; do not overwrite the shipped ",
          "data until this is resolved.")
}

usethis::use_data(model_space_nonnested, overwrite = TRUE)

# Dimensions to check against the roxygen block in R/data.R:
#   dim(model_space_nonnested$params)
#   dim(model_space_nonnested$stats)
