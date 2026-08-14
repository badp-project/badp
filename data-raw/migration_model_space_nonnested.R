# migration_model_space_nonnested

# CHANGING THE DATA INTO APPROPRIATE FORMAT
migration_formatted <- join_lagged_col(migration_data, col = Mig, col_lagged = Mig_lag, entity_col = Pair, timestamp_col = Time, timestep = 5)

# DATA STANDARDIZATION
standardized_migration <- feature_standardization(df = migration_formatted,  excluded_cols = c(Time,Pair))

# INTRODUCTION OF TIME FIXED EFFECTS
time_effect_migration <- feature_standardization(df = standardized_migration, group_by_col = Time, excluded_cols = Pair)


# CALCULATION OF MODEL SPACE WITH PARALLEL COMPUTING  (57 sec)
library(parallel)
cores <- detectCores()
cl <- makeCluster(cores)
# The workers must load the development version of the package, not whatever
# is installed; otherwise the model space is computed by the installed code
# and silently differs from the one the master session would produce.
clusterEvalQ(cl, devtools::load_all())
setDefaultCluster(cl)

migration_model_space_nonnested <- optim_model_space(
  df = time_effect_migration,
  timestamp_col = Time,
  entity_col = Pair,
  dep_var_col= Mig,
  cl=cl,
  init_value = function(n) rep(0.5, n),
  nested = FALSE)

stopCluster(cl)

usethis::use_data(migration_model_space_nonnested, overwrite = TRUE)
