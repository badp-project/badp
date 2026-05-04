# data-raw/fix_model_space_classes.R
#
# One-off repair script: some bundled model_space data files were saved
# before the `badp_model_space` S3 class was introduced. Without that
# class, `print()` and `summary()` fall back to the default list output
# instead of dispatching to the package's S3 methods. This script loads
# each affected file, attaches the class if missing, and re-saves the
# .rda file with the same compression settings used elsewhere.
#
# Run from the package root, e.g.
#   source("data-raw/fix_model_space_classes.R")

files_to_fix <- c(
  "migration_model_space",
  "migration_model_space_nonnested"
)

repair_one <- function(name) {
  path <- file.path("data", paste0(name, ".rda"))
  if (!file.exists(path)) {
    message("[skip] ", name, ": file not found at ", path)
    return(invisible(FALSE))
  }

  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  if (!exists(name, envir = env, inherits = FALSE)) {
    message("[skip] ", name, ": object not present in ", path)
    return(invisible(FALSE))
  }

  obj <- get(name, envir = env)
  if (inherits(obj, "badp_model_space")) {
    message("[ok]   ", name, ": already has class 'badp_model_space'")
    return(invisible(TRUE))
  }

  class(obj) <- "badp_model_space"
  assign(name, obj)

  # Match the compression used by usethis::use_data() / R CMD build.
  save(list = name, file = path, compress = "xz", version = 2)
  message("[fixed] ", name, ": class attribute added and re-saved")
  invisible(TRUE)
}

invisible(lapply(files_to_fix, repair_one))
