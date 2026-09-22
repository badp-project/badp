source("renv/activate.R")

# Packages not tracked by renv (Suggests-only dev tools, per
# renv/settings.json's package.dependency.fields) come from the devbox
# shell's R_LIBS_SITE instead. Register them as external libraries so renv
# is aware of them without trying to manage them, and append them after the
# renv project library so renv-tracked package versions still win.
devbox_libs <- Sys.getenv("R_LIBS_SITE")
if (nzchar(devbox_libs)) {
  devbox_libs <- strsplit(devbox_libs, .Platform$path.sep, fixed = TRUE)[[1]]
  .libPaths(c(.libPaths(), devbox_libs))
}
