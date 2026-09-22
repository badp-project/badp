# Moral-Benito (2016), "Model averaging in economics: an overview",
# Journal of Applied Econometrics 31(4): 584-602, Table II (p. 594),
# "Growth regressions using panel BMA under weak exogeneity".
#
# Columns (1), (2) and (3) of that table are the BMA posterior mean, the
# posterior standard deviation, and the posterior inclusion probability.
# Results there use the unit information prior for the parameter space and
# the uniform prior for the model space.
#
# These are transcribed from the published paper and are the independent
# anchor for the replication claim made in the vignette and the JSS
# manuscript. Do NOT regenerate them from badp output: the point of the
# anchor is that it does not depend on this package.
#
# Note on which column of bma() they correspond to: the published SD column
# matches the *robust* posterior standard deviation (PSDR), not the
# Hessian-based one (PSD). Moral-Benito's GAUSS implementation reports the
# sandwich version.

published_table_ii <- data.frame(
  row.names = c("gdp_lag", "ish", "sed", "pgrw", "pop",
                "ipr", "opem", "gsh", "lnlex", "polity"),
  PM   = c(0.918, 0.063, 0.031, 0.018, 0.121,
           -0.033, 0.034, -0.013, 0.086, -0.056),
  PSDR = c(0.106, 0.062, 0.071, 0.052, 0.079,
           0.043, 0.032, 0.086, 0.095, 0.052),
  PIP  = c(NA, 77, 72, 71, 98,
           66, 77, 75, 86, 68)
)

# ---------------------------------------------------------------------------
# Diagnostic: compare a fitted model space against the published table.
#
#   devtools::load_all()
#   source("data-raw/published_table_ii.R")
#   check_published_replication(badp::full_model_space)
# ---------------------------------------------------------------------------

check_published_replication <- function(model_space, ...) {
  res <- badp::bma(model_space, round = 5, ...)
  got <- res[[1]]

  out <- data.frame(
    row.names = rownames(published_table_ii),
    PM_pub    = published_table_ii$PM,
    PM_got    = round(got[, "PM"], 3),
    PM_diff   = round(got[, "PM"] - published_table_ii$PM, 3),
    PSDR_pub  = published_table_ii$PSDR,
    PSDR_got  = round(got[, "PSDR"], 3),
    PSDR_diff = round(got[, "PSDR"] - published_table_ii$PSDR, 3),
    PIP_pub   = published_table_ii$PIP,
    PIP_got   = round(got[, "PIP"] * 100, 1)
  )

  n_bad <- sum(model_space$convergence["converged", ] == 0)
  cat("Models that did not converge:", n_bad, "of",
      ncol(model_space$convergence), "\n")
  cat("Max |PM  - published|:  ", max(abs(out$PM_diff)), "\n")
  cat("Max |PSDR - published|: ", max(abs(out$PSDR_diff), na.rm = TRUE), "\n\n")

  out
}
