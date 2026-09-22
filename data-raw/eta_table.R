# Curvature-adjusted learning rate across the bundled model spaces, under the
# corrected numerator.
#
# The magnitude adjustment matches the mean of the adjusted log-likelihood
# ratio to a chi-squared distribution. Only rank(J) eigenvalues of H^-1 J are
# non-zero, so the rate is rank(J) / tr(H^-1 J). Earlier versions used
# dim(theta), inflating it by dim(theta)/rank(J) and producing estimates above
# one, which a magnitude adjustment cannot take when it is meaningful.
#
# Run AFTER regenerating the model spaces, so that rank(J) is stored:
#
#   devtools::load_all()
#   source("data-raw/eta_table.R")
#   eta_table()

eta_table <- function() {
  spaces <- list(
    small          = badp::small_model_space,
    migration      = badp::migration_model_space,
    migration_nn   = badp::migration_model_space_nonnested,
    full           = badp::full_model_space
  )

  rows <- lapply(names(spaces), function(nm) {
    ms <- spaces[[nm]]
    K <- length(ms$reg_names)
    stats <- ms$stats

    if (nrow(stats) < 5 + 2 * K) {
      return(data.frame(
        space = nm, models = ncol(stats), dim_theta = NA, rank_J = NA,
        eta_old = NA, eta_new = NA, inflation = NA
      ))
    }

    trace_hinv_j <- stats[3 + 2 * K, ]
    n_theta      <- stats[4 + 2 * K, ]
    rank_j       <- stats[5 + 2 * K, ]

    ok <- is.finite(trace_hinv_j) & trace_hinv_j > 0

    data.frame(
      space     = nm,
      models    = ncol(stats),
      dim_theta = paste0(min(n_theta), "-", max(n_theta)),
      rank_J    = paste0(min(rank_j), "-", max(rank_j)),
      eta_old   = round(stats::median(n_theta[ok] / trace_hinv_j[ok]), 4),
      eta_new   = round(stats::median(rank_j[ok]  / trace_hinv_j[ok]), 4),
      inflation = round(stats::median(n_theta[ok] / rank_j[ok]), 2)
    )
  })

  out <- do.call(rbind, rows)

  cat("\neta_old used dim(theta) in the numerator, eta_new uses rank(J).\n")
  cat("A meaningful magnitude adjustment satisfies eta <= 1.\n\n")

  out
}
