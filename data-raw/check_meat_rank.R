# Which parameters does the sandwich meat actually identify?
#
# check_sandwich_meat() showed that J = crossprod(G) is unchanged by centring
# the rows of G, so rank(J) = rank(G - colMeans(G)): only the *variation across
# entities* of the per-entity scores contributes. Any parameter whose score
# contribution is the same for every entity drops out entirely.
#
# That rank turned out to be far below min(N, dim theta) -- 8 out of 43 with
# 253 entities -- so the deficiency is structural, not a too-few-clusters
# problem. This script identifies which coordinates survive, and in particular
# whether the alpha and beta coordinates reported as PSDR are among them.
#
# Usage:
#   devtools::load_all()
#   source("data-raw/check_meat_rank.R")
#   check_meat_rank()

check_meat_rank <- function(model_space = badp::migration_model_space,
                            model = 8,
                            tol = 1e-8) {

  df <- model_space$df

  shared <- badp:::matrices_from_df(
    df,
    timestamp_col  = Time,
    entity_col     = Pair,
    dep_var_col    = Mig,
    which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix")
  )

  params <- model_space$params[, model]
  regressors <- badp:::regressor_names_from_params_vector(params)

  model_matrices <- badp:::matrices_from_df(
    df,
    timestamp_col          = Time,
    entity_col             = Pair,
    dep_var_col            = Mig,
    lin_related_regressors = regressors,
    which_matrices         = c("cur_Y2", "cur_Z")
  )

  dat <- shared
  dat$cur_Z  <- model_matrices$cur_Z
  dat$cur_Y2 <- model_matrices$cur_Y2

  kept <- !is.na(params)
  p <- as.numeric(params[kept])
  param_names <- names(params)[kept]

  per_entity_tape <- RTMB::MakeTape(
    function(q) badp:::sem_likelihood(q, data = dat, per_entity = TRUE),
    p
  )
  G <- per_entity_tape$jacobian(p)

  Gc <- sweep(G, 2, colMeans(G))

  # Per-coordinate cross-entity variation, scaled by the typical magnitude of
  # the score itself so that "no variation" is distinguishable from "small".
  variation <- apply(Gc, 2, function(x) sqrt(sum(x^2)))
  magnitude <- apply(G,  2, function(x) sqrt(sum(x^2)))
  rel_variation <- variation / pmax(magnitude, .Machine$double.eps)

  identified <- rel_variation > tol

  res <- data.frame(
    coord         = seq_along(p),
    parameter     = param_names,
    value         = p,
    variation     = variation,
    rel_variation = rel_variation,
    identified    = identified
  )

  cat("\nModel", model, "of", ncol(model_space$params), "\n")
  cat("Entities:", nrow(G), "  dim(theta):", ncol(G), "\n")
  cat("rank(J):", qr(crossprod(G))$rank,
      "  coordinates with cross-entity variation:", sum(identified), "\n\n")

  cat("Coordinates WITH cross-entity variation (identified):\n")
  print(res[identified, c("coord", "parameter", "rel_variation")],
        row.names = FALSE)

  cat("\nCoordinates WITHOUT (robust SE not identified):\n")
  print(utils::head(res[!identified, c("coord", "parameter", "rel_variation")],
                    20), row.names = FALSE)
  if (sum(!identified) > 20) {
    cat("... and", sum(!identified) - 20, "more\n")
  }

  # The columns bma() reports: alpha, then the betas.
  reported <- grep("^(alpha|beta)", param_names)
  cat("\nCoordinates reported as PSD/PSDR by bma():",
      paste(param_names[reported], collapse = ", "), "\n")
  cat("Of these, identified:",
      sum(identified[reported]), "of", length(reported), "\n\n")

  invisible(res)
}
