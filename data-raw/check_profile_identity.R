# Confirm that the reported robust standard errors equal the profile sandwich.
#
# check_meat_rank() showed J is exactly zero outside a small set S of
# coordinates (those with cross-entity score variation), which includes every
# alpha and beta that bma() reports. Writing nu for the remaining nuisance
# coordinates, J = [[J_SS, 0], [0, 0]], so
#
#   (H^-1 J H^-1)_SS = (H^-1)_SS J_SS (H^-1)_SS
#
# and by block inversion (H^-1)_SS = (H_SS - H_Snu H_nunu^-1 H_nuS)^-1, the
# Schur complement. The reported robust standard errors are therefore exactly
# the profile sandwich on the identified block: the nuisance directions are
# profiled out rather than dividing through a degenerate block.
#
# This script checks that identity numerically.
#
# Usage:
#   devtools::load_all()
#   source("data-raw/check_profile_identity.R")
#   check_profile_identity()

check_profile_identity <- function(model_space = badp::migration_model_space,
                                   models = 1:8,
                                   tol = 1e-8) {

  df <- model_space$df

  shared <- badp:::matrices_from_df(
    df,
    timestamp_col  = Time,
    entity_col     = Pair,
    dep_var_col    = Mig,
    which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix")
  )

  one_model <- function(j) {
    params <- model_space$params[, j]
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

    per_entity_tape <- RTMB::MakeTape(
      function(q) badp:::sem_likelihood(q, data = dat, per_entity = TRUE), p
    )
    lik_tape <- RTMB::MakeTape(
      function(q) badp:::sem_likelihood(q, data = dat, exact_value = TRUE), p
    )

    G <- per_entity_tape$jacobian(p)
    hess <- -lik_tape$jacfun()$jacobian(p)
    J <- crossprod(G)

    # Identified coordinates: those with cross-entity variation in the score.
    Gc <- sweep(G, 2, colMeans(G))
    rel_var <- apply(Gc, 2, function(x) sqrt(sum(x^2))) /
      pmax(apply(G, 2, function(x) sqrt(sum(x^2))), .Machine$double.eps)
    S <- which(rel_var > tol)
    nu <- setdiff(seq_along(p), S)

    # (a) as implemented: full sandwich, then take the S diagonal
    hinv <- solve(hess)
    se_full <- sqrt(diag(hinv %*% J %*% hinv))[S]

    # (b) profile sandwich on the identified block
    H_prof <- if (length(nu) > 0) {
      hess[S, S] - hess[S, nu] %*% solve(hess[nu, nu]) %*% hess[nu, S]
    } else {
      hess[S, S]
    }
    Hp_inv <- solve(H_prof)
    se_prof <- sqrt(diag(Hp_inv %*% J[S, S] %*% Hp_inv))

    # (c) naive: ignore the nuisance coupling entirely
    Hss_inv <- solve(hess[S, S])
    se_naive <- sqrt(diag(Hss_inv %*% J[S, S] %*% Hss_inv))

    data.frame(
      model            = j,
      dim_theta        = length(p),
      n_identified     = length(S),
      full_vs_profile  = max(abs(se_full - se_prof) / se_full),
      full_vs_naive    = max(abs(se_full - se_naive) / se_full)
    )
  }

  out <- do.call(rbind, lapply(models, one_model))

  cat("\nfull_vs_profile ~ 0 confirms the reported robust SEs are exactly the",
      "\nprofile sandwich on the identified block.\n")
  cat("full_vs_naive shows how much the nuisance coupling actually matters.\n\n")

  out
}
