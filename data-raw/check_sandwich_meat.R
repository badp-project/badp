# Numerical check of the sandwich "meat" construction.
#
# Background. The concentrated log-likelihood is computed as
#
#   l(theta) = sum_i [ g(theta) + c(theta) + b_i(theta) ]
#
# with g = -1/2 log det Sigma_11 and c = -1/2 log det(H/N) common to every
# entity, and b_i = -1/2 u_i Sigma_11^{-1} u_i' specific to entity i. The
# per-entity score used to build J = sum_i s_i s_i' is therefore
# s_i = m + b_i' with m = g' + c' the same for all i.
#
# A referee asked whether allocating the global log|H/N| term across entity
# contributions yields a valid meat, given that H depends on the whole sample.
#
# Claim to be tested. At the MLE, sum_i s_i = 0, hence sum_i b_i' = -N m, and
#
#   J = sum_i s_i s_i' = sum_i b_i' b_i'' - N m m' = sum_i (b_i' - bbar')(...)'
#
# i.e. the common term cancels exactly and J depends only on the variation of
# the entity-specific parts across i. Equivalently, centring the rows of G
# must leave crossprod(G) unchanged.
#
# Note that centring G also removes m, so centred G equals centred G_b without
# needing a separate likelihood that omits the common term: the two claims
# collapse into one, and both hold if and only if colSums(G) = 0.
#
# Usage:
#   devtools::load_all()
#   source("data-raw/check_sandwich_meat.R")
#   check_sandwich_meat()

check_sandwich_meat <- function(model_space = badp::migration_model_space,
                                models = 1:8) {

  df <- model_space$df

  shared <- badp:::matrices_from_df(
    df,
    timestamp_col  = Time,
    entity_col     = Pair,
    dep_var_col    = Mig,
    which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix")
  )

  n_entities <- nrow(shared$Z)

  gmat_for_model <- function(j) {
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

    p <- as.numeric(stats::na.omit(params))

    per_entity_tape <- RTMB::MakeTape(
      function(q) badp:::sem_likelihood(q, data = dat, per_entity = TRUE),
      p
    )
    lik_tape <- RTMB::MakeTape(
      function(q) badp:::sem_likelihood(q, data = dat, exact_value = TRUE),
      p
    )

    list(
      G    = per_entity_tape$jacobian(p),
      hess = -lik_tape$jacfun()$jacobian(p)
    )
  }

  out <- do.call(rbind, lapply(models, function(j) {
    fit <- gmat_for_model(j)
    G <- fit$G
    hess <- fit$hess

    # (1) First-order condition. The total score is the column sum of G.
    total_score <- colSums(G)
    # Scale reference: typical magnitude of the summed entity scores if they
    # were independent, i.e. sqrt(N) times their standard deviation.
    scale_ref <- max(apply(G, 2, stats::sd)) * sqrt(nrow(G))

    # (2) Does centring change the meat?
    Gc <- sweep(G, 2, colMeans(G))
    J  <- crossprod(G)
    Jc <- crossprod(Gc)

    # (3) Does it change the reported robust standard errors?
    hinv <- solve(hess)
    se  <- sqrt(diag(hinv %*% J  %*% hinv))
    sec <- sqrt(diag(hinv %*% Jc %*% hinv))

    data.frame(
      model         = j,
      n_theta       = ncol(G),
      n_entities    = nrow(G),
      rank_J        = qr(J)$rank,
      max_score     = max(abs(total_score)),
      score_rel     = max(abs(total_score)) / scale_ref,
      J_rel_diff    = max(abs(J - Jc)) / max(abs(J)),
      se_rel_diff   = max(abs(se - sec) / se)
    )
  }))

  cat("\nEntities:", n_entities, "\n")
  cat("If the claim holds, score_rel, J_rel_diff and se_rel_diff are all ~0.\n")
  cat("rank_J < n_theta indicates the separate rank-deficiency problem.\n\n")

  out
}
