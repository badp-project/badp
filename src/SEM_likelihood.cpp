#include "SEM_matrices.h"
#include <RcppArmadillo.h>
#include <cmath>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
SEXP sem_likelihood_calculate(double alpha, double phi_0, double err_var,
                              const arma::vec &dep_vars, const arma::mat &Y1,
                              const arma::mat &Y2, const arma::mat &cur_Z,
                              Rcpp::Nullable<arma::mat> cur_Y2 = R_NilValue,
                              Rcpp::Nullable<arma::vec> beta = R_NilValue,
                              Rcpp::Nullable<arma::vec> phi_1 = R_NilValue,
                              Rcpp::Nullable<arma::vec> phis = R_NilValue,
                              Rcpp::Nullable<arma::vec> psis = R_NilValue,
                              bool per_entity = false,
                              bool exact_value = true) {
  arma::mat res_maker_matrix = residual_maker_matrix(cur_Z);

  int n_entities = Y1.n_rows;
  int periods_n = dep_vars.n_elem;
  int tot_regressors_n = Y2.n_cols / (periods_n - 1);
  int lin_related_regressors_n =
      beta.isNotNull() ? as<arma::vec>(beta).n_elem : 0;

  Rcpp::List B = sem_B_matrix(alpha, periods_n, beta);
  arma::mat C = sem_C_matrix(alpha, phi_0, periods_n, beta, phi_1);
  Rcpp::List S = sem_sigma_matrix(err_var, dep_vars, phis, psis);

  arma::mat B1 = as<arma::mat>(B[0]);
  arma::mat S1 = as<arma::mat>(S[0]);
  arma::mat S2 = as<arma::mat>(S[1]);

  arma::mat U1;
  if (lin_related_regressors_n == 0) {
    U1 = Y1 * B1.t() - cur_Z * C.t();
  } else {
    arma::mat B2 = as<arma::mat>(B[1]);
    const arma::mat cur_Y2_mat = as<arma::mat>(cur_Y2);
    U1 = cur_Y2_mat * B2.t() + Y1 * B1.t() - cur_Z * C.t();
  }
  arma::mat S11_inverse;
  if (!arma::inv_sympd(S11_inverse, S1)) {
    return wrap(NumericVector::create(NA_REAL));
  }
  arma::mat M = Y2 - U1 * S11_inverse * S2;
  arma::mat H = trans(M) * res_maker_matrix * M;

  double gaussian_normalization_const =
      log(2 * M_PI) * n_entities *
      (periods_n + (periods_n - 1) * tot_regressors_n) / 2.0;
  double trace_simplification_term =
      0.5 * n_entities * (periods_n - 1) * tot_regressors_n;

  double S1_logdet{}, S1_sign{};
  arma::log_det(S1_logdet, S1_sign, S1);
  if (!std::isfinite(S1_logdet) || S1_sign <= 0) {
    return wrap(NumericVector::create(NA_REAL));
  }

  arma::mat H_scaled = H / static_cast<double>(n_entities);
  double H_logdet{}, H_sign{};
  arma::log_det(H_logdet, H_sign, H_scaled);
  if (!std::isfinite(H_logdet) || H_sign <= 0) {
    return wrap(NumericVector::create(NA_REAL));
  }

  double likelihood =
      -static_cast<double>(n_entities) / 2.0 * (S1_logdet + H_logdet);

  if (exact_value) {
    likelihood -= gaussian_normalization_const + trace_simplification_term;
  }

  if (!per_entity) {
    likelihood -= 0.5 * sum(diagvec(S11_inverse * trans(U1) * U1));
    return wrap(likelihood);
  } else {
    arma::vec per_entity_likelihood(n_entities);
    per_entity_likelihood.fill(likelihood / n_entities);
    arma::vec diagonal_terms = -0.5 * diagvec(U1 * S11_inverse * trans(U1));
    per_entity_likelihood += diagonal_terms;
    return wrap(per_entity_likelihood);
  }
}
