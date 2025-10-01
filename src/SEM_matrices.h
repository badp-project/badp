#ifndef SEM_MATRICES_H
#define SEM_MATRICES_H

#include <RcppArmadillo.h>

arma::mat residual_maker_matrix(const arma::mat &m);

Rcpp::List sem_B_matrix(double alpha, int periods_n,
                        Rcpp::Nullable<arma::vec> beta);

arma::mat sem_C_matrix(double alpha, double phi_0, int periods_n,
                       Rcpp::Nullable<arma::vec> beta,
                       Rcpp::Nullable<arma::vec> phi_1);

Rcpp::List sem_sigma_matrix(double err_var, const arma::vec &dep_vars,
                            Rcpp::Nullable<arma::vec> phis,
                            Rcpp::Nullable<arma::vec> psis);

#endif // SEM_MATRICES_H
