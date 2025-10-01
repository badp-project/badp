#include "SEM_matrices.h"
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

//' Residual Maker Matrix
//'
//' Create residual maker matrix from a given matrix \code{m}. See article about
//' \href{https://en.wikipedia.org/wiki/Projection_matrix}{projection matrix} on
//' the Wikipedia.
//'
//' @param m Matrix
//'
//' @return
//' M x M matrix where M is the number of rows in the \code{m} matrix.
//' @export
//'
//' @examples
//' residual_maker_matrix(matrix(c(1,2,3,4), nrow = 2))
// [[Rcpp::export]]
arma::mat residual_maker_matrix(const arma::mat &m) {
  arma::mat proj_matrix = m * arma::solve(m.t() * m, m.t());
  return arma::eye(m.n_rows, m.n_rows) - proj_matrix;
}

//' Coefficients matrix for SEM representation
//'
//' Create coefficients matrix for Simultaneous Equations Model (SEM)
//' representation.
//'
//' @param alpha numeric
//' @param periods_n integer
//' @param beta numeric vector. Default is c() for no regressors case.
//'
//' @return List with two matrices B11 and B12
//' @export
//'
//' @examples
//' sem_B_matrix(3, 4, 4:6)
// [[Rcpp::export]]
Rcpp::List sem_B_matrix(double alpha, int periods_n,
                        Rcpp::Nullable<arma::vec> beta = R_NilValue) {
  // Create B11 matrix
  arma::mat B11 = arma::eye(periods_n, periods_n);

  // Create alpha_matrix (diagonal matrix with -alpha values)
  arma::mat alpha_matrix = arma::diagmat(arma::vec(periods_n - 1).fill(-alpha));

  // Add alpha_matrix to B11[2:periods_n, 1:(periods_n-1)]
  B11.submat(1, 0, periods_n - 1, periods_n - 2) += alpha_matrix;

  // Create B12 matrix
  arma::mat B12;
  bool has_beta = false;

  if (beta.isNotNull()) {
    arma::vec beta_vec = Rcpp::as<arma::vec>(beta);
    if (beta_vec.n_elem > 0) {
      has_beta = true;
      int regressors_n = beta_vec.n_elem;
      int n_cols = regressors_n * (periods_n - 1);

      // Initialize B12 with zeros
      B12 = arma::zeros(periods_n, n_cols);

      // Fill beta_matrix for rows 2 to periods_n
      for (int row_ind = 1; row_ind < periods_n; row_ind++) {
        // Place -beta values in the appropriate position
        int start_col = (row_ind - 1) * regressors_n;
        B12(row_ind, arma::span(start_col, start_col + regressors_n - 1)) =
            -beta_vec.t();
      }
    }
  }

  // Return list
  if (has_beta) {
    return Rcpp::List::create(B11, B12);
  } else {
    return Rcpp::List::create(B11, R_NilValue);
  }
}

//' Coefficients matrix for initial conditions
//'
//' Create matrix for Simultaneous Equations Model (SEM)
//' representation with coefficients placed next to initial values
//' of regressors, dependent variable and country-specific time-invariant
//' variables.
//'
//' @param alpha numeric
//' @param phi_0 numeric
//' @param periods_n numeric
//' @param beta numeric vector. Default is c() for no regressors case.
//' @param phi_1 numeric vector. Default is c() for no regressors case.
//'
//' @return matrix
//' @export
//'
//' @examples
//' alpha <- 9
//' phi_0 <- 19
//' beta <- 11:15
//' phi_1 <- 21:25
//' periods_n <- 4
//' sem_C_matrix(alpha, phi_0, periods_n, beta, phi_1)
// [[Rcpp::export]]
arma::mat sem_C_matrix(double alpha, double phi_0, int periods_n,
                       Rcpp::Nullable<arma::vec> beta = R_NilValue,
                       Rcpp::Nullable<arma::vec> phi_1 = R_NilValue) {
  // Create C1 matrix - column vector with phi_0 repeated periods_n times
  arma::mat C1(periods_n, 1, arma::fill::value(phi_0));

  // Add alpha to the first element
  C1(0, 0) += alpha;

  // Handle beta and phi_1 if provided
  if (beta.isNotNull() && phi_1.isNotNull()) {
    arma::vec beta_vec = Rcpp::as<arma::vec>(beta);
    arma::vec phi_1_vec = Rcpp::as<arma::vec>(phi_1);

    if (beta_vec.n_elem > 0) {
      arma::mat col2(periods_n, phi_1_vec.n_elem);

      col2.each_row() = phi_1_vec.t();

      col2.row(0) += beta_vec.t();

      C1 = arma::join_rows(C1, col2);
    }
  }

  return C1;
}

//' Matrix with psi parameters for SEM representation
//'
//' @param psis double vector with psi parameter values
//' @param timestamps_n number of time stamps (e.g. years)
//' @param features_n number of features (e.g. population size, investment rate)
//'
//' @return
//' A matrix with \code{timestamps_n} rows and
//' \code{(timestamps_n - 1) * feature_n} columns. Psis are filled in row by row
//' in a block manner, i.e. blocks of size \code{feature_n} are placed next to
//' each other
//'
//' @export
//'
//' @examples
//' sem_psi_matrix(1:30, 4, 5)
// [[Rcpp::export]]
arma::mat sem_psi_matrix(const arma::vec &psis, int timestamps_n,
                         int features_n) {
  int matrix_row_n = timestamps_n;
  int n_cols = (timestamps_n - 1) * features_n;
  arma::mat result(matrix_row_n, n_cols, arma::fill::zeros);

  for (int row_ind = 1; row_ind <= matrix_row_n; row_ind++) {
    // Calculate psi indices for this row
    unsigned int psi_start_ind_in_row =
        row_ind * (row_ind - 1) * features_n / 2 +
        (row_ind - 1) * (timestamps_n - row_ind) * features_n + 1;

    if (row_ind == 1) {
      // First row: fill with psis[psi_start_ind_in_row:psi_end_ind_in_row]
      for (int j = 0; j < (timestamps_n - row_ind) * features_n; j++) {
        if (psi_start_ind_in_row - 1 + j < psis.n_elem) {
          result(row_ind - 1, j) = psis(psi_start_ind_in_row - 1 + j);
        }
      }
    } else if (row_ind == matrix_row_n) {
      // Last row: all zeros (already filled with zeros)
    } else {
      // Other rows: zeros followed by psis
      int n_zeros_front = (row_ind - 1) * features_n;

      // Fill the psi values after the zeros
      for (int j = 0; j < (timestamps_n - row_ind) * features_n; j++) {
        if (psi_start_ind_in_row - 1 + j < psis.n_elem) {
          result(row_ind - 1, n_zeros_front + j) =
              psis(psi_start_ind_in_row - 1 + j);
        }
      }
    }
  }

  return result;
}

//' Covariance matrix for SEM representation
//'
//' Create covariance matrix for Simultaneous Equations Model (SEM)
//' representation. Only the part necessary to compute concentrated likelihood
//' function is computed (cf. Appendix in the Moral-Benito paper)
//'
//' @param err_var numeric
//' @param dep_vars numeric vector
//' @param phis numeric vector
//' @param psis numeric vector
//'
//' @return List with two matrices Sigma11 and Sigma12
//' @export
//'
//' @examples
//' err_var <- 1
//' dep_vars <- c(2, 2, 2, 2)
//' phis <- c(10, 10, 20, 20, 30, 30)
//' psis <- c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112)
//' sem_sigma_matrix(err_var, dep_vars, phis, psis)
// [[Rcpp::export]]
Rcpp::List sem_sigma_matrix(double err_var, const arma::vec &dep_vars,
                            Rcpp::Nullable<arma::vec> phis = R_NilValue,
                            Rcpp::Nullable<arma::vec> psis = R_NilValue) {
  int periods_n = dep_vars.n_elem;

  // Create O11 matrix
  arma::mat O11 = err_var * err_var * arma::ones(periods_n, periods_n) +
                  arma::diagmat(arma::square(dep_vars));

  if (phis.isNull()) {
    return Rcpp::List::create(O11, R_NilValue);
  }
  // Create O12 matrix
  arma::vec phis_vec = Rcpp::as<arma::vec>(phis);
  if (phis_vec.n_elem == 0) {
    return Rcpp::List::create(O11, R_NilValue);
  }

  int regressors_n = phis_vec.n_elem / (periods_n - 1);

  arma::mat phi_matrix(periods_n, phis_vec.n_elem);
  phi_matrix.each_row() = phis_vec.t();

  arma::mat psi_matrix = arma::zeros(periods_n, phis_vec.n_elem);
  if (psis.isNotNull()) {
    const arma::vec psis_vec = Rcpp::as<arma::vec>(psis);
    psi_matrix = sem_psi_matrix(psis_vec, periods_n, regressors_n);
  }

  arma::mat O12 = phi_matrix + psi_matrix;
  return Rcpp::List::create(O11, O12);
}
