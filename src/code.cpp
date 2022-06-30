#include <Rcpp.h>
using namespace Rcpp;


//' Helper function to set up covariance matrix
//' @param N number of years
//' @param lambda vector of lambda values (one for each box)
//' @param weights vector of weights values (one for each box)
//' @param deltat time step in years (usually 1)
//' @param noise_factor constant scaling factor for AR(1) noise,
//' related to (see sx_from_sd and sd_from_sx)
//' @param o SD of white noise
//' @export
// [[Rcpp::export]]
NumericMatrix setup_sigma(int N, NumericVector lambda,
                          NumericVector weights, double deltat, double noise_factor, double o) {
  NumericMatrix sigma_e(N, N);
  int m = lambda.size();
  for(int k = 0; k < m; k++) {
    for(int l = 0; l < m; l++) {
      for (int i = 0; i < N; i++) {
        sigma_e(i, i) += pow(noise_factor, 2) * weights(k) * weights(l) / (lambda(k) + lambda(l));
        for (int j = 0; j < i; j++) {
          double entry = pow(noise_factor, 2) * weights(k) * weights(l) / (lambda(k) + lambda(l)) * pow(M_E, - lambda(k) * deltat * (i - j));
          sigma_e(i, j) += entry;
          sigma_e(j, i) += entry;
        }
      }
    }
  }
  for(int i = 0; i < N; i++) {
    sigma_e(i, i) += o*o;
  }
  return sigma_e;
}


//' Helper function to solve the ODE
//' @param n_years number of years considered
//' @param lambda vector for lambda, each entry corresponding to one box
//' @param weights vector of weights values, one for each box; should sum up to 1
//' @param forc_vals vector of forcing values (yearly data)
//' @param F0 initial forcing value
//' @return vector of solution values,
//' not scaled be heat capacity yet
//' @export
// [[Rcpp::export]]
NumericVector ode_helper(int n_years, NumericVector lambda,
                        NumericVector weights, NumericVector forc_vals, double F0) {
  NumericVector out(n_years);

  for (int t = 1; t <= n_years; t++) {
    for (int s = 1; s <= t; s++) {
      double response = 0;
      for(int k = 1; k <= lambda.size(); k++) {
        response += weights(k - 1) * pow(M_E, -(t - s + 0.5) * lambda(k - 1));
      }

      out(t - 1) +=  response * (forc_vals(s - 1) + F0);
    }
  }
  return out;
}


//' Simple C++ code for matrix-vector multiplication
//' @param m matrix
//' @param v vector
//' @return matrix product (but in a fast way)
//' @export
// [[Rcpp::export]]
NumericVector my_mm(NumericMatrix m, NumericVector v){
      int nRow = m.rows();
      int nCol = m.cols();
      NumericVector ans(nRow);
      double v_j;
      for(int j = 0; j < nCol; j++){
        v_j = v[j];
        for(int i = 0; i < nRow; i++){
          ans[i] += m(i,j) * v_j;
        }
      }
      return(ans);
    }
