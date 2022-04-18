// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// timesTwo
int timesTwo(int x);
RcppExport SEXP _ClimBayes_timesTwo(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(timesTwo(x));
    return rcpp_result_gen;
END_RCPP
}
// setup_sigma
NumericMatrix setup_sigma(int N, NumericVector lambda, NumericVector weights, double deltat, double noise_factor, double o);
RcppExport SEXP _ClimBayes_setup_sigma(SEXP NSEXP, SEXP lambdaSEXP, SEXP weightsSEXP, SEXP deltatSEXP, SEXP noise_factorSEXP, SEXP oSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< double >::type deltat(deltatSEXP);
    Rcpp::traits::input_parameter< double >::type noise_factor(noise_factorSEXP);
    Rcpp::traits::input_parameter< double >::type o(oSEXP);
    rcpp_result_gen = Rcpp::wrap(setup_sigma(N, lambda, weights, deltat, noise_factor, o));
    return rcpp_result_gen;
END_RCPP
}
// ode_helper
NumericVector ode_helper(int n_years, NumericVector lambda, NumericVector weights, NumericVector forc_vals, double F0);
RcppExport SEXP _ClimBayes_ode_helper(SEXP n_yearsSEXP, SEXP lambdaSEXP, SEXP weightsSEXP, SEXP forc_valsSEXP, SEXP F0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n_years(n_yearsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type forc_vals(forc_valsSEXP);
    Rcpp::traits::input_parameter< double >::type F0(F0SEXP);
    rcpp_result_gen = Rcpp::wrap(ode_helper(n_years, lambda, weights, forc_vals, F0));
    return rcpp_result_gen;
END_RCPP
}
// my_mm
NumericVector my_mm(NumericMatrix m, NumericVector v);
RcppExport SEXP _ClimBayes_my_mm(SEXP mSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(my_mm(m, v));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ClimBayes_timesTwo", (DL_FUNC) &_ClimBayes_timesTwo, 1},
    {"_ClimBayes_setup_sigma", (DL_FUNC) &_ClimBayes_setup_sigma, 6},
    {"_ClimBayes_ode_helper", (DL_FUNC) &_ClimBayes_ode_helper, 5},
    {"_ClimBayes_my_mm", (DL_FUNC) &_ClimBayes_my_mm, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_ClimBayes(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}