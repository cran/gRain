// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// propagateLS__
List propagateLS__(List cqpotList_, List rip);
RcppExport SEXP _gRain_propagateLS__(SEXP cqpotList_SEXP, SEXP ripSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type cqpotList_(cqpotList_SEXP);
    Rcpp::traits::input_parameter< List >::type rip(ripSEXP);
    rcpp_result_gen = Rcpp::wrap(propagateLS__(cqpotList_, rip));
    return rcpp_result_gen;
END_RCPP
}
// sparse_setXtf1
SEXP sparse_setXtf1(SEXP XX_, SEXP TF_);
RcppExport SEXP _gRain_sparse_setXtf1(SEXP XX_SEXP, SEXP TF_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type XX_(XX_SEXP);
    Rcpp::traits::input_parameter< SEXP >::type TF_(TF_SEXP);
    rcpp_result_gen = Rcpp::wrap(sparse_setXtf1(XX_, TF_));
    return rcpp_result_gen;
END_RCPP
}
