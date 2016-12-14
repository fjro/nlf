// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// linearCpp
NumericVector linearCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_linearCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(linearCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// quadraticCpp
NumericVector quadraticCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_quadraticCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(quadraticCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// cubicCpp
NumericVector cubicCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_cubicCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(cubicCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// qrootCpp
NumericVector qrootCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_qrootCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(qrootCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// exponentialCpp
NumericVector exponentialCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_exponentialCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(exponentialCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// logECpp
NumericVector logECpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_logECpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(logECpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// sigmoidCpp
NumericVector sigmoidCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_sigmoidCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(sigmoidCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// stepCpp
NumericVector stepCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_stepCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(stepCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// spikeCpp
NumericVector spikeCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_spikeCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(spikeCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// sinLowCpp
NumericVector sinLowCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_sinLowCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(sinLowCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// sinHighCpp
NumericVector sinHighCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_sinHighCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(sinHighCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// linearPeriodicCpp
NumericVector linearPeriodicCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_linearPeriodicCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(linearPeriodicCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// varyingFreqCpp
NumericVector varyingFreqCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_varyingFreqCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(varyingFreqCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// circleCpp
NumericVector circleCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_circleCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(circleCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
// xShapedCpp
NumericVector xShapedCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n);
RcppExport SEXP nlf_xShapedCpp(SEXP xSEXP, SEXP noiseSEXP, SEXP noiseLevelSEXP, SEXP numNoiseSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< float >::type noiseLevel(noiseLevelSEXP);
    Rcpp::traits::input_parameter< int >::type numNoise(numNoiseSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    __result = Rcpp::wrap(xShapedCpp(x, noise, noiseLevel, numNoise, n));
    return __result;
END_RCPP
}
