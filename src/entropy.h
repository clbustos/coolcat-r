#ifndef ENTROPY_H
#define ENTROPY_H
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/BLAS.h>
#endif /* ENTROPY_H */
double entropy(int *values, int n, int levels);

SEXP entropy_(SEXP x);
SEXP conditional_entropy_(SEXP y, SEXP x);
SEXP multivariate_entropy_(SEXP x);
SEXP conditional_multivariate_entropy_(SEXP df, SEXP x);
