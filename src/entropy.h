#ifndef ENTROPY_H
#define ENTROPY_H
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/BLAS.h>
#endif /* ENTROPY_H */
SEXP entropy_(SEXP x);
double entropy(int *values, int n, int levels);
SEXP conditional_entropy(SEXP y, SEXP x);
SEXP multivariate_entropy(SEXP x);
