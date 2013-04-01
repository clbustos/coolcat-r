#include "entropy.h"
// Conditional entropy of multivariate Y conditioned on variable X
// Assume independence of Y variables
SEXP conditional_multivariate_entropy_(SEXP df, SEXP x) {
    SEXP el,res,total_entropy;
    if(!isFactor(x)) {
       error("Group variable should be factor");
    }
    if(!isFrame(df)) {
        error("First parameter should be a data.frame");
    }
    double t=0.0;
    PROTECT(total_entropy = NEW_NUMERIC(1));
    // length refiere a la cantidad de elementos
    for(int i=0; i<length(df);i++) {
        el=VECTOR_ELT(df,i);
        if(!isFactor(el)) {
            //printf("Elemento %d no es factor\n",i);
        } else {
            res=conditional_entropy_(el,x);
            //printf("Entropia elemento %d:%0.4f\n",i,REAL(res)[0]);
            t+=REAL(res)[0];
        }
    }
    NUMERIC_POINTER(total_entropy)[0]=t;
    UNPROTECT(1);
    return(total_entropy);
    return(x);
}
