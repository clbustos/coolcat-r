#include "entropy.h"
// Requires a dataframe
SEXP multivariate_entropy_(SEXP x) {
    if(!isFrame(x)) {
        error("Should be a dataframe");
    }
    SEXP el,res,total_entropy;
    double t=0.0;
    PROTECT(total_entropy = NEW_NUMERIC(1));
    // length refiere a la cantidad de elementos
    for(int i=0; i<length(x);i++) {
        el=VECTOR_ELT(x,i);
        if(!isFactor(el)) {
            //printf("Elemento %d no es factor\n",i);
        } else {
            res=entropy_(el);
            //printf("Entropia elemento %d:%0.4f\n",i,REAL(res)[0]);
            t+=REAL(res)[0];
        }
    }
    NUMERIC_POINTER(total_entropy)[0]=t;
    UNPROTECT(1);
    return(total_entropy);
}
