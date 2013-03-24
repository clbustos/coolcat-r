#include "entropy.h"

SEXP entropy_(SEXP x) {
    
    SEXP total_entropy;
    PROTECT(total_entropy = NEW_NUMERIC(1));
    double p=0.0;
    double total_entropy_d=0.0;
    int i=0;
    int n=length(x);
    int n_calc=0;
    if(!isFactor(x)) {
       error("You should send a factor");
    } 
    int *table=(int *)R_alloc(nlevels(x), sizeof(int));
    for(i=0;i<nlevels(x);i++) {
        table[i]=0;
    }
    int *pX=INTEGER(x);
    for(i=0;i<n;i++) {
        if(pX[i]!=NA_INTEGER) {
            //printf("%d\n",pX[i]);
            table[pX[i]-1]++;
            n_calc++;
        }  
        //printf("%d:%d\n",INTEGER(x)[i], table[INTEGER(x)[i]-1]);
    }
    for(i=0;i<nlevels(x);i++) {
        if(table[i]>0) {
        //printf("%d:%d %d\n",i,table[i],n_calc);
        p=(double)table[i] / n_calc;
        //printf("%0.4f\n",p);
        total_entropy_d+=p*log2(p);
    }
    }
    //printf("Entropia total: %0.4f\n",total_entropy_d);
    REAL(total_entropy)[0]=-total_entropy_d;
    UNPROTECT(1);
    
    return(total_entropy);
}

// Requires a dataframe
SEXP multivariate_entropy(SEXP x) {
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

SEXP test_(SEXP x) {
    int i=0;
    for(i=0;i<length(x);i++) {
        printf("%s\n",CHAR(STRING_ELT(x,i)));
    }
    return(x);
}
