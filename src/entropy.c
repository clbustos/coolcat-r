#include "entropy.h"

SEXP entropy_(SEXP x) {
    SEXP total_entropy;
    PROTECT(total_entropy = NEW_NUMERIC(1));

    if(!isFactor(x)) {
       error("You should send a factor");
    }
    
    REAL(total_entropy)[0]=entropy(INTEGER(x), length(x), nlevels(x));
    UNPROTECT(1);
    
    return(total_entropy);
}

double entropy(int *values, int n, int levels) {
//	printf("Entropia, %d con %d niveles", n, levels);
	int i=0;
	double p=0.0;
	int *table=(int *)R_alloc(levels, sizeof(int));
	double total_entropy_d=0.0;
    for(i=0;i<levels;i++) {
        table[i]=0;
    }
    int n_calc=0;
    for(i=0;i<n;i++) {
		if(values[i]!=NA_INTEGER) {
            //printf("%d ",values[i]);
            table[values[i]-1]++;
            n_calc++;
        }  
    }
    //puts("\n");
    for(i=0;i<levels;i++) {
        if(table[i]>0) {
        //printf("%d:%d %d\n",i,table[i],n_calc);
        p=(double)table[i] / n_calc;
        //printf("%0.4f\n",p);
        total_entropy_d+=p*log2(p);
		}
	}
    return(-total_entropy_d);
}


SEXP test_(SEXP x) {
    int i=0;
    for(i=0;i<length(x);i++) {
        printf("%s\n",CHAR(STRING_ELT(x,i)));
    }
    return(x);
}
