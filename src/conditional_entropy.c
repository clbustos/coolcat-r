#include "entropy.h"

// Conditional entropy of Y conditioned on variable X
SEXP conditional_entropy_(SEXP y, SEXP x) {
	SEXP total_entropy;
	double total_entropy_d=0.0;
	if(!isFactor(x) || !isFactor(y)) {
       error("Both variables should be factors");
    }
    if(length(x)!=length(y)) {
		error("Both variables should have same length");
	} 
	int i=0;
	int n=length(x);
	int n_calc=0;
	int x_act=0;
	//printf("Categories on x: %d; length:%d\n",nlevels(x),length(x));
	// We create a two dim table
	int *table_vals=(int *)R_alloc(length(y)*nlevels(x), sizeof(int));
	int *table_sizes=(int *)R_alloc(nlevels(x), sizeof(int));
	for(i=0;i<nlevels(x);i++) {
		table_sizes[i]=0;
	}
	for(i=0;i<length(y)*nlevels(x);i++) {
		table_vals[i]=0;
	}
	// Store values for partial entropies 
	//puts("Valores:");
	for(i=0;i<n;i++) {
		if(INTEGER(y)[i]!=NA_INTEGER && INTEGER(x)[i]!=NA_INTEGER) {
			x_act=INTEGER(x)[i];
		//	printf("(%d->%d)",x_act,INTEGER(y)[i]);
            table_vals[(x_act-1)*n + table_sizes[x_act-1]]=INTEGER(y)[i];
            table_sizes[x_act-1]++;
            n_calc++;
        }
	}
	int j=0;
	//for(i=0;i<nlevels(x);i++) {
	//	printf("\nElements on %d category:%d\n",i,table_sizes[i]);
	//	for(j=0;j<table_sizes[i];j++) {
	//		printf("%d ",table_vals[i*n + j]);
	//	}
	//}
	//puts("\n");
	int base_level=0;
	// Calculate partial entropies 
	double p;
	for(i=0;i<nlevels(x);i++) {
		base_level=i*n;
		p = (double) table_sizes[i] / (double)n_calc;
		//printf("Entropia nivel %d:%0.4f -> %0.4f\n",i,entropy(&table_vals[base_level], table_sizes[i], nlevels(y)), p);
		//printf("Parcial %0.4f\n",p*entropy(&table_vals[base_level], table_sizes[i], nlevels(y)));
		total_entropy_d+=  p * entropy(&table_vals[base_level], table_sizes[i], nlevels(y)) ;
		//printf("Total acumulado:%0.4f",total_entropy_d);
	}
	
	PROTECT(total_entropy = NEW_NUMERIC(1));
	REAL(total_entropy)[0]=total_entropy_d;
    UNPROTECT(1);
    return(total_entropy);
}
