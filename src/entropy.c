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
// Conditional entropy of Y conditioned on variable X
SEXP conditional_entropy(SEXP y, SEXP x) {
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
