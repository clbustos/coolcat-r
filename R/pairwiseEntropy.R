# 'Pairwise Entropy
# 'Slow....

pairwiseEntropy<-function(x) {
  n<-nrow(x)
  out<-matrix(0,n,n)
  for(i in 1:(n-1)) {
  cat("i:",i);
    for(j in (i+1):n ) {
      cat(j,",");
      en.m.l<-multivariateEntropy(x[c(i,j),])
      out[i,j]<-en.m.l
      out[j,i]<-en.m.l
    }
    cat("\n");
}
out
}
