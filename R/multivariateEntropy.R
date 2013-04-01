#'  Multivariate entropy
#' E(X)=sum_i E(X_i)
#' @useDynLib coolcat multivariate_entropy
#' @export
multivariateEntropy<-function(x) {
  if(is.factor(x)) {
    return(entropy(x))
  }
  return(.Call("multivariate_entropy",x))
#  t=0.0  
#  for(i in 1:ncol(x)) {
#    t=t+entropy(x[,i])
#  }
#  t
}

multivariateEntropy.r<-function(x) {
  t=0.0  
  for(i in 1:ncol(x)) {
    t=t+entropy.r(x[,i])
  }
  t
}
