#' Expected entropy of Y given X, where Y is multivariate
#' Assumes independence of variables on Y
#' @param x data.frame
#' @param cluster factor to clustering
#' @useDynLib coolcat conditional_multivariate_entropy_
#' @export
expectedEntropy<-function(x,cluster) {
    .Call("conditional_multivariate_entropy_",x,cluster);
}

#' Expected entropy of Y given X, where Y is factor
#' Assumes independence of variables on Y
#' @param y factor
#' @param x factor to clustering
#' @useDynLib coolcat conditional_entropy_
expectedEntropy.pairwise<-function(y,x) {
	.Call("conditional_entropy_",y,x);
}


#' R version of conditional/expected entropy
#' Only useful for debug
expectedEntropy.r<-function(x,cluster) {
  #clus<-factor(cluster)
  n.r<-nrow(x)
  x.2<-split(x,cluster)
  p.me<-sapply(x.2,function(xx) { multivariateEntropy(xx)*nrow(xx)/n.r})
  sum(p.me)
}

#' Expected entropy for a partition object
#' @param x partition object
#' @export
expectedEntropy.partition<-function(x) {
    expectedEntropy(x$data,x$clustering)
}

