#' Expected entropy
#' @param x data.frame
#' @param vector with clustering
#' @export

expectedEntropy<-function(x,cluster) {
  #clus<-factor(cluster)
  n.r<-nrow(x)
  x.2<-split(x,cluster)
  p.me<-sapply(x.2,function(xx) {multivariateEntropy(xx)*nrow(xx)/n.r})
  sum(p.me)
}

#' Expected entropy for a partition object
#' @param x partition object
#' @export
expectedEntropy.partition<-function(x) {
    expectedEntropy(x$data,x$clustering)
}
