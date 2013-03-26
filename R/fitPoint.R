#' Returns the cluster for which the point is better suited.
#' Using expectedEntropy as criteria
#' @param xx partition object
#' @param i number index
#' @return number of conglomerate
#' @export
fitPoint<-function(xx,i) {
    k<-xx$k
    t.clusters<-c(xx$clustering[xx$clustering!=0],0)
    t.data<-xx$data[c(which(xx$clustering!=0),i),]
    i.i<-nrow(t.data)
    en.min<-100000
    j.min<-0
    for(j in 1:k) {
      t.clusters[i.i]<-j
      en.clus<-expectedEntropy(t.data, t.clusters)
      #cat("Cluster ",j,":",en.clus,"\n")
      if(en.clus<en.min) {
        en.min<-en.clus
        j.min<-j
      }
      ## Todos los del cluster, más el que estoy analizando
    }
    j.min
}
