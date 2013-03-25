#' Calculates the category utility function
#' Sums the differences between conditional and marginal probabilities
#' for each attribute
#' @param x partition object
#' @export
categoryUtilityFunction<-function(x) {
    at<-tableByFactor(x$data,x$clustering,values="frequency")
    at.s<-names(at)
    n.clusters<-length(unique(x$clustering))
    total<-0
    for(i in at.s) {
        at.a<-at[[i]]
        n<-sum(at.a)
        # probabilidad del cluster
        p.c<-margin.table(at.a,1)/n
        # probabilidad condicional
        p.avc<-prop.table(at.a,1)
        # probabilidad marginal
        p.av <-margin.table(at.a,2)/n
        # matrix version
        p.av.m<-(matrix(1,n.clusters,1)%*%p.av)
        p.uf<-sum(p.c%*%(p.avc^2-p.av.m^2))
        total<-total+p.uf
    }
    total
}
