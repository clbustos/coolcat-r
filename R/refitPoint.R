#' Detects bad categorized points
#' and returns new clustering
#' @export
refitPoint<-function(xx,indexes=1:nrow(xx$data),m.replacement=0.1,trace.log=F) {
      if(trace.log) {cat("Searching for low probability points\n")}
      bad.points<-detectBadPoints(xx,m.replacement,indexes)
      if(trace.log) {cat(length(bad.points)," points located\n")}
      original.clust<-xx$clustering[bad.points]
      new.clust<-integer(length(bad.points))
      for(i in 1:length(bad.points)) {
        new.clust[i]<-fitPoint(xx,bad.points[i])
      }
      relocated<-sum(original.clust!=new.clust)
      if(trace.log) {cat(relocated," points re-located\n")}
      out<-xx$clustering
      
      out[bad.points]=new.clust
      o<-out[indexes]
      attr(o,"relocated")<-relocated
      o
}
