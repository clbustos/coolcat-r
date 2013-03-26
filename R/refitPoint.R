#' Detects bad categorized points
#' and returns new clasification
 
refitPoint<-function(xx,indexes=1:nrow(xx$data),m.replacement=0.1,trace.log=F) {
      if(trace.log) {cat("Searching for bad points\n")}
      bad.points<-detect.bad.points(xx,m.replacement,indexes)
      if(trace.log) {cat(length(bad.points)," points located\n")}
      original.clust<-xx$clustering
      xx$clustering[bad.points]<-0
      for(i in bad.points) {
        xx$clustering[i]<-fit.point(xx,i)
      }
      if(trace.log) {cat(sum(i.clust[indexes]!=xx$clustering[indexes])," points re-located\n")}
      xx$clustering
  }
