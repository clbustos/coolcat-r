#' Detect bad fitting points on data
#' @param xx partition object
#' @param m percentage of data to refit
#' @param first for first element to scan
#' @param last for last element to scan
#' @return indexes for bad fit data
#' @export

detect.bad.points<-function(xx,m,indexes=1:nrow(xx$data)) {
    if(m<=0) {
        return(c())
    }
    p.is<-pointFittingProbability(xx,indexes)
    fit.o<-order(p.is)
    selected<-fit.o[1:ceiling(length(indexes)*m)]
    selected
}
#detect.bad.points(xx,0.2)
