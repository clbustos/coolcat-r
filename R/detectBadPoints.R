#' Detect bad fitting points on data
#' @param xx partition object
#' @param m percentage of data to refit
#' @param index for first element to scan
#' @param index for last element to scan
#' @return indexes for bad fit data
#' @export

detect.bad.points<-function(xx,m,first=1,last=nrow(xx$data)) {
    if(m<=0) {
        return(c())
    }
    p.is<-pointFittingProbability(xx,m,first,last)
    fit.o<-order(p.is)
    selected<-fit.o[1:ceiling((last-first+1)*m)]
    selected
}
#detect.bad.points(xx,0.2)
