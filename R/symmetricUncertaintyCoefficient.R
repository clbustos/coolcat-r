#' Symmetrical measure of uncertainty
#' between two categorical variables (factor).
#'
#' Removes NA from both factors
#' @param x factor
#' @param y factor
#' @return real value
#' @export
symmetricUncertaintyCoefficient<-function(x,y) {
    d.f<-na.omit(data.frame(x,y))
    x.y<-factor(paste(d.f$x,d.f$y))
    h.x<-entropy(d.f$x)
    h.y<-entropy(d.f$y)
    2*(h.x+h.y-entropy(x.y))/(h.x+h.y)
}
