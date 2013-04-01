#' Symmetrical measure of uncertainty
#' between variables on a data.frame or 
#' two categorical variables (factor).
#' Range: 0 (no dependence)- 1(complete dependence)
#' Removes NA from both factors
#' @param x data.frame or first factor
#' @param y second factor
#' @return real value
#' @export
symUncertaintyCoefficient<-function(x,y=NULL) {
    if(inherits(x,"data.frame")) {
        k<-ncol(x)
        k.n<-colnames(x)
        m<-matrix(1,k,k,dimnames=list(k.n,k.n))
        for(i in 1:(k-1)) {
            for(j in (i+1):k) {
                val=symmetricUncertaintyCoefficient(x[,i],x[,j])
                m[i,j]<-val
                m[j,i]<-val
            }
        }
        return(m)
    }
    d.f<-na.omit(data.frame(x,y))
    x.y<-factor(paste(d.f$x,d.f$y))
    h.x<-entropy(d.f$x)
    h.y<-entropy(d.f$y)
    2*(h.x+h.y-entropy(x.y))/(h.x+h.y)
}
