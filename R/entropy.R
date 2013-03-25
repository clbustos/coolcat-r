#' Entropy for a vector. 
#' Omits NA. If you consider NA as a value itself, 
#' use is.na to deactivate it.
#' @param x factor 
#' @return total entropy for vector
#' @useDynLib coolcat entropy_
#' @export
entropy<-function(x) {
    x<-factor(x)
    .Call("entropy_",x);
}
#' Entropy, R bases
#' Works fine, but is slow!
#' @param x factor 
entropy.r<-function(x) {
pp<-table(factor(x))/length(x)
-sum(pp*log2(pp))
}
