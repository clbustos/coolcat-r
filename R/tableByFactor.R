#' Percentages or frequency for attribute on a data.frame, ordered by factor
#' @param x dataframe
#' @param f factor which divide the data.frame
#' @param values percentage or frequency
#' @return a list. Every dataframe with percentages has the name of the attribute
#' @export

tableByFactor<-function(x,f,values="percentage") {
    table.p<-function(f,xx) {prop.table(table(f,xx),1)}
    f.table<-switch(values,percentage=table.p, frequency = table)
    n.clusters<-levels(f)
    out<-lapply(x,function(xx) {
      f.table(f,xx)
    })
    attr(out,"clusters")<-n.clusters
    return(out)
}
