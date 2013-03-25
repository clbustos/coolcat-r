#' Point fitting probability
#' Sum all the p for each attribute on a case
#' @param xx coolcat object
#' @param first index of case to start
#' @param last index of case to finnish

pointFittingProbability<-function(xx,first=1,last=nrow(xx$data)) {
    
    clusters=factor(xx$clustering)
    clusters.c=as.character(xx$clustering)

    n.row<-nrow(xx$data)
    at<-lapply(tableByFactor(xx$data,clusters),log)
    p.is<-numeric(n.row)
    
    k=xx$k
    n.clusters<-levels(clusters)
    vals<-names(at)
    for(i in first:last) {
        c.i<-clusters.c[i]
        if(c.i=="0") {
            p.is[i]<- -99
            next;
        }
        p.i<-0
        for(j in 1:length(vals)) {
            if(is.na(xx$data[i,j])) {
                next;
            }
            n.val<-vals[j]
            tabl<-at[[n.val]]
            #print(tabl)
            p.i<-p.i+tabl[n.clusters==c.i, colnames(tabl)==as.character(xx$data[i,j])]
            #cat(i,":",j,"(",n.val,")=",p.i,"\n")
        }
        p.is[i]<-p.i
    }
    print(p.is)
    m.p<-max(p.is)+1
    p.is[p.is==-99]<- m.p # don't mark unfitted items
    if(first>1) {p.is[1:(first-1)]<-m.p}
    if(last<nrow(xx$data)) {p.is[(last+1):nrow(xx$data)]<-m.p}
    p.is
}
