#' Point fitting probability
#' Calculates the log of conditional probability for each case
#' given the clustering factor.
#
#' @param xx coolcat object
#' @param first index of case to start
#' @param last index of case to finnish
#' @export
pointFittingProbability<-function(xx, indexes=1:nrow(xx$data)) {
    clusters=factor(xx$clustering)
    # cluster como caracter
    clusters.c=as.character(xx$clustering)
    clusters.cs=clusters.c[indexes]
    n.row<-nrow(xx$data)
    at<-lapply(tableByFactor(xx$data,clusters),log)
    m.p<-1

    p.is<-rep(m.p,n.row)
    
    k=xx$k
    c.data=lapply(xx$data[indexes,],as.character)
    out<-matrix(0,length(indexes),length(names(at)))
    # Tengo la matriz de datos. Necesito reemplazar en cada uno el valor
    # de la probabilidad que corresponda
    
    for(i.var in 1:length(names(at))) {
      n.var=names(at)[i.var]
      
      data.var=c.data[[n.var]]
      at.var<-at[[n.var]]
      rn.at.var=rownames(at.var)
      cn.at.var=colnames(at.var)
      for(i.clus in 1:length(rn.at.var)) {
        n.clus=rn.at.var[i.clus]
        for(i.val in 1:length(cn.at.var)) {
          n.val=cn.at.var[i.val]
          t.val=at.var[i.clus,i.val]
          #print(data.var==n.val)
          out[clusters.cs==n.clus & data.var==n.val, i.var]<-t.val
        }
      }
    }
    p.is.pre<-rowSums(out)
    p.is[indexes]<-p.is.pre
    p.is[clusters.c=="0"]<-m.p
    p.is
}
