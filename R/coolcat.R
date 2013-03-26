#' Coolcat!
#' Clustering algorithm, entropy based. 
#'
#' @param x dataframe
#' @param k clusters number
#' @param m.replacement proportion of point to re-place after each batch
#' @param batch batch size for replacement of bad fitting points
#' @param final.refitting If TRUE, will do a final refitting of all items
#' @param trace.log If TRUE, writes a log of actions
#' @return a partition object
#' @export
coolcat<-function(x,k,m.replacement=0.1,batch=100,final.refitting=T,trace.log=F) {
  require('cluster')
  dist.1<-daisy(x)
  xx<-list(data=x, clustering=numeric(nrow(x)), k=k, m.replacement=m.replacement)
  m.dist<-as.matrix(dist.1)
  is.na(m.dist)<-!as.logical(as.matrix(dist.1))
  mins.m<-apply(m.dist,1,function(x) {min(x,na.rm=T)})
  #print(mins.m)
  max.mins<-unique(sort(mins.m,decreasing=T)[1:k])
  #print(max.mins)
  # Estos son los puntos de máxima diferencia con todo el resto
  m.points<-numeric(0)
  for(mm in max.mins) {
    m.points<-c(m.points,which(mins.m==mm))
  } 
  
  if(trace.log) {cat("First stage: searching for first ",k," clustering points\n")}
  nm<-length(m.points)
  out<-matrix(0,0.5*(nm-1)*nm,3)
  r<-1
  for(i in 1:(nm-1)) {
    for(j in (i+1):nm) {
      p1<-min(m.points[i],m.points[j])
      p2<-max(m.points[i],m.points[j])
      
      out[r,]<-c(p1,p2,multivariateEntropy(rbind(x[p1,],x[p2,])))
      r<-r+1
    }
  }
  
  ind<-match(max(out[,3]),out[,3])
  max.e<-out[ind,]
  out<-out[-ind,]
  ps<-matrix(0,length(m.points),2)
  ps[,1]<-m.points
  #print(out)
  ps[m.points==max.e[1],2]<-1
  ps[m.points==max.e[2],2]<-2
  #print(ps);
  if(k>2) {
    for(j.act in 3:k) {
    v.max<-0
    p.max<-0
      # Paso por cada punto no elegido
      for(j in ps[ps[,2]==0,1]) {
        minn<-100000000
        # Debo hacerlo pasar por los valores ya elegidos
        for(i in ps[ps[,2]!=0,1]) {
          jj<-min(i,j)
          ii<-max(i,j)
          #cat(jj,",",ii,":")
          v<-out[out[,1]==jj & out[,2]==ii,]
          #cat(v,"\n")
          if(v[3]<minn) {
            minn <- v[3]
          }
        }
        #print(minn)
        if(minn>v.max) {
          p.max=j
          v.max=minn
        }
      }
      #cat("Pmax:",p.max,":vmax:",v.max)
      ps[ps[,1]==p.max,2]<-j.act
      j.act=j.act+1
    }
    }
  ps=ps[ps[,2]!=0,]
  for(i in 1:nrow(ps)) {
    xx$clustering[ps[i,1]]<-ps[i,2]
  }
  # Second stage: Fit points
  if(trace.log) {cat("Second stage: Fitting points\n")}
  c.batch=1
  n.row=nrow(x)
  for(c.batch in 1:(ceiling(n.row/batch))) {
      i.batch=(c.batch-1)*batch+1
      e.batch=min(c.batch*batch,n.row)
      if(trace.log) {cat("From ",i.batch," to ",e.batch,"\n")}
      for(i in i.batch:e.batch) {
        if(xx$clustering[i]==0) {
            xx$clustering[i]<-fitPoint(xx,i)
         }
      }
      
      if(m.replacement>0) {
      #print(xx$clustering)
        xx$clustering[i.batch:e.batch]<-refitPoint(xx,i.batch:e.batch,m.replacement,trace.log=trace.log)
        #print(xx$clustering)
      }
   }
   
   if(final.refitting & m.replacement>0) {
    xx$clustering<-refitPoint(xx,m.replacement=m.replacement,trace.log=trace.log)
   }
   
  #print(xx$clustering)
  
  xx$diss<-dist.1
  xx$silhoutte<-silhouette(xx$clustering,daisy(xx$data))
  xx$objetive<-expectedEntropy(xx$data,xx$clustering)
  class(xx)<-c("coolcat","partition")
  xx
}
#' Plot coolcat object
#' @param x coolcat object
#' @export
plot.coolcat<-function(x,...) {
    devAskNewPage(T)
    clusplot(x,...);
    plot(x$silhoutte,...);
    devAskNewPage(F)
}
