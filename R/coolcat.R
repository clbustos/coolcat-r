

# 'Pairwise Entropy
# 'Slow....

pairwiseEntropy<-function(x) {
  n<-nrow(x)
  out<-matrix(0,n,n)
  for(i in 1:(n-1)) {
  cat("i:",i);
    for(j in (i+1):n ) {
      cat(j,",");
      en.m.l<-multivariateEntropy(x[c(i,j),])
      out[i,j]<-en.m.l
      out[j,i]<-en.m.l
    }
    cat("\n");
}
out
}

#' Distancia de humming entre vectores
#' Es más cómodo utilizar daisy del paquete cluster, que aparte
#' entrega el valor estandarizado

hummingDistance<-function(x,y=NULL) {

if(is.null(y)) {
x<-sapply(x,as.character)
  n<-nrow(x)
  z<-matrix(0,n,n)
  colnames(z)<-rownames(x)
  rownames(z)<-rownames(x)
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      hd<-h.dist(x[i,],x[j,])
      z[i,j]<-hd
      z[j,i]<-hd
    }
  } 
  return(z)
} else {
    j.r<-na.omit(t(rbind(x,y)))
    return(sum(j.r[,1]!=j.r[,2]))
}
}


#' Coolcat!
#' Algoritmo de clustering, basado en la entropia. 
#' Tiene su gracia
#' Reemplazo la búsqueda de desigualdad inicial basado en entropia por
#' una basada en Humming. Estoy seguro que es monotónico creciente
#' la función entre ambas, así que no debería ser problema.
#' @param x dataframe
#' @param k clusters number
#' @param batch batch size for replacement of bad fitting points
#' @param m.replacement proportion of point to re-place after each batch
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

  refitting<-function(i.batch,e.batch) {
      if(trace.log) {cat("Searching for bad points\n")}
      bad.points<-detect.bad.points(xx,m.replacement,first=i.batch,last=e.batch)
      if(trace.log) {cat(length(bad.points)," points located\n")}
      i.clust<-xx$clustering
      xx$clustering[bad.points]<-0
      for(i in which(xx$clustering==0)) {
        if(i<i.batch | i>e.batch) {
            next
        }
        xx$clustering[i]<-fit.point(xx,i)
      }
      if(trace.log) {cat(sum(i.clust[i.batch:e.batch]!=xx$clustering[i.batch:e.batch])," points re-located\n")}
      xx$clustering
  }


  for(c.batch in 1:(ceiling(n.row/batch))) {
    
      i.batch=(c.batch-1)*batch+1
      e.batch=min(c.batch*batch,n.row)
      if(trace.log) {cat("From ",i.batch," to ",e.batch,"\n")}
      for(i in i.batch:e.batch) {
        
        if(xx$clustering[i]==0) {
            xx$clustering[i]<-fit.point(xx,i)
         }
      }
      
      if(m.replacement>0) {
        xx$clustering<-refitting(i.batch,e.batch)
      }
   }
   
   if(final.refitting & m.replacement>0) {
    xx$clustering<-refitting(1,n.row)
   }
  
  
  xx$diss<-dist.1
  xx$silhoutte<-silhouette(xx$clustering,daisy(xx$data))
  xx$objetive<-expectedEntropy(xx$data,xx$clustering)
  class(xx)<-c("coolcat","partition")
  xx
}
