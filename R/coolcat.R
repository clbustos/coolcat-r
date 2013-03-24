#' Entropy for a vector. 
#' Omits NA. If you consider NA as a value itself, 
#' use is.na to deactivate it.
#' @param vector
#' @return total entropy for vector
#' @useDynLib coolcat entropy_
#' @export
entropy<-function(x) {
    x<-factor(x)
    .Call("entropy_",x);
}
#' Entropy, R bases
#' Works fine, but is slow!
entropy.r<-function(x) {
pp<-table(factor(x))/length(x)
-sum(pp*log2(pp))
}


#'  Multivariate entropy
#' E(X)=\sum_i E(X_i)
#' @useDynLib coolcat multivariate_entropy
#' @export
multivariateEntropy<-function(x) {
  if(is.factor(x)) {
    return(entropy(x))
  }
  return(.Call("multivariate_entropy",x))
#  t=0.0  
#  for(i in 1:ncol(x)) {
#    t=t+entropy(x[,i])
#  }
#  t
}

multivariateEntropy.r<-function(x) {
  t=0.0  
  for(i in 1:ncol(x)) {
    t=t+entropy.r(x[,i])
  }
  t
}

#' Expected entropy
#' @param x data.frame
#' @param vector with clustering
#' @export

expectedEntropy<-function(x,cluster) {
  clus<-factor(cluster)
  n.r<-nrow(x)
  x.2<-split(x,clus)
  p.me<-sapply(x.2,function(xx) {multivariateEntropy(xx)*nrow(xx)/n.r})
  sum(p.me)
}
#' Expected entropy for a partition object
#' @param x partition object
#' @export
expectedEntropy.partition<-function(x) {
    expectedEntropy(x$data,x$clustering)
}
#' Percentages or frequency for attribute, ordered by cluster
#' @param x partition object
#' @param dd data.frame with original data. If not used, use the data from partition object
#' @param total if true, returns counts, not percentages (useful for X^2 tests)
#' @return a list. Every dataframe with percentages has the name of the attribute
#' @export

attributesTable.partition<-function(x,dd=NULL,values="percentage") {
    if(is.null(dd)) {
        dd<-as.data.frame(x$data)
    }
    f.table<-switch(values,percentage=as.data.frame,frequency=as.table)
    clusters<-split(dd,x$clustering)
    n.clusters<-names(clusters)
    out<-list()
    for(i in colnames(dd)) {
      vals<-unique(dd[[i]])
      out.p<-matrix(0,length(n.clusters),length(vals))
      rownames(out.p)<-n.clusters
      colnames(out.p)<-c(as.character(vals))
      for(j in 1:length(n.clusters)) {
        n.cluster<-n.clusters[[j]]
        clust<-clusters[[n.cluster]]
        n.clust=switch(values,
            percentage=nrow(clust),frequency=1
        )
        #print(n.clust)
        
        for(k in 1:length(vals)) {
            v.k<-as.character(vals[k])
            out.p[j,k]<- sum(as.character(clust[[i]])==v.k) / n.clust
        }
        out[[i]]<-f.table(out.p)
      }
    }
    attr(out,"clusters")<-n.clusters
    return(out)
}

#' Calculates the category utility function
#' Sums the differences between conditional and marginal probabilities
#' for each attribute
#' @param x partition object
#' @export
categoryUtilityFunction<-function(x) {
    at<-attributesTable.partition(x,values="frequency")
    at.s<-names(at)
    n.clusters<-length(unique(x$clustering))
    total<-0
    for(i in at.s) {
        at.a<-at[[i]]
        n<-sum(at.a)
        # probabilidad del cluster
        p.c<-margin.table(at.a,1)/n
        # probabilidad condicional
        p.avc<-prop.table(at.a,1)
        # probabilidad marginal
        p.av <-margin.table(at.a,2)/n
        # matrix version
        p.av.m<-(matrix(1,n.clusters,1)%*%p.av)
        p.uf<-sum(p.c%*%(p.avc^2-p.av.m^2))
        total<-total+p.uf
    }
    total
}


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
coolcat<-function(x,k,batch=100,m.replacement=0.1,final.refitting=T,trace.log=F) {
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
          cat(jj,",",ii,":")
          v<-out[out[,1]==jj & out[,2]==ii,]
          cat(v,"\n")
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


#' Detect bad fitting points on data
#' @param xx partition object
#' @param m percentage of data to refit
#' @return indexes for bad fit data
#' @export
detect.bad.points<-function(xx,m,first=1,last=nrow(xx$data)) {
    if(m<=0) {
        return(c())
    }
    n.row<-nrow(xx$data)
    at<-attributesTable.partition(xx)
    p.is<-numeric(n.row)
    k=xx$k
    n.clusters<-attr(at,"clusters")
    vals<-names(at)
    i.f<-
    for(i in first:last) {
        c.i<-xx$clustering[i]
        if(c.i==0) {
            p.is[i]<- -99
            next;
        }
        p.i<-0
        for(j in 1:length(vals)) {
            n.val<-vals[k]
            tabl<-at[[n.val]]
            p.i<-p.i+tabl[n.clusters==c.i, colnames(tabl)==xx$data[i,j]]
            #cat(i," ")
        }
        p.is[i]<-p.i
    }
    m.p<-max(p.is)+1
    p.is[p.is==-99]<- m.p # don't mark unfitted items
    if(first>1) {p.is[1:(first-1)]<-m.p}
    if(last<nrow(xx$data)) {p.is[(last+1):nrow(xx$data)]<-m.p}
    fit.o<-order(p.is)
    selected<-fit.o<ceiling((last-first+1)*m)
    #print(fit.o)
    which(selected)
}

fit.point<-function(xx,i) {
    k<-xx$k
    t.clusters<-c(xx$clustering[xx$clustering!=0],0)

    t.data<-xx$data[c(which(xx$clustering!=0),i),]
    i.i<-nrow(t.data)
    en.min<-100000
    j.min<-0
    for(j in 1:k) {
      t.clusters[i.i]<-j
      en.clus<-expectedEntropy(t.data, t.clusters)
      #cat("Cluster ",j,":",en.clus,"\n")
      if(en.clus<en.min) {
        en.min<-en.clus
        j.min<-j
      }
      ## Todos los del cluster, más el que estoy analizando
    }
    j.min
}

list.clusters<-function(x) {
  return(unique(x$clustering[x$clustering!=0]))
}
get.cluster<-function(x,c) {
  x$data[x$clustering==c,]
}
