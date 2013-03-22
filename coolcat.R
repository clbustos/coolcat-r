#entropia
en<-function(x) {
pp<-table(factor(x))/length(x)
-sum(pp*log2(pp))
}
#entropia multivariada
en.m<-function(x) {
  sum(apply(x,2,en))
}
# expected entropy
ex.en<-function(x,cluster) {
  clus<-factor(cluster)
  n=dim(x)[1]
  t<-0
  for(k in levels(clus)) {
    t<-t+(sum(clus==k)/n)*en.m(x[clus==k,])
  }
  t
}

pairwise.entropy<-function(x) {
  n<-nrow(x)
  out<-matrix(0,n,n)
  for(i in 1:(n-1)) {
    for(j in (i+1):n ) {
      en.m.l<-en.m(x[c(i,j),])
      out[i,j]<-en.m.l
      out[j,i]<-en.m.l
    }

}
out
}

#' Distancia de humming entre vectores
#' Es más cómodo utilizar daisy del paquete cluster, que aparte
#' entrega el valor estandarizado

h.dist<-function(x,y=NULL) {

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
#' Funcion que genera una lista de todos los cruces.
#' Puede ser útil
matrix.as.ol<-function(x) {
  n<-ncol(x)
  d.f<-expand.grid(c=1:n,r=1:n)
  d.f$vals<-as.numeric(x)
  d.f<-d.f[d.f$c>d.f$r,]
  d.f
}


#' Coolcat!
#' Algoritmo de clustering, basado en la entropia. 
#' Tiene su gracia
#' Reemplazo la búsqueda de desigualdad inicial basado en entropia por
#' una basada en Humming. Estoy seguro que es monotónico creciente
#' la función entre ambas, así que no debería ser problema.
#' @param x dataframe
#' @param k numero de clusters
coolcat<-function(x,k) {
  require('cluster')
  dist.1<-daisy(x)
  
  xx<-list(data=x, clusters=numeric(nrow(x)))
  
  m.dist<-as.matrix(dist.1)
  is.na(m.dist)<-!as.logical(as.matrix(dist.1))
  mins.m<-apply(m.dist,1,function(x) {min(x,na.rm=T)})
  max.mins<-unique(sort(mins.m,decreasing=T)[1:k])
  # Estos son los puntos de máxima diferencia con todo el resto
  m.points<-numeric(0)
  for(mm in max.mins) {
    m.points<-c(m.points,which(mins.m==mm))
  } 
  nm<-length(m.points)
  out<-matrix(0,0.5*(nm-1)*nm,3)
  r<-1
  for(i in 1:(nm-1)) {
    for(j in (i+1):nm) {
      p1<-min(m.points[i],m.points[j])
      p2<-max(m.points[i],m.points[j])
      
      out[r,]<-c(p1,p2,en.m(rbind(x[p1,],x[p2,])))
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
            minn<-v[3]
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
  # Segunda etapa. Punto a punto
  
  for(i in 1:nrow(x)) {
    if(xx$clustering[i]!=0) {
      next
    }

    t.clusters<-xx$clustering    
    t.data<-xx$data[t.clusters!=0,]
    en.min<-100000
    j.min<-0
    for(j in 1:k) {
      t.clusters[i]<-j
      en.clus<-ex.en(t.data,t.clusters)
      if(en.clus<en.min) {
        en.min<-en.clus
        j.min<-j
      }
      ## Todos los del cluster, más el que estoy analizando
    }
    cat("Punto ",i," en cluster ",j.min,"\n")
    xx$clustering[i]<-j.min
  }
  xx$diss<-dist.1
  xx$silhoutte<-silhouette(xx$clustering,daisy(xx$data))
  xx
}

list.clusters<-function(x) {
  return(unique(x$clustering[x$clustering!=0]))
}
get.cluster<-function(x,c) {
  x$data[x$clustering==c,]
}

