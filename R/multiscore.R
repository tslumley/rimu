
as.ms<-function(x,...) UseMethod("as.ms")


as.ms.data.frame<-function(x,...,na.rm=TRUE){        
    x<-as.matrix(x)
    if (na.rm){
        x[is.na(x)]<-0
     }
    class(x)<-"ms"
    x
 }


levels.ms<-function(x,...) colnames(x)

"levels<-.ms"<-function(x, value) {
  colnames(x)<-value
  x
}

as.logical.ms<-function(x,...) {unclass(x)>0}

as.numeric.ms<-function(x,...){
  unclass(x)
}

as.mr.ms<-function(x,...) {
  x<-as.logical(x)
  class(x)<-"mr"
  x
}

as.ms.mr<-function(x,...) {
  x<-unclass(x)+0
  class(x)<-"ms"
  x
}


ms_na<-function(x){
  y<-as.numeric(x)
  y[is.na(y)]<-0
  levels(y)<-levels(x)
  class(y)<-"ms"
  y
}
    
as.character.ms<-function(x){
  levels<-levels(x)
  y<-as.character(unclass(x))
  x[unclass(x)==0]<-"."
  noquote(unclass(x))
}

print.ms<-function(x,...) print(as.character(x))

as.ms.ms<-function(x,...) x
as.ms.default<-function(x,...) as.ms(as.mr(x))

"[.ms"<-function(x,i,j,...){
  levels<-levels(x)
  x<-unclass(x)[i,j,drop=FALSE]
  new_levels<-levels[j]
  class(x)<-"ms"
  levels(x)<-new_levels
  x
}


length.ms<-function(x) NROW(x)


ms_reorder<-function(x, v, fun=median){
  values<-apply(x, 2, function(xi) fun(v[xi]))
  x<-x[,order(values)]
  x
}
ms_inseq<-function(x){
  x<-x[,order(colnames(x))]
  x
}
ms_inorder<-function(x){
  pos<-apply(as.logical(x),2, function(xi) min(which(xi)))
  x<-x[,order(pos)]
  x
}
ms_infreq<-function(x){
  freqs<-colSums(x>0)  
  x<-x[,order(-freqs)]
  x
}

sum0<-function(x) -sum(x[x>0],na.rm=TRUE)
mean0<-function(x) {x = x[x>0]; if (length(x)) mean(x) else 0}

ms_inscore<-function(x, fun=sum0){
  freqs<-apply(x,2,fun)  
  x<-x[,order(freqs)]
  x
}

stack.ms<-function(x,...,na.rm=FALSE){
  levels<-levels(x)
  x<-unclass(x)
  x[is.na(x)]<-!na.rm
  r<-rowSums(x>0)
  values<-do.call(c,lapply(seq_len(NROW(x)),function(i) levels[x[i,]]))
  id<-rep(seq_len(NROW(x)),r)
  s<-as.numeric(t(unclass(x)))
  data.frame(values=factor(values,levels=levels),scores=s[s>0],id)
}

image.ms<-function(x,...){
  image( t(as.logical(x)), axes=FALSE)
  axis(3,at=seq(0,1,length=length(levs)),labels=levs)
  invisible(x)
}
