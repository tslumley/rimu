
as.ms<-function(x,...) UseMethod("as.ms")

as.data.frame.ms<-function(x,...) {as.data.frame(unclass(x))}

as.ms.list<-function(x,...,levels=NULL){
    levs<-unique(do.call(c,x))
    if (!is.null(levels)){
        if (any(xtra<-setdiff(levs,levels)))
            warning(paste("values not in 'levels' ",paste(xtra,collapse=", ")))
        levs<-levels
    }
    m<-matrix(0,nrow=length(x),ncol=length(levs))
    for(i in seq_along(x)){
        l<-match(x[[i]],levs)
        if (any(l))
            m[i,l]<-seq_len(length(l))
    }
    colnames(m)<-levs
    class(m)<-"ms"
    m
}

as.ms.data.frame<-function(x,...,na.rm=TRUE){        
    x<-as.matrix(x)
    if(!is.numeric(x)) stop("must be numeric")
    if (na.rm){
        x[is.na(x)]<-0
     }
    class(x)<-"ms"
    x
 }

as.ms.matrix<-function(x,...,na.rm=TRUE){
    if(!is.numeric(x)) stop("must be numeric")
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
    
as.character.ms<-function(x,...){
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
  if (!missing(j)){
      if (is.character(j))
          new_levels<-j
      else
          new_levels<-levels[j]
  } else
      new_levels<-levels
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

mean0<-function(y) {y = y[y>0]; if (length(y)) mean(y) else 0}

ms_inscore<-function(x, fun=mean0){
  freqs<-apply(x,2,fun)  
  x<-x[,order(freqs)]
  x
}



ms_flatten<-function(x, priorities, fun, start=0){
    if (!is.function(fun))
        fun <- get(fun,mode="function")
    if (is.null(priorities))
        priorities<-levels(x)
    y<-rep(start,length=length(x))
    nm<-rep(NA_character_, nrow(x))
    for(l in rev(priorities)){
        i<-!(x[,l] %in% 0)
        y[i]<-fun(as.vector(x[,l])[i],y[i])
        nm[x %has% l]<-l
    }
    names(y)<-nm
    y
}

ms_recode<-function(x, ...){
    new<-list(...)
    newlevs<-names(new)
    deadlevs<-unlist(new)
    levs<-levels(x)
    if(!all(deadlevs %in% levs)){
        stop(paste("non-existent levels",deadlevs[!(deadlevs %in% levs)]))
    }
    levs[match(deadlevs,levs)]<-newlevs
    levels(x)<-levs
    x
}


ms_drop<-function(x, levels){
    if(!all(levels %in% levels(x))){
        stop(paste("non-existent levels:", levels[!(levels %in% levels(x))]))
    }
    x[,!(levels(x) %in% levels)]
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
    levs<-levels(x)
    axis(3,at=seq(0,1,length=length(levs)),labels=levs)
    invisible(x)
}
