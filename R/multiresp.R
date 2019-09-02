

as.mr.data.frame<-function(x,...,na.rm=TRUE){        
    x<-as.matrix(x)>0
    if (na.rm){
        x[is.na(x)]<-FALSE
     }
    class(x)<-"mr"
    x
}

as.mr.list<-function(x,...,levels=NULL){
    levs<-unique(do.call(c,x))
    if (!is.null(levels)){
        if (any(xtra<-setdiff(levs,levels)))
            warning(paste("values not in 'levels' ",paste(xtra,collapse=", ")))
        levs<-levels
    }
    m<-matrix(FALSE,nrow=length(x),ncol=length(levs))
    for(i in seq_along(x)){
        m[i,]<-levs %in% x[[i]]
    }
    m
}

levels.mr<-function(x,...) colnames(x)

"levels<-.mr"<-function(x, value) {
  colnames(x)<-value
  x
}

as.logical.mr<-function(x,...){
  unclass(x)
}

mr_na<-function(x, na=TRUE){
  y<-as.logical(x)
  y[is.na(y)]<-na
  levels(y)<-levels(x)
  class(y)<-"mr"
  y
}
    
as.character.mr<-function(x,sep="+",na.rm=TRUE){
  levels<-levels(x)
  if (na.rm) {
    x<-unclass(mr_na(x, na=FALSE))
    x<-lapply(seq_len(NROW(x)), function(i) levels[x[i,]])
  } else  {
    z<-unclass(mr_na(x,na=TRUE))
    tmp<-lapply(seq_len(NROW(x)),function(i) ifelse(is.na(x[i,])[z[i,]], paste0("?",levels[z[i,]]), levels[z[i,]]))
    x<-tmp
  }
  sapply(x,paste,collapse=sep)
}

as.data.frame.mr<-function(x,...){
  as.data.frame(unclass(x)+0)
}

"[.mr"<-function(x,i,j,...){
  levels<-levels(x)
  x<-as.logical(x)[i,j,drop=FALSE]
  new_levels<-levels[j]
  class(x)<-"mr"
  levels(x)<-new_levels
  x
}
length.mr<-function(x) NROW(x)


print.mr <-function(x,...,na.rm=FALSE,sep="+"){
  x<-as.character(x,na.rm=na.rm,sep=sep)
  print(x)
}




"%has%"<- function(x,y) {
  x<-as.mr(x)
  if (is.factor(y)) y<-as.character(y)
  if (!is.character(y)) stop('needs to be character or factor')
  if(length(y)==1) y<-rep(y,length(x))
  ifelse(y %in% levels(x), unclass(x)[,match(y,levels(x))] ,FALSE)
}


mr_count<-function(x,na.rm=TRUE) rowSums(x,na.rm=na.rm)

"%hasonly%"<- function(x,y) {
  (x %has% y) & (mr_count(x)==1)
}


mr_union<-function(x,y){
  x<-as.mr(x)
  y<-as.mr(y)
  if (length(x)!=length(y)) 
    stop("different numbers of observations in x and y")
  levs<-unique(c(levels(x),levels(y)))
  m<-matrix(FALSE,nrow=nrow(x),ncol=length(levs))
  colnames(m)<-levs
  m[,levels(x)]<-x
  m[,levels(y)]<-m[,levels(y)] | y
  class(m)<-"mr"
  m
}
mr_diff<-function(x,y){
  x<-as.mr(x)
  y<-as.mr(y)
  if (length(x)!=length(y)) 
    stop("different numbers of observations in x and y")
  if(!all(levels(y) %in% levels(x)))
    stop("levels of y must be a subset of levels of x")
  levs<-levels(x)
  ilevs<-intersect(levs,levels(y))
  m<-matrix(FALSE,nrow=nrow(x),ncol=length(levs))
  colnames(m)<-levs
  m[,levs]<-x[,levs]
  m[,ilevs]<-m[,ilevs] & !y[,ilevs]
  class(m)<-"mr"
  m
}
mr_intersect<-function(x,y){
  x<-as.mr(x)
  y<-as.mr(y)
  if (length(x)!=length(y)) 
    stop("different numbers of observations in x and y")
  levs<-intersect(levels(x),levels(y))
  if (!length(levs)){
    warning("x and y have no levels in common")
  }
  m<-matrix(FALSE,nrow=nrow(x),ncol=length(levs))
  colnames(m)<-levs
  m[,levs]<-x[,levs] & y[,levs]
  class(m)<-"mr"
  m
}

mr_reorder<-function(x, v, fun=median){
  values<-apply(x, 2, function(xi) fun(v[xi]))
  x<-x[,order(values)]
  x
}
mr_inseq<-function(x){
  x<-x[,order(colnames(x))]
  x
}
mr_inorder<-function(x){
  pos<-apply(as.logical(x),2, function(xi) min(which(xi)))
  x<-x[,order(pos)]
  x
}
mr_infreq<-function(x, na.rm=TRUE){
  y<-mr_na(x, na=!na.rm)
  freqs<-colSums(x)  
  x<-x[,order(-freqs)]
  x
}

mr_collapse<-function(x, priorities){
    y<-rep(NA_character_,length(x))
    if (is.null(priorities))
        priorities<-levels(x)
    for(l in rev(priorities)){
        y<-ifelse(x %has% l,l,y)
    }
    factor(y,levels=levels(x))
}

mr_recode<-function(x, ...){
    new<-list(...)
    newlevs<-names(new)
    deadlevs<-unlist(new)
    levs<-levels(x)
    if(!all(deadlevs %in% levs)){
        stop(paste("non-existent levels",setdiff(levs,deadlevs)))
    }
    levs[match(deadlevs,levs)]<-newlevs
    levels(x)<-levs
    x
}


stack.mr<-function(x,...,na.rm=FALSE){
  levels<-levels(x)
  x<-unclass(x)
  x[is.na(x)]<-!na.rm
  r<-rowSums(x)
  values<-do.call(c,lapply(seq_len(NROW(x)),function(i) levels[x[i,]]))
  id<-rep(seq_len(NROW(x)),r)
  data.frame(values=factor(values,levels=levels),id)
}

indicator<-function(x, ...){
  xx<-1L*unclass(x)
  xx
}


as.mr<-function(x,...) UseMethod("as.mr",x)
as.mr.mr<-function(x,...) x
as.mr.default<-function(x,levels=unique(x)){
  rval<-outer(x,levels,"==")
  colnames(rval)<-levels
  class(rval)<-"mr"
  rval
}

as.mr.factor<-function(x,...){
  rval<-outer(x,levels(x),"==")
  colnames(rval)<-levels(x)
  class(rval)<-"mr"
  rval
}

mtable<-function(x,y,na.rm=TRUE){
  x<-as.mr(x)
  if (missing(y)){
    rval<-matrix(colSums(x,na.rm=na.rm),nrow=1)
    dimnames(rval)<-list("",levels(x))
    rval
  } else {
    y<-as.mr(y)
    xx<-unclass(x)
    if(na.rm) xx[is.na(xx)]<-FALSE
    yy<-unclass(y)
    if(na.rm) yy[is.na(yy)]<-FALSE
    rval<-crossprod(xx,yy)
    dimnames(rval)<-list(levels(x),levels(y))
    rval
  }
}


plot.mr<-function(x,...){
  UpSetR::upset(as.data.frame(x),...)
}


image.mr<-function(x,type=c("overlap","conditional","association","raw"),...){
  type<-match.arg(type)
  levs<-levels(x)
  if (type=="raw"){
    image( t(as.logical(x)), axes=FALSE)
    axis(3,at=seq(0,1,length=length(levs)),labels=levs)
    invisible(x)
  } else{
    m<-mtable(x,x)
    switch(type,
           overlap=ggimage(m,"",""),
           conditional=ggimage(m/diag(m),"Given","Proportion present"),
           association=ggimage(cov2cor(m),"","")
           )
  }
}

ggimage<-function(x,xlab,ylab){
    d<-data.frame(x=rep(rownames(x),ncol(x)),
                  y=rep(colnames(x),each=nrow(x)),
                  z=as.vector(x))
    ggplot(d, aes(x=x,y=y,fill=z))+
        geom_raster()+xlab(xlab)+ylab(ylab)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
