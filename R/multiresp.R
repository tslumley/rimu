


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

"[.mr"<-function(x,i,j){
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

prioritise<-function(x, priorities){
  y<-rep(NA_character_,length(x))
  for(l in rev(priorities)){
    y<-ifelse(x %has% l,l,y)
  }
  factor(y,levels=levels(x))
}

pr_eth<-prioritise(ethnicity, c("Māori","Pacific","Asian","MELAA","European"))
str(pr_eth)
```


Tidying to individual indicators (like `gather`)

```{r}
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


image.mr<-function(x,type=c("overlap","conditional","raw"),...){
  type<-match.arg(type)
  levs<-levels(x)
  if (type=="raw"){
    image( t(as.logical(x)), axes=FALSE)
    axis(3,at=seq(0,1,length=length(levs)),labels=levs)
    invisible(x)
  } else{
    m<-mtable(x,x)
    switch(type,
           overlap=image(m,...,axes=FALSE),
           conditional=image(t(m/diag(m)),...,axes=FALSE)
           )
    axis(1,at=seq(0,1,length=length(levs)),labels=levs)
    axis(2,at=seq(0,1,length=length(levs)),labels=levs,las=1)
    if(type=="conditional"){
      title(xlab="Proportion",ylab="Out of")
    }
    invisible(m)
  }
}
