

fix_levels<-function(x){
    first<-!duplicated(levels(x))
    if (all(first))
        return(x)
    y<-x[,first]
    x<-as.logical(x)
   
    for(lev in levels(y)){
        is_lev<-colnames(x)==lev
        if (sum(is_lev)>1)
            y[,lev]<-apply(x[,is_lev],1,any)
    }
    y
}

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
    colnames(m)<-levs
    class(m)<-"mr"
    m
}

as.mr.character<-function(x, sep=", ",...,levels=NULL){
    l<-strsplit(x,sep)
    as.mr(l,levels=levels)
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
    
as.character.mr<-function(x,sep="+",na.rm=TRUE,...){
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


"[.mr"<-function(x,i,j,...){
  levels<-levels(x)
  x<-as.logical(x)[i,j,drop=FALSE]
  if (!missing(j)){
      if (is.character(j))
          new_levels<-j
      else
          new_levels<-levels[j]
  } else
      new_levels<-levels
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


mr_count<-function(x, na.rm=TRUE) UseMethod("mr_count")
mr_count.default<-function(x,na.rm=TRUE) rowSums(x,na.rm=na.rm)

"%hasonly%"<- function(x,y) {
  (x %has% y) & (mr_count(x)==1)
}

"%hasall%"<-function(x, ys) {
    if (length(ys)==1) return(x %has% ys)
    
    rowSums(sapply(ys, function(y) !(x %has% y)))==0
}

"%hasany%"<-function(x, ys) {
    if (length(ys)==1) return(x %has% ys)
    
    rowSums(sapply(ys, function(y) (x %has% y)))> 0
}

mr_union<-function(x,y,...) UseMethod("mr_union")
mr_union.default<-function(x,y,...){
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

mr_diff<-function(x,y,...) UseMethod("mr_diff")
mr_diff.default<-function(x,y,...){
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

mr_intersect<-function(x,y,...) UseMethod("mr_intersect")
mr_intersect.default<-function(x,y,...){
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

mr_reorder<-function(x,v,fun=median,...) UseMethod("mr_reorder")
mr_reorder.default<-function(x, v, fun=median,...){
  values<-apply(x, 2, function(xi) fun(v[xi]))
  x<-x[,order(values)]
  x
}

mr_inseq<-function(x,...) UseMethod("mr_inseq")
mr_inseq.default<-function(x,...){
  x<-x[,order(colnames(x))]
  x
}

mr_inorder<-function(x,...) UseMethod("mr_inorder")
mr_inorder<-function(x,...){
  pos<-apply(as.logical(x),2, function(xi) min(which(xi)))
  x<-x[,order(pos)]
  x
}

mr_infreq<-function(x,na.rm=TRUE,...) UseMethod("mr_inorder")
mr_infreq<-function(x, na.rm=TRUE,...){
  y<-mr_na(x, na=!na.rm)
  freqs<-colSums(x)  
  y<-y[,order(-freqs)]
  y
}

mr_flatten<-function(x, priorities, sort=FALSE){
    x<-as.mr(x)
    y<-rep(NA_character_,length(x))
    if (is.null(priorities))
        priorities<-levels(x)
    for(l in rev(priorities)){
        y<-ifelse(x %has% l,l,y)
    }
    if (sort){
        factor(y, priorities)
    } else {
        factor(y,levels=levels(x))
    }
}

mr_recode<-function(x,...) UseMethod("mr_recode")
mr_recode.default<-function(x, ...){
    new<-list(...)
    newlevs<-names(new)
    deadlevs<-unlist(new)
    levs<-levels(x)
    if(!all(deadlevs %in% levs)){
        stop(paste("non-existent levels",deadlevs[!(deadlevs %in% levs)]))
    }
    levs[match(deadlevs,levs)]<-newlevs
    levels(x)<-levs
    fix_levels(x)
}

mr_drop<-function(x, levels,...) UseMethod("mr_drop")
mr_drop.default<-function(x, levels,...){
    if(!all(levels %in% levels(x))){
        stop(paste("non-existent levels:", levels[!(levels %in% levels(x))]))
    }
    x[,!(levels(x) %in% levels)]
}

mr_lump<-function(x, n, prop,  other_level = "Other",
                  ties.method = c("min", "average", "first", "last", "random", "max"),...) UseMethod("mr_lump")

mr_lump.default<-function(x, n, prop,  other_level = "Other",
                     ties.method = c("min", "average", "first", "last", "random", "max"),...) {
    if(!inherits(x,"mr"))
        stop("x must be an 'mr' object")
    ties.method <- match.arg(ties.method)

    levels <- levels(x)
  
    count <- as.vector(mtable(x))
    total <- length(x)
 

  if (!xor(missing(n), missing(prop))) {
    stop("need to specify one of 'n' and 'prop'")
  } else if (!missing(n)) {
    if (n < 0) {
      rank <- rank(count, ties = ties.method)
      n <- -n
    } else {
      rank <- rank(-count, ties = ties.method)
    }

    if (sum(rank > n) <= 1) {
      # No lumping needed
      return(x)
    }

    new_levels <- ifelse(rank <= n, levels, other_level)
  } else if (!missing(prop)) {
    prop_n <- count / total
    if (prop < 0) {
      new_levels <- ifelse(prop_n <= -prop, levels, other_level)
    } else {
      if (sum(prop_n <= prop) <= 1) {
        # No lumping needed
        return(x)
      }

      new_levels <- ifelse(prop_n > prop, levels, other_level)
    }
  }

    
    if (other_level %in% new_levels) {
        others<-data.frame(other=apply(x[,new_levels==other_level],1,any))
        names(others)<-other_level
        others<-as.mr(others)
        kept<-x[,new_levels!=other_level]
        rval <- mr_union(kept,others)
        rval
    } else {
        x
    }
}

stack1.mr<-function(x,label,na.rm){
  levels<-levels(x)
  x<-unclass(x)
  x[is.na(x)]<-!na.rm
  r<-rowSums(x)
  values<-do.call(c,lapply(seq_len(NROW(x)),function(i) levels[x[i,]]))
  id<-rep(seq_len(NROW(x)),r)
  rval<-data.frame(id,values=factor(values,levels=levels))
  names(rval)[2]<-label
  rval
}

stack1<-function(x,label, na.rm) UseMethod("stack1")

mr_stack<-function(x,..., na.rm=FALSE){
    label<-deparse(substitute(x))
    sx<-stack1(x,label, na.rm=na.rm)
    
    nms<-substitute(list(...))
    dots<-list(...)
    if(length(dots))  {
        sdots<-lapply(seq_along(dots), function(i) stack1(dots[[i]], deparse(nms[[i+1]]), na.rm=na.rm))
        for(sdot in sdots)            
            sx<-merge(sx, sdot, by="id")
        }
    sx
}


as.mr<-function(x,...) UseMethod("as.mr",x)
as.mr.mr<-function(x,...) x
as.mr.default<-function(x,levels=unique(x),...){
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


as.mr.logical<-function(x,name,...){
    if (!is.matrix(x)){
        x<-as.matrix(x)
        colnames(x)<-name
    } else {
        if (!missing(name))
            colnames(x)<-name
    }
    class(x)<-"mr"
    x
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

as.matrix.mr<-function(x,...){
    m<-as.logical(x,...)
    storage.mode(m)<-"integer"
    m
}

plot.mr<-function(x,...){
  UpSetR::upset(as.data.frame(as.matrix(x)),...)
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
           conditional=ggimage(t(m/diag(m)),"Proportion present","Given"),
           association=ggimage(cov2cor(m),"","")
           )
  }
}

globalVariables(c("x","y","z"))

ggimage<-function(x,xlab,ylab){
    d<-data.frame(x=rep(rownames(x),ncol(x)),
                  y=rep(colnames(x),each=nrow(x)),
                  z=as.vector(x))
    ggplot(d, aes(x=x,y=y,fill=z))+
        geom_raster()+xlab(xlab)+ylab(ylab)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }


## based on Surv(), to try to get data frame columns working
length.mr <- function(x) nrow(x)
names.mr <- function(x) rownames(x)
format.mr <- function(x, ...) format(as.character.mr(x), ...)
as.data.frame.mr <- function(x, ...) as.data.frame.model.matrix(x, ...)
