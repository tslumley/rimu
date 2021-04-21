
as.td <- function(x,...) UseMethod("as.td",x)

as.td.td <- function(x,...) x

as.td.default <- function(x, ...,levels = colnames(x), na.rm = TRUE){
  x <- as.character(as.mr(x, levels = levels, na.rm = na.rm, name = levels), na.rm = na.rm)
  vctrs::new_vctr(x, class = "td")
}

as.mr.td <-function(x,name = levels(x)){
  y<-as.logical.td(x, name = name)
  as.mr.logical(y)
}

as.logical.td <- function(x, levels = NULL, ..., na.rm = FALSE) {
  x <- strsplit(unclass(as.td(x, na.rm = na.rm)), "+", fixed = TRUE)
  x_all <- unlist(x)
  x_unique <- unique(x_all)
  if (na.rm == TRUE & "?" %in% substring(x_unique, 1, 1)) {
    ind <- which(substring(x_unique, 1, 1)=="?")
    x_unique <- x_unique[-c(ind)]
  }
  if (!is.null(levels) & !identical(x_unique,levels)) {
    x_unique <- levels
  }
  x_tf_matrix <- matrix(FALSE, ncol = length(x_unique), nrow = length(x))
  colnames(x_tf_matrix) <- x_unique
  for (i in 1:length(x)) {
    x_tf_matrix[i,match(unlist(x[i]), x_unique)] <- TRUE
  }
  x_tf_matrix
}

tdtable <- function(x, y, na.rm = TRUE) {
  x <- as.logical.td(x)
  if (!missing(y)){
    y <- as.logical.td(y)
  }
  mtable(x,y, na.rm = na.rm)
}


fix_levels.td<-function(x, name = NULL){
  x <- as.logical.td(x, name = name)
  first<-!duplicated(colnames(x))
  if (all(first))
    return(x)
  y<-x[,first]
  x<-as.logical.td(x)
  
  for(lev in levels(y)){
    is_lev<-colnames(x)==lev
    if (sum(is_lev)>1)
      y[,lev]<-apply(x[,is_lev],1,any)
  }
  y
}


td_recode <- function(x, ...){
  new <- list(...)
  newnames <- names(new)
  deadlevs <- unlist(new)
  x <- as.logical.td(x)
  levs <- colnames(x)
  if(!all(deadlevs %in% levs)){
    stop(paste("non-existent levels",deadlevs[!(deadlevs %in% levs)]))
  }
  levs[match(deadlevs,levs)]<-newnames
  colnames(x)<-levs
  fix_levels(x)
  as.td(x)
}

as.mr.td <- function(x, na.rm=FALSE, ...){
  as.mr(as.logical.td(x, na.rm = na.rm))
}

as.character.td <- function(x,sep="+",na.rm=FALSE,...){
  as.character.mr(as.mr(x,na.rm=na.rm,...))
}

as.data.frame.td <- function(x,...){
  as.data.frame.mr(as.mr(as.logical.td(x)))
}

print.td <- function(x, ..., na.rm=FALSE,sep="+"){
  x<-as.character.td(x, ..., na.rm=FALSE)
  print(x)
}

td_count <- function(x,na.rm=TRUE) {
  x <- as.logical.td(x)
  rowSums(x,na.rm=na.rm)
}

td_drop <- function(x, levels, name = NULL){
  x <- as.logical.td(x, name = name)
  if(!all(levels %in% colnames(x))){
    stop(paste("non-existent levels:", levels[!(levels %in% levels(x))]))
  }
  as.td(x[,!(colnames(x) %in% levels)])
}

levels.td <-function(x, na.rm=FALSE,...) {
  colnames(as.logical.td(x,na.rm=na.rm))
}

"levels<-.td"<-function(x, value) {
  x <- as.logical.td(x)
  colnames(x)<-value
  as.td(x)
}

length.td<-function(x) length(as.character(x))


"%has%"<-function(x,y) {
  x<-as.mr(x)
  if (is.factor(y)) y<-as.character(y)
  if (!is.character(y)) stop('needs to be character or factor')
  if(length(y)==1) y<-rep(y,length(x))
  ifelse(y %in% levels(x), unclass(x)[,match(y,levels(x))] ,FALSE)
}

"%hasonly%"<-function(x,y) {
  (x %has% y) & (td_count(x)==1)
}

td_union<-function(x,y, na.rm = FALSE){
  x<-as.mr(x, na.rm = na.rm)
  y<-as.mr(y, na.rm = na.rm)
  if (length(x)!=length(y)) 
    stop("different numbers of observations in x and y")
  levs<-unique(c(levels(x),levels(y)))
  m<-matrix(FALSE,nrow=nrow(x),ncol=length(levs))
  colnames(m)<-levs
  m[,levels(x)]<-x
  m[,levels(y)]<-m[,levels(y)] | y
  as.td(m, na.rm = na.rm)
}

td_diff<-function(x,y, na.rm = FALSE, ...){
  x<-as.mr(x, na.rm = na.rm, ...)
  y<-as.mr(y, na.rm = na.rm, ...)
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
  m
  as.td(m, colnames(m), na.rm = na.rm)
}


td_intersect<-function(x,y){
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
  as.td(m, colnames(m), na.rm = na.rm)
}

td_reorder<-function(x,v,fun=median){
  x<-as.logical(x)
  values<-apply(x, 2, function(xi) fun(v[xi]))
  x<-x[,order(values)]
  as.td(x, colnames(x))
}

td_inseq<-function(x){
  x<-as.logical(x)
  x<-x[,order(colnames(x))]
  as.td(x,colnames(x))
}

td_inorder<-function(x){
  x<-as.logical(x)
  pos<-apply(as.logical(x),2, function(xi) min(which(xi)))
  x<-x[,order(pos)]
  as.td(x,colnames(x))
}

td_na<-function(x,na=TRUE, levels = NULL){
  y<-as.logical(x, na.rm =!na)
  if (!is.null(levels)){
    y<-as.td(y,levels=levels)
  }
  as.td(y,levels = colnames(y),na.rm=!na)
}

td_infreq<-function(x, na.rm = TRUE){
  y<-as.logical(td_na(x,na=!na.rm), na.rm = na.rm)
  freqs<-colSums(y)
  y<-y[,order(-freqs)]
  as.td(y,levels = colnames(y))
}

td_flatten<-function(x,priorities){
  y<-rep(NA_character_,length(x))
  if (is.null(priorities))
    priorities<-levels(x)
  for(l in rev(priorities)){
    y<-ifelse(x %has% l,l,y)
  }
  factor(y,levels=levels(x))
}

td_lump<-function(x, n, prop, other_level = "Other",
                  ties.method = c("min", "average", "first", "last", "random", "max")) {
  if(!inherits(x,"td"))
    stop("x must be a 'td' object")
  x<-as.mr(as.logical(x, name = levels(x)))
  mr_lump(x, n, prop,  other_level = other_level,
          ties.method = ties.method)
}

stack.td<-function(x,...,na.rm=FALSE){
  levels<-levels(x)
  x<-unclass(as.logical(x))
  x[is.na(x)]<-!na.rm
  r<-rowSums(x)
  values<-do.call(c,lapply(seq_len(NROW(x)),function(i) levels[x[i,]]))
  id<-rep(seq_len(NROW(x)),r)
  data.frame(values=factor(values,levels=levels),id)
}

plot.td<-function(x,...){
  x<-as.mr(x)
  UpSetR::upset(as.data.frame(x),...)
}

image.td<-function(x,type=c("overlap","conditional","association","raw"),...){
  x<-as.mr(x)
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

globalVariables(c("x","y","z"))

ggimage<-function(x,xlab,ylab){
  x<-as.mr(x)
  d<-data.frame(x=rep(rownames(x),ncol(x)),
                y=rep(colnames(x),each=nrow(x)),
                z=as.vector(x))
  ggplot(d, aes(x=x,y=y,fill=z))+
    geom_raster()+xlab(xlab)+ylab(ylab)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
