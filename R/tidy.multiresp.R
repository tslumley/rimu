
# as.td function

as.td<-function(x,...) UseMethod("as.td",x)
as.td.td<-function(x,...) x

as.td.logical<-function(x,name,...){
  if (!is.data.frame(x)){
    x<-as.data.frame(x)
    colnames(x)<-name
  } else {
    if (!missing(name))
      colnames(x)<-name
  }
  class(x)<-"td"
  x
}

as.td.list <- function(x,...,levels=NULL) {
  levs <- vec_c(unique(do.call(c,x)))
  if (!is.null(levels)) {
    if (any(xtra <- setdiff(levs,levels)))
      warning(paste("values not in 'levels' ",paste(xtra,collapse=", ")))
    levs <- vec_c(levels)
  }
  df<-as.data.frame(matrix(FALSE,nrow=length(x),ncol=length(levs)))
  for(i in seq_along(x)){
    df[i,]<-levs %in% x[[i]]
  }
  for(i in length(df)) {
    df[,i] <- vec_c(df[,i])
  }
  colnames(df)<-levs
  class(df)<-"td"
  df
}

as.td.data.frame<-function(x,...,na.rm=TRUE){
  x <- as.data.frame(x > 0)
  if (na.rm){
    x[is.na(x)]<-FALSE
  }
  for(i in length(x)) {
    x[,i] <- vec_c(x[,i])
  }
  class(x)<-"td"
  x
}

as.td.logical<-function(x,name,...){
  if (!is.data.frame(x)){
    x<-as.data.frame(unclass(x))
    colnames(x)<-name
  } else {
    if (!missing(name))
      colnames(x)<-name
  }
  for(i in length(x)) {
    x[,i] <- vec_c(x[,i])
  }
  class(x)<-"td"
  x
}


