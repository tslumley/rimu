
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
  m<-as.data.frame(matrix(FALSE,nrow=length(x),ncol=length(levs)))
  for(i in seq_along(x)){
    m[i,]<-levs %in% x[[i]]
  }
  for(i in length(m)) {
    m[,i] <- vec_c(m[,i])
  }
  colnames(m)<-levs
  #class(m)<-"td"
  m
}

as.td.data.frame<-function(x,...,na.rm=TRUE){
  x <- as.data.frame(x > 0)
  if (na.rm){
    x[is.na(x)]<-FALSE
  }
  class(x)<-"td"
  x
}



