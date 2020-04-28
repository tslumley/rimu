
library(tidyverse)
library(vctrs)

# as.td method
as.td<-function(x,...) UseMethod("as.td",x)


# as.td.td
as.td.td<-function(x,...) x


#as.td.list function
as.td.list <- function(x, levels=NULL, name = NULL) {
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
  if (!is.null(name)){
    df <- as.td.logical(df, name = name)
  }
  else {
    df <- as.td.logical(df, name = colnames(df))
  }
  df
}


#as.td.logical function
as.td.logical <- function(x, ..., name = NULL) {
  if (sum(is.na(x))>0){
  x <- replace_na(x, FALSE)
  }
  if (!is.matrix(x)){
  x <- as.matrix(x)
  colnames(x) <- name
  }
  else{
    if (!is.null(name))
      colnames(x) <- name
  }
  v <- c()
  for (i in 1:dim(x)[1]){
    v <- vec_c(v,paste(rep(colnames(x),matrix(as.numeric(x), ncol = dim(x)[2], nrow(x)[1])[i,]), collapse = "+"))
  }
  class(v) <- "td"
  v
}


#as.td.data.frame function
as.td.data.frame<-function(x,...,na.rm=TRUE){
  m <- as.matrix(x > 0)
  if (na.rm){
    m[is.na(m)]<-FALSE
  }
  v <- as.td.logical(m)
  v
}




