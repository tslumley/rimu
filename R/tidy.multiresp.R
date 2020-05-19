
library(tidyverse)
library(vctrs)
library(varhandle)

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
  df <- as.matrix(df)
  if (!is.null(name)){
    df <- as.td.logical(df, name = name)
  }
  else {
    df <- as.td.logical(df, name = colnames(df))
  }
  df
}


#as.td.default ???

#?????????
as.td.factor<-function(){
  as.mr.factor(x)
}


as.td <-function(x, ..., name = NULL){
  x <- unclass(as.mr(x, name = name))
  for (i in 1:length(x)) {
  vec_assert(x[i],logical())
  }
  if (sum(is.na(x))>0){
    x <- replace_na(x, FALSE)
  }
  if (!is.matrix(x)){
    x <- as.matrix(x)
    colnames(x) <- name
  }
  else{
    if (!is.null(name)){
      colnames(x) <- name
    }
  }
  v <- c()
  for (i in 1:dim(x)[1]){
    v <- vec_c(v,paste(rep(colnames(x),matrix(as.numeric(x), ncol = dim(x)[2], nrow(x)[1])[i,]), collapse = "+"))
  }
  new_vctr(v, class = "td")
}

