
library(tidyverse)
library(vctrs)
library(varhandle)

# as.td method
as.td <- function(x,...) UseMethod("as.td",x)

# as.td.td
as.td.td <- function(x,...) x

as.td.default <- function(x, ..., name = NULL, na.rm = TRUE){
  x <- unclass(as.mr(x, name = name, na.rm = na.rm))
  for (i in 1:length(x)) {
  vec_assert(x[i],logical())
  }
  if (!na.rm){
    na.vals<-which(is.na(x), arr.ind = TRUE)
    x <- replace_na(x, TRUE)
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
    if (i %in% na.vals[1,]){
      na.names <- colnames(x)
      na.names[unname(na.vals[which(na.vals[1]==i),][2])] <- paste0("?",colnames(x)[unname(na.vals[which(na.vals[1]==i),][2])])
      v <- vec_c(v,paste(rep(na.names,matrix(as.numeric(x), ncol = dim(x)[2], nrow(x)[1])[i,]), collapse = "+"))
    }
    else{
    v <- vec_c(v,paste(rep(colnames(x),matrix(as.numeric(x), ncol = dim(x)[2], nrow(x)[1])[i,]), collapse = "+"))
    }
  }
  new_vctr(v, class = "td")
}

### examples

as.td.default(strsplit(as.character(usethnicity$Q5),""))
as.td.default(usethnicity$Q4==1, name ="Hispanic")
as.td(nzbirds>0, name = colnames(nzbirds), na.rm = FALSE)
