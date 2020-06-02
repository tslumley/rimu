
library(tidyverse)
library(vctrs)
library(varhandle)

# as.td method
as.td <- function(x,...) UseMethod("as.td",x)

# as.td.td
as.td.td <- function(x,...) x

as.td.default <- function(x, ..., levels = NULL, na.rm = TRUE){
  x <- unclass(as.mr(x, levels = levels, na.rm = na.rm))
  for (i in 1:length(x)) {
  vec_assert(x[i],logical())
  }
  na.vals<-c()
  if (!na.rm){
    na.vals<-which(is.na(x), arr.ind = TRUE)
    x <- replace_na(x, TRUE)
  } 
  if (sum(is.na(x))>0){
    x <- replace_na(x, FALSE)
  }
  if (!is.matrix(x)){
    x <- as.matrix(x)
    if (ncol(x)==length(levels)){
    colnames(x) <- levels
    }
  }
  else{
    if (!is.null(levels) & ncol(x)==length(levels)){
      colnames(x) <- levels
    }
  }
  v <- c()
  for (i in 1:dim(x)[1]){
    if (i %in% na.vals[,1]){
      na.names <- colnames(x)
      na.names[unname(matrix(na.vals[which(na.vals[,1]==i),], ncol = 2)[,2])] <- paste0("?",na.names[unname(matrix(na.vals[which(na.vals[,1]==i),], ncol = 2)[,2])])
      v <- vec_c(v,paste(rep(na.names,matrix(as.numeric(x), ncol = dim(x)[2], nrow(x)[1])[i,]), collapse = "+"))
    }
    else{
    v <- vec_c(v,paste(rep(colnames(x),matrix(as.numeric(x), ncol = dim(x)[2], nrow(x)[1])[i,]), collapse = "+"))
    }
  }
  new_vctr(v, class = "td")
}


tdtable <- function(x, y, na.rm = TRUE) {
  x <- as.logical.td(x)
  y <- as.logical.td(y)
  mtable(x,y, na.rm = na.rm)
}

as.logical.td <- function(x,..., na.rm = TRUE) {
  x <- strsplit(unclass(as.td(x, na.rm = na.rm)), "+", fixed = TRUE)
  x_all <- unlist(x)
  x_unique <- unique(x_all)
  x_tf_matrix <- matrix(FALSE, ncol = length(x_unique), nrow = length(x))
  colnames(x_tf_matrix) <- x_unique
  for (i in 1:length(x)) {
    x_tf_matrix[i,match(unlist(x[i]), x_unique)] <- TRUE
  }
  x_tf_matrix
}

