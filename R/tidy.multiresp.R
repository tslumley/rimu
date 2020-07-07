
library(tidyverse)
library(vctrs)
library(varhandle)

# as.td method
as.td <- function(x,...) UseMethod("as.td",x)

# as.td.td
as.td.td <- function(x,...) x

as.td.default <- function(x, ...,levels = NULL, na.rm = TRUE){
  x <- as.character(as.mr(x, levels = levels, na.rm = na.rm), na.rm = na.rm)
  new_vctr(x, class = "td")
}

as.logical.td <- function(x, names = NULL, ..., na.rm = TRUE) {
  x <- strsplit(unclass(as.td(x, na.rm = na.rm)), "+", fixed = TRUE)
  x_all <- unlist(x)
  x_unique <- unique(x_all)
  if (na.rm == TRUE & "?" %in% substring(x_unique, 1, 1)) {
    ind <- which(substring(x_unique, 1, 1)=="?")
    x_unique <- x_unique[-c(ind)]
  }
  if (!is.null(names) & !identical(x_unique,names)) {
    x_unique <- names
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

as.character.td <- function(x,sep="+",na.rm=TRUE,...){
  as.character.mr(as.mr(as.logical.td(x)),sep=sep,na.rm=na.rm,...)
}

as.data.frame.td <- function(x,...){
  as.data.frame.mr(as.mr(as.logical.td(x)))
}

print.td <- function(x, ..., na.rm=FALSE,sep="+"){
  x<-as.character(x, ..., na.rm=FALSE)
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
  x[,!(colnames(x) %in% levels)]
}

levels.td <-function(x,...) {
  colnames(as.logical.td(x))
}

"levels<-.td"<-function(x, value) {
  x <- as.logical.td(x)
  colnames(x)<-value
  as.td(x)
}

