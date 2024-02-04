new_vmr <- function(x,levels=unique(do.call(c,x))) {
  vctrs::new_list_of(x, ptype = character(), class = "vmr", levs=levels)
}

as.vmr<-function(x,...) UseMethod("as.vmr")
as.vmr.mr<-function(x,na.rm=FALSE,...) {
    if(na.rm)
        x<-mr_na(x, FALSE)        
    l<-levels(x)
    y<-lapply(seq_len(length(x)), function(i) l[as.logical(x[i,])])
    rval <- new_vmr(y,l)
    if (!na.rm && any(i<-rowSums(is.na(x))>0)){
        for(j in which(i)){
            rval[[j]]<- NA_character_
            }
    }
    rval
}
as.vmr.default<-function(x,na.rm=FALSE,...) as.vmr(as.mr(x,...),na.rm=na.rm)

vec_ptype_full.vmr <- function(x, ...) "vmultiresp"
vec_ptype_abbr.vmr <- function(x, ...) "vmr"

levels.vmr<-function(x,...) attr(x, "levs")

format.vmr <- function(x, ...) {
    format(as.mr(unclass(x),...,levels=levels(x)))
}

obj_print_data.vmr <- function(x, ...) {
  if (length(x) == 0)
    return()
  print(format(x), quote = FALSE)
}


pillar_shaft.vmr <- function(x, ...) {
  full <- format(x)
  short <-paste0("[",sapply(x,length),"]")

  pillar::new_pillar_shaft(
    list(full = full, short=short),
    width = pillar::get_max_extent(full),
    min_width = pillar::get_max_extent(short),
    class = "pillar_shaft_vmr"
  )
}


format.pillar_shaft_vmr <- function(x, width, ...) {
  if (pillar::get_max_extent(x$full) <= width) {
    ornament <- x$full
  } else {
    ornament <- x$short
  }

  pillar::new_ornament(ornament, align = "right")
}

mr_count.vmr<-function(x,na.rm=TRUE,...) sapply(x,length)

mr_union.vmr<-function(x,y,...) {
    r<-NextMethod()
    as.vmr(r)
}

mr_diff.vmr<-function(x,y,...) {
    r<-NextMethod()
    as.vmr(r)
}

mr_intersect.vmr<-function(x,y,...) {
    r<-NextMethod()
    as.vmr(r)
}


mr_reorder.vmr<-function(x,v,fun=median,...) {
    x<-as.mr(x)
    r<-NextMethod()
    as.vmr(r)
}


mr_inseq.vmr<-function(x,...) {
    x<-as.mr(x)
    r<-NextMethod()
    as.vmr(r)
}


mr_inorder.vmr<-function(x,...) {
    x<-as.mr(x)
    r<-NextMethod()
    as.vmr(r)
}


mr_infreq.vmr<-function(x,na.rm=TRUE,...) {
    x<-as.mr(x)
    r<-NextMethod()
    as.vmr(r)
}

mr_recode.vmr<-function(x,...){
    x<-as.mr(x)
    r<-NextMethod()
    as.vmr(r)
    }

mr_drop.vmr<-function(x, levels,...) {
    x<-as.mr(x)
    r<-NextMethod()
    as.vmr(r)
}

mr_lump.vmr<-function(x, n, prop,  other_level = "Other",
                     ties.method = c("min", "average", "first", "last", "random", "max"),...) {
    x<-as.mr(x)
    r<-NextMethod()
    as.vmr(r)
}

stack1.vmr<-function(x,label,na.rm){
    x<-as.mr(x)
    stack1(x, label, na.rm)
}


plot.vmr<-function(x, ...) plot(as.mr(x),...)
image.vmr<-function(x,type = c("overlap", "conditional", "association", 
                               "raw"), ...) image(as.mr(x), type=type,...)

barplot.vmr<-function(height, ...) barplot(mtable(height),...)

