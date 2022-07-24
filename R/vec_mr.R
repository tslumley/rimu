new_vmr <- function(x,levels=unique(do.call(c,x))) {
  new_list_of(x, ptype = character(), class = "vmr", levs=levels)
}

as.vmr<-function(x,...) UseMethod("as.vmr")
as.vmr.mr<-function(x,...) {
    l<-levels(x)
    y<-lapply(apply(x,1,c, simplify=FALSE), function(i) l[as.logical(i)])
    new_vmr(y,l)
}
as.vmr.default<-function(x,...) as.vmr(as.mr(x,...))

vec_ptype_full.vmr <- function(x, ...) "vmultiresp"
vec_ptype_abbr.vmr <- function(x, ...) "vmr"

format.vmr <- function(x, ...) {
    format(as.mr(unclass(x),...,levels=attr(x,"levs")))
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
  if (get_max_extent(x$full) <= width) {
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
    r<-NextMethod(x,...)
    as.vmr(r)
    }

mr_drop.vmr<-function(x, levels,...) {
    x<-as.mr(x)
    r<-NextMethod(x,levels,...)
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
