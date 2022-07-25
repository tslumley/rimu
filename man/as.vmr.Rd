\name{as.vmr}
\alias{as.vmr}
\alias{new_vmr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tidyversatile multiple-response objects
}
\description{
The \code{vmr} class wraps the \code{mr} class using the \code{vctrs} package, for compatibility with tidyverse \code{tbl_df} objects (tibbles). 
}
\usage{
as.vmr(x, ...)
new_vmr(x, levels = unique(do.call(c, x))) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{
For \code{as.vmr}, an \code{mr} object or anything coercible to one. For \code{new_vmr}, a list of character vectors
}
  \item{\dots}{
  not used
}
\item{levels}{the permitted levels for the object}
}
\details{
These objects need the \code{vctrs} and \code{pillar} packages to work, and need the \code{tibble} package to be useful. 
}
\value{
An object of class \code{vmr}
}
\seealso{
\code{\link{as.mr}}

The \code{internals} vignette for internal structure
}
\examples{
if (requireNamespace("vctrs", quietly=TRUE)){
   data(nzbirds)
   nzbirds
   tidybirds<-as.vmr(nzbirds, na.rm=TRUE)
   tidybirds
}

}