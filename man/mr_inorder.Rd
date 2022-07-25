\name{mr_inorder}
\alias{mr_inorder}
\alias{mr_infreq}
\alias{mr_inseq}
\alias{mr_reorder}
\alias{mean0}
\alias{ms_inorder}
\alias{ms_inseq}
\alias{ms_infreq}
\alias{ms_inscore}
\alias{ms_reorder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reorder levels of multiple-response objects
}
\description{
 \code{mr_inorder} and \code{ms_inorder} use the order in which the
levels first appear in the data (which is invariant to locale),\code{mr_inseq} and
 \code{ms_inseq} sort alphabetically (for the current locale).  \code{mr_infreq} sorts by frequency, and \code{ms_inscore} applies a function to the values in each level -- one such function is \code{mean0}, which takes the mean of non-zero values.  Finally, \code{ms_reorder} and \code{mr_reorder} use some function of a second variable computed on the observations where each level is present. 
 }
\usage{
mr_inorder(x,...)
ms_inorder(x)
mr_inseq(x,...)
ms_inseq(x)
mr_infreq(x,na.rm=TRUE,...)
ms_infreq(x)
ms_inscore(x, fun=mean0)
mean0(y)
mr_reorder(x, v, fun=median,...)
ms_reorder(x, v, fun=median)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{mr}
  }
\item{na.rm}{Remove \code{NA} values before computing frequencies (\code{NA}
  sorts last)}
\item{v,fun}{Sort levels of \code{x} according to the values of
    \code{fun(v)} for observations having each level}
\item{y}{numeric vector}
\item{\dots}{not used}
}
\value{
Object of class \code{mr}
}
\references{
These are based on the reordering functions for factors in the
\code{forcats} package.
}

\examples{
data(ethnicity)
mr_infreq(ethnicity)
mr_inseq(ethnicity)

data(nzbirds)
mtable(nzbirds)
mtable(ms_inorder(nzbirds))
mtable(ms_inseq(nzbirds))
mtable(ms_inscore(nzbirds, mean0))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }% use one of  RShowDoc("KEYWORDS")

