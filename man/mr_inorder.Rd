\name{mr_inorder}
\alias{mr_inorder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reorder levels of \code{mr} objects
}
\description{
 \code{mr_inorder} uses the order in which the
levels first appear in the data (which is invariant to locale),
\code{mr_infreq} sorts by frequency, and \code{mr_inseq} sorts
alphabetically (for the current locale).}
\usage{
mr_inorder(x)
mr_infreq(x,na.rm=TRUE)
mr_inseq(x)
mr_reorder(x, v, fun=median)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{mr} object
  }
\item{na.rm}{Remove \code{NA} values before computing frequencies (\code{NA}
  sorts last)}
\item{v,fun}{Sort levels of \code{x} according to the values of
    \code{fun(v)} for observations having each level}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }% use one of  RShowDoc("KEYWORDS")

