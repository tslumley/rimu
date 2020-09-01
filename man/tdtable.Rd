\name{tdtable}
\alias{tdtable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tables involving multiple-response objects
}
\description{
Creates one-way and two-way tables using every level of a multiple response object. Use \code{table(as.character(x))} to tabulate combinations of levels
}
\usage{
tdtable(x, y, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{td} object or a factor
}
  \item{y}{
\code{td} object or a factor
}
  \item{na.rm}{
remove missing values?
}
}

\value{
A 1-d or 2-d array with names giving the levels
}

\examples{
data(ethnicity)
tdtable(ethnicity)
table(as.character(ethnicity))

data(nzbirds)
nzbirds<-as.mr(nzbirds)

## co-occurence table
tdtable(nzbirds, nzbirds)

## table by a factor
v<-rep(c("A","B"),3)
tdtable(nzbirds,v)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
