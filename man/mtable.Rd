\name{mtable}
\alias{mtable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tables involving multiple-response objects
}
\description{
Creates one-way and two-way tables using every level of a multiple response object. Use \code{table(as.character(x))} to tabulate combinations of levels
}
\usage{
mtable(x, y, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{mr}  object or a factor
}
  \item{y}{
\code{mr}  object or a factor
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
mtable(ethnicity)
table(as.character(ethnicity))

data(nzbirds)
nzbirds<-as.mr(nzbirds)

## co-occurence table
mtable(nzbirds, nzbirds)

## table by a factor
v<-rep(c("A","B"),3)
mtable(nzbirds,v)

data(nzbirds)
mtable(nzbirds>0)

}
