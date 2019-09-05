\name{as.mr}
\alias{as.mr}
\alias{as.mr.logical}
\alias{as.mr.data.frame}
\alias{as.mr.list}
\alias{as.mr.ms}
\alias{as.mr.factor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct multiple-response objects
}
\description{
Constructs \code{mr} objects representing multiple-choice questions where more than one choice is allowed.
}
\usage{
as.mr(x, ...)
\method{as.mr}{logical}(x,name,...)
\method{as.mr}{list}{x,...,levels=NULL}
\method{as.mr}{factor}(x,...)
\method{as.mr}{data.frame}(x,...,na.rm=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object to be converted to class \code{mr}
  }
  \item{\dots}{
    for compatibility; not used
  }
  \item{levels}{optional character vector of the permitted levels}
  \item{name}{level name (for a vector) or vector of level names to
    replace the column names (for a matrix)}
  \item{na.rm}{If \code{TRUE}, replace \code{NA} in the input by \code{FALSE}}
}
\details{
The internal representation of \code{mr} objects is as a logical matrix
with the levels as column names.

The method for logical \code{x} coerces a single vector to a one-column
matrix, and then applies the \code{name} argument as the column
name. Given a matrix, the \code{name} argument is optional and replaces
the existing column names

The method for list \code{x} takes a list of character vectors that
represent the levels present for one observation.

The method for factor \code{x} produces an \code{mr} object with the
factor levels as levels. Each observation will have only one value.

The \code{data.frame} object works for logical or numeric columns of a
data frame. Zero or negative values are treated as 'not present',
positive values as 'present'. Optionally, \code{NA} values are coded as
'not present', which is useful when the data frame was created by
\code{reshape} or \code{dplyr::spread}.

The method for \code{ms} objects simply drops the score/rank information

}
\value{
Object of class \code{mr}
}

\examples{

nzbirds_list<-list(c("kea","tui"), c("kea","ruru","kaki"), c("ruru"),
c("tui","ruru"), c("tui","kea","ruru"), c("tauhou","kea"))
nzbirds_list
as.mr(nzbirds_list)

data(nzbirds)
nzbirds
as.mr(nzbirds)

data(ethnicity)
ethnicity
as.logical(ethnicity)
as.mr(as.logical(ethnicity))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }% use one of  RShowDoc("KEYWORDS")
\keyword{classes }% __ONLY ONE__ keyword per line
