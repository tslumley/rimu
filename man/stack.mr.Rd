\name{stack.mr}
\alias{stack.mr}
\alias{stack.ms}
\alias{stack.td}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pivot a multiple-response object to long form
}
\description{
Creates a data frame where every observation has as many rows as it has levels present, plus an id column to specify which rows go together.
}
\usage{
\method{stack}{mr}(x, ..., na.rm = FALSE)
\method{stack}{ms}(x, ..., na.rm = FALSE)
\method{stack}{td}(x, ..., na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
multiple response object
}
  \item{\dots}{
for compatibility; not used.
}
  \item{na.rm}{
drop \code{NA} values?
}
}

\value{
A data frame with columns \code{values} and \code{id}, plus a column \code{scores} if \code{x} is a  \code{ms} object
}

\examples{
data(ethnicity)
ethnicity
stack(ethnicity)

data(nzbirds)
nzbirds
stack(nzbirds)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }% use one of  RShowDoc("KEYWORDS")
