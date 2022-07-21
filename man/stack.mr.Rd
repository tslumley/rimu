\name{mr_stack}
\alias{mr_stack}
\alias{ms_stack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pivot a multiple-response object to long form
}
\description{
Creates a data frame where every observation has as many rows as it has levels present, plus an id column to specify which rows go together.
}
\usage{
mr_stack(x, ..., na.rm = FALSE)
ms_stack(x, ..., na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
multiple response object
}
  \item{\dots}{
other multiple response objects
}
  \item{na.rm}{
drop \code{NA} values?
}
}

\value{
A data frame with columns \code{values} and \code{id}, plus a column \code{scores} if \code{x} is a  \code{ms} object. When more than one object is supplied, the result is an outer join of the two indindividual results, so it contains a row for every combination of an observed value from each object.
}

\examples{
data(ethnicity)
ethnicity
mr_stack(ethnicity)

data(nzbirds)
nzbirds
ms_stack(nzbirds)

## not actually a sensible use
d <- mr_stack(ethnicity, nzbirds)
head(d)
with(d, table(ethnicity, nzbirds))
## equivalent, but more efficient
mtable(mr_na(ethnicity), mr_na(nzbirds))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }% use one of  RShowDoc("KEYWORDS")
