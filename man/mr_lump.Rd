\name{mr_lump}
\alias{mr_lump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Collapse common or rare levels
}
\description{
Combine the least common or most common levels of a \code{mr} object into an "other" level.
}
\usage{
mr_lump(x, n, prop, other_level = "Other",
ties.method = c("min", "average", "first", "last", "random", "max"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{mr} or \code{td}
}
  \item{n}{
Positive integer to keep the most common \code{n} values, negative integer to keep the least common values.
}
  \item{prop}{
Positive prop preserves values that appear at least prop of the time. Negative prop preserves values that appear at most -prop of the time.
}
  \item{other_level}{
Label for the lumped levels
}
  \item{ties.method}{
How to handle ties. Passed to \code{\link{rank}}
}
}

\value{
An object of class \code{mr} 
}
\references{
Based on \code{fct_lump} from the \code{forcats} package.
}

\examples{

data(ethnicity)
mtable(ethnicity)
mtable(mr_lump(ethnicity,2))
mtable(mr_lump(ethnicity,-2))

data(rstudiosurvey)
## Other software being used
other_software<- as.mr(rstudiosurvey[[40]])
mtable(other_software)
## only those with at least 20 responses
common<-mr_lump(other_software, n=20)
mtable(common)
## 'None' isn't really another package
mtable(mr_drop(common,"None"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
