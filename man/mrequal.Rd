\name{mrequal}
\alias{\%has\%}
\alias{\%hasonly\%}
\alias{\%hasall\%}
\alias{\%hasany\%}
\alias{mrequal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if a level or levels is present
}
\description{
Returns vector of \code{TRUE} or \code{FALSE} according to whether \code{y} is onle of the levels present for that row or is the only level present for that row.
}
\usage{
x \%has\% y
x \%hasonly\% y
x \%hasall\% ys
x \%hasany\% ys

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  \code{mr}  object
}
  \item{y}{
  character vector specifying a level
}
\item{y}{
  character vector specifying one or more levels
}
}

\value{
Logical vector
}

\examples{
data(ethnicity)

ethnicity
ethnicity \%has\% "Māori"
ethnicity \%hasonly\% "Māori"

data(nzbirds)
as.mr(nzbirds) %hasall% c("kea","kaki")
as.mr(nzbirds) %hasany% c("kea","kaki")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% __ONLY ONE__ keyword per line
