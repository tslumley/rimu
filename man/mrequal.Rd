\name{mrequal}
\alias{\%has\%}
\alias{\%hasonly\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if a level is present
}
\description{
Returns vector of \code{TRUE} or \code{FALSE} according to whether \code{y} is onle of the levels present for that row or is the only level present for that row.
}
\usage{
x \%has\% y
x \%hasonly\% y
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  \code{mr} object
}
  \item{y}{
  character vector specifying a level
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% __ONLY ONE__ keyword per line
