\name{mr_flatten}
\alias{mr_flatten}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flatten a multiple-response object into a factor
}
\description{
Convert a multiple-response object into a factor using a supplied ordering.  Each observation is assigned its first level in the ordering. That is, an observation that has \code{priorities[1]} as one of its levels is assigned that value. A nobservation that does not \code{priorities[1]} as one of its levels, but does have \code{priorities[2]} is assigned \code{priorities[2]}.

}
\usage{
mr_flatten(x, priorities)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{mr} object
}
  \item{priorities}{
Character vector of levels.
}
}
\value{
A factor
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(ethnicity)
ethnicity

## NZ 'prioritised ethnicity'
eth <- mr_recode(ethnicity, `European/Other`="European", `European/Other` = "MELAA")
mr_flatten(ethnicity, c("Māori","Pacific","Asian","European/Other"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }% use one of  RShowDoc("KEYWORDS")

