\name{ms_flatten}
\alias{ms_flatten}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flatten a scored multiple-response object into a factor
}
\description{
Convert a multiple-response object into a named numeric vector using a supplied ordering. 

}
\usage{
ms_flatten(x, priorities, fun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{mr} object
}
  \item{priorities}{
Character vector of levels.
}
\item{fun}{Function for reducing two values to one. }
\item{start}{starting value for \code{fun}}
}
\details{
Each observation is initially assigned the value \code{start}.  Starting with the lowest-priority level, the current value is combined with the new value as \code{fun(new, current)}. Using \code{fun=function(x,y) x} would return the value for the highest-priority level present; using \code{fun=pmax} would return the highest score for any level present; using \code{fun="+"} would return the sum of the scores.  
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

data(nzbirds)
## hardest to see first
ms_flatten(nzbirds, c("kaki","ruru","kea","tui","tauhou"),"+")
ms_flatten(nzbirds, c("kaki","ruru","kea","tui","tauhou"),
  fun=function(x,y) x)
ms_flatten(nzbirds, c("kaki","ruru","kea","tui","tauhou"),pmin,start=Inf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
