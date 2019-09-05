\name{nzbirds}
\alias{nzbirds}
\docType{data}
\title{
Toy example using New Zealand birds
}
\description{
A small artifical dataset that could be produced by asking people to name New Zealand birds. Each observation has scores from 1 (first bird named) to at most 4 (fourth bird named). 
}
\usage{data("nzbirds")}
\format{
  A \code{ms} object with 6 observations on the following 5 variables.
  \describe{
    \item{\code{kea}}{a numeric vector}
    \item{\code{ruru}}{a numeric vector}
    \item{\code{tui}}{a numeric vector}
    \item{\code{tauhou}}{a numeric vector}
    \item{\code{kaki}}{a numeric vector}
  }
}

\examples{
data(nzbirds)
nzbirds
as.mr(nzbirds)
}
\keyword{datasets}
