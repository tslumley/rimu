\name{birds}
\alias{birds}
\docType{data}
\title{
Subset of the Great Backyard Bird survey
}
\description{
Counts of observations for 12 bird species by US county and Canadian province in the Great Backyard Bird survey.  These birds were randomly sampled from the much larger number in the full data set. See the vignette for more details.
}
\usage{data("birds")}
\format{
  A data frame with 3046 observations on the following 13 variables.
  \describe{
    \item{\samp{Phalaenoptilus nuttallii}}{a numeric vector}
    \item{\samp{Fregata magnificens}}{a numeric vector}
    \item{\samp{Melanerpes lewis}}{a numeric vector}
    \item{\samp{Melospiza georgiana}}{a numeric vector}
    \item{\samp{Rallus limicola}}{a numeric vector}
    \item{\samp{Myioborus pictus}}{a numeric vector}
    \item{\samp{Poecile gambeli}}{a numeric vector}
    \item{\samp{Aythya collaris}}{a numeric vector}
    \item{\samp{Xanthocephalus xanthocephalus}}{a numeric vector}
    \item{\samp{Gracula religiosa}}{a numeric vector}
    \item{\samp{Icterus parisorum}}{a numeric vector}
    \item{\samp{Coccyzus erythropthalmus}}{a numeric vector}
    \item{\code{location}}{a character vector}
  }
}

\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
data(birds)
birds<-as.ms(birds[,1:12],na.rm=TRUE)
mtable(as.mr(birds))
}
\keyword{datasets}
