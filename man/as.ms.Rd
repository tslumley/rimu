\name{as.ms}
\alias{as.ms}
\alias{as.ms.list}
\alias{as.ms.matrix}
\alias{as.ms.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct scored or ranked multiple-response objects
}
\description{
The internal representation is as a numeric matrix with 0 when a level is not present and the non-zero rank or score when it is present. The data.frame and matrix methods uses the numeric values of \code{x}, and by default set \code{NA} values to 'not present'.  The list method takes a list with a character vector for each observation and uses the position in the list as the rank/score.  The \code{mr} method uses a score of 1 whenever the level is present.
}
\usage{
as.ms(x, ...)
\method{as.ms}{list}(x,...)
\method{as.ms}{data.frame}(x,...,na.rm=TRUE)
\method{as.ms}{matrix}(x,...,na.rm=TRUE)
\method{as.ms}{mr}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  object to be converted
}
  \item{\dots}{
   for compatibility; not used.
}
\item{na.rm}{Convert \code{NA} values to 'not present', ie, 0}
}
\value{
Object of class \code{ms}
}

\examples{

nzbirds_list<-list(c("kea","tui"), c("kea","ruru","kaki"), c("ruru"),
c("tui","ruru"), c("tui","kea","ruru"), c("tauhou","kea"))
nzbirds_list
(msbirds<-as.ms(nzbirds_list))

(bird_mat <- unclass(msbirds))
as.ms(bird_mat)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
