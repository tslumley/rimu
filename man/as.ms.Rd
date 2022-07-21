\name{as.ms}
\alias{as.ms}
\alias{as.ms.list}
\alias{as.ms.matrix}
\alias{as.ms.data.frame}
\alias{as.ms.mr}
\alias{as.ms.character}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct scored or ranked multiple-response objects
}
\description{
The internal representation is as a numeric matrix with 0 when a level is not present and the non-zero rank or score when it is present. The data.frame and matrix methods uses the numeric values of \code{x}, and by default set \code{NA} values to 'not present'.  The list method takes a list with a character vector for each observation and uses the position in the list as the rank/score. The character method splits the string at the separators to give a list and uses the list method.
The \code{mr} method uses a score of 1 whenever the level is present.
}
\usage{
as.ms(x, ...)
\method{as.ms}{list}(x,...,levels=NULL)
\method{as.ms}{data.frame}(x,...,na.rm=TRUE)
\method{as.ms}{matrix}(x,...,na.rm=TRUE)
\method{as.ms}{mr}(x,...)
\method{as.ms}{character}(x,sep=", ", ...,levels=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  object to be converted
}
  \item{\dots}{
   for compatibility; not used.
}
\item{levels}{Optional character vector giving the permitted levels}
\item{na.rm}{Convert \code{NA} values to 'not present', ie, 0}
\item{sep}{Regular expression for splitting the character string}
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
