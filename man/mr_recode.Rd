\name{mr_recode}
\alias{mr_recode}
\alias{ms_recode}
\alias{td_recode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Relabel levels of multiple-response objects
}
\description{
Relabel some or all of the levels of a multiple-response object.  Two
levels that are recoded to the same value will be combined.
}
\usage{
mr_recode(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class \code{mr} or \code{ms}
  }
  \item{\dots}{
    new names in the form \code{new_name="old name"}. 
  }
}
\value{
New object of class  \code{mr}, \code{ms}, or \code{td}
}

\examples{
data(nzbirds)
nzbirds<-as.mr(nzbirds)
nzbirds
## recode to English names
mr_recode(nzbirds,morepork="ruru",stilt="kaki",waxeye="tauhou")

data(usethnicity)
race<-as.td(strsplit(as.character(usethnicity$Q5),""))
race<-td_drop(race,c(" ","F","G","H"))
race <- td_recode(race, AmIndian="A",Asian="B", Black="C", Pacific="D", White="E")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }% use one of  RShowDoc("KEYWORDS")
