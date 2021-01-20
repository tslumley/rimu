\name{mr_union}
\alias{mr_union}
\alias{td_union}
\alias{mr_intersect}
\alias{td_intersect}
\alias{mr_diff}
\alias{td_diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set operations on multiple-response objects
}
\description{
These functions take union, intersection, and difference of two multiple-response objects.  An observation has a level in the union if it has that level in either input. It has the level in the intersection if it has the level in both inputs. It has the level in the difference if it has the level in \code{x} and not in \code{y}
}
\usage{
mr_union(x, y)
td_union(x, y)
mr_intersect(x, y)
td_intersect(x, y)
mr_diff(x, y)
td_diff(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{Objects of class \code{mr}}
}



\value{
Object of class \code{mr} or \code{td}
}

\examples{

data(usethnicity)
race<-as.mr(strsplit(as.character(usethnicity$Q5),""))
race<-mr_drop(race,c(" ","F","G","H"))
race <- mr_recode(race, AmIndian="A",Asian="B", Black="C", Pacific="D", White="E")
mtable(race)

hispanic<-as.mr(usethnicity$Q4==1, "Hispanic")
ethnicity<-mr_union(race, hispanic)
mtable(ethnicity)
ethnicity[101:120]

data(usethnicity)
race<-as.td(strsplit(as.character(usethnicity$Q5),""))
race<-td_drop(race,c(" ","F","G","H"))
race <- td_recode(race, AmIndian="A",Asian="B", Black="C", Pacific="D", White="E")
mtable(race)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")

