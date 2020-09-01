\name{td_count}
\alias{td_count}
\alias{td_na}
\alias{td_drop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility functions for tidy multiple-response objects
}
\description{
These perform diverse useful tasks.  \code{td_count} counts the number of levels present for each individual. \code{td_na} sets \code{NA} values to something else, \code{td_drop} drops some levels from the object. 
}
\usage{
td_count(x, na.rm = TRUE)
td_na(x, levels, name = NULL)
td_drop(x, na = TRUE, levels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{td} object
}
  \item{na.rm}{
Remove \code{NA} first?
}
\item{levels}{character vector of levels to remove}
\item{na}{Value to set \code{NA} to}
\item{name}{all levels if there exist additional levels that are not in \code{x}}
}

\examples{
data(usethnicity)
race<-as.td(strsplit(as.character(usethnicity$Q5),""))
tdtable(race)
race<-td_drop(race,c(" ","F","G","H"))
tdtable(race)

## How many do people identify with
table(td_count(race))

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
