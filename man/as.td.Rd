\name{as.td}
\alias{as.td}
\alias{as.td.td}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct tidy multiple-response objects
}
\description{
Constructs \code{td} objects representing multiple-choice questions where more than one choice is allowed.
}
\usage{
as.td(x, ..., name = NULL, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object to be converted to class \code{td}
    }
  \item{\dots}{
    for compatibility; not used
    }
  \item{name}{
    level name (for a vector) or vector of level names to
    replace the column names (for a matrix)
    }
  \item{na.rm}{
    If \code{TRUE}, replace \code{NA} in the input by \code{FALSE}
    }
%%     ~~Describe \code{x} here~~
}
\details{
The representation of \code{td} objects is as a \code{tidyverse} \code{vctrs} vector with an element corresponding to each observation.
}
\value{
Object of class \code{td}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Annie Cohen and Thomas Lumley
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data(nzbirds)
as.td(nzbirds>0)

data(ethnicity)
as.td(ethnicity, na.rm = FALSE)

data(usethnicity)
as.td(strsplit(as.character(usethnicity$Q5),""))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip }% use one of  RShowDoc("KEYWORDS")
\keyword{classes }% __ONLY ONE__ keyword per line
