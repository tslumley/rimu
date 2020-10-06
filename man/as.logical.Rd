\name{as.logical.td}
\alias{as.logical.td}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct logical matrices
}
\description{
Converts \code{td} objects into logical matrices.
}
\usage{
as.logical.td(x, levels = NULL, ..., na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object to be converted into a logical matrix.
}
  \item{levels}{
    Levels to be used as columns for the matrix. The default assigns the unique objects in \code{x} as levels.
  }
  \item{na.rm}{
    If \code{TRUE}, replace \code{NA} in the input by \code{FALSE}, The default is \code{FALSE}.
  }
}
\details{
The object represents each element of \code{x} as a row of \code{TRUE} and \code{FALSE} within a matrix, where the columns are the unique objects within the elements of \code{x}.
}
\value{
Matrix of class \code{logical}
}
\references{

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
data(ethnicity)
y <- as.td(ethnicity, na.rm = TRUE)
as.logical(y)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
