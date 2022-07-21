\name{plot.mr}
\alias{plot.mr}
\alias{image.mr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot multiple-response objects
}
\description{
The plot method for \code{mr} objects is an UpSet plot, showing co-occurences of the various categories.  The \code{image} method is a heatmap of the variable plotted against itself with \code{\link{mtable}}.
}
\usage{
\method{plot}{mr}(x, ...)
\method{image}{mr}(x, type = c("overlap", "conditional", "association",  "raw"), ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{mr} object
}
\item{type}{\code{"overlap"} is a plot of counts, \code{"conditional"} is of column proportions, \code{"association"} has rows and columns scaled to give unit diagonals. \code{"raw"} just plots \code{as.logical(x)}.}
  \item{\dots}{
Not used
}
}
\value{
Used for its side effect
}
\seealso{
\code{\link{as.mr}}, \code{\link{mtable}}
}
\examples{
data(rstudiosurvey)
other_software<- as.mr(rstudiosurvey[[40]])
## only those with at least 20 responses
common<-mr_lump(other_software, n=20)
common<-mr_drop(common, "None")

## UpSet plot
plot(common)

## images

image(common, type="conditional")
image(common, type="association")
}


