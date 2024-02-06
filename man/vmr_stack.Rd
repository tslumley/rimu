\name{vmr_stack}
\alias{vmr_stack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stack a multiple response column in a tibble
}
\description{
Similar to \code{\link{mr_stack}}, but it works on whole data frames
rather than individual vectors, expanding a multiple-response variable
to a factor with a record for each observed response.  The algorithm is
to convert the object to a logical matrix, \code{pivot_longer}, then
\code{filter} on the logical value.
}
\usage{
vmr_stack(data, col, names_to = "level")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a tibble
  }
  \item{col}{
the name of a multiple-response column in \code{data}
  }
  \item{names_to}{
the desired output name of the variable with the stacked levels
}
}

\value{
An expanded data frame
}


\seealso{
\code{\link{mr_stack}}
}
\examples{
data(ethnicity)

if(require("tidyr",quietly=TRUE)){

 t<-tibble(a=LETTERS[1:6], e=as.vmr(ethnicity,na.rm=TRUE))
 t
 t |> vmr_stack(e,names_to="ethnicity")


}

}
 \keyword{manip}
