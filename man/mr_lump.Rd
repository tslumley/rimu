\name{mr_lump}
\alias{mr_lump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mr_lump(x, n, prop, other_level = "Other", ties.method = c("min", "average", "first", "last", "random", "max"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{prop}{
%%     ~~Describe \code{prop} here~~
}
  \item{other_level}{
%%     ~~Describe \code{other_level} here~~
}
  \item{ties.method}{
%%     ~~Describe \code{ties.method} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, n, prop, other_level = "Other", ties.method = c("min", 
    "average", "first", "last", "random", "max")) 
{
    if (!inherits(x, "mr")) 
        stop("x must be an 'mr' object")
    ties.method <- match.arg(ties.method)
    levels <- levels(x)
    count <- as.vector(mtable(x))
    total <- length(x)
    if (!xor(missing(n), missing(prop))) {
        new_levels <- ifelse(!in_smallest(count), levels, other_level)
    }
    else if (!missing(n)) {
        if (n < 0) {
            rank <- rank(count, ties = ties.method)
            n <- -n
        }
        else {
            rank <- rank(-count, ties = ties.method)
        }
        if (sum(rank > n) <= 1) {
            return(x)
        }
        new_levels <- ifelse(rank <= n, levels, other_level)
    }
    else if (!missing(prop)) {
        prop_n <- count/total
        if (prop < 0) {
            new_levels <- ifelse(prop_n <= -prop, levels, other_level)
        }
        else {
            if (sum(prop_n <= prop) <= 1) {
                return(x)
            }
            new_levels <- ifelse(prop_n > prop, levels, other_level)
        }
    }
    if (other_level \%in\% new_levels) {
        others <- data.frame(other = apply(x[, new_levels == 
            other_level], 1, any))
        names(others) <- other_level
        others <- as.mr(others)
        kept <- x[, new_levels != other_level]
        rval <- mr_union(kept, others)
    }
    else {
        x
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
