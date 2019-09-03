\name{usethnicity}
\alias{usethnicity}
\docType{data}
\title{
Data from Youth Risk Behaviour Survey
}
\description{
This data set contains variables on race and ethnic identification from the 2017 Youth Risk Behaviour Survey, together with two variables on smoking behaviour.  The YRBS is a multistage cluster-sampled survey, so valid inference about associations requires using survey design information. This subset is useful only for demonstration purposes. 
}
\usage{data("usethnicity")}
\format{
  A data frame with 14765 observations on the following 4 variables.
  \describe{
    \item{\code{Q4}}{1 is "Hispanic or Latino}
    \item{\code{Q5}}{Character string with zero or more of: A. American Indian or Alaska Native, B. Asian, C. Black or African American, D. Native Hawaiian or Other Pacific Islander, E. White}
    \item{\code{QN30}}{1 is "smoked cigarettes on one or more of the past 30 days"}
    \item{\code{QN31}}{1 is "smoked more than 10 cigarettes per day on the days they smoked during the past 30 days", those who did not smoke at all are \code{NA}}
  }
}

\source{
\url{https://www.cdc.gov/healthyyouth/data/yrbs/data.htm}
}

\examples{
data(usethnicity)
race<-as.mr(strsplit(as.character(usethnicity$Q5),""))
race<-mr_drop(race," ")
mtable(race)
hispanic<-as.mr(usethnicity$Q4==1,"Hispanic")
ethnicity<-mr_union(race,hispanic)
ethnicity[101:120]
}
\keyword{datasets}
