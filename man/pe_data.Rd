\name{pe_data}
\alias{pe_data}
\docType{data}
\title{
Example plant eye data
}
\description{
Example plant eye data containing the leaf area information for 10 genotypes from a BC-NAM population experiment.
}
\usage{data("pe_data")}
\format{
  A data frame with n observations on the following 5 variables.
  \describe{
    \item{\code{sector}}{a character vector describing the sector position}
    \item{\code{genotype}}{a numeric or character vector indicating the genotype numbers.}
    \item{\code{replicate}}{a numeric vector indicating the replication number. If not available or not existing set to NA.}
    \item{\code{timestamp}}{a character vector indicating the time of measurement in format yyyy-mm-dd hh:mm:ss.}
    \item{\code{leaf_area}}{a integer vector indicating the weight (mass) measured in g.}
  }
}

\examples{
data(pe_data)
}
\keyword{datasets}
