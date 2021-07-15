\name{lc_data}
\alias{lc_data}
\docType{data}
\title{
Example load cell data
}
\description{
Example load cell data containing the mass information for 10 genotypes from a BC-NAM population experiment.
}
\usage{data("lc_data")}
\format{
  A data frame with n observations on the following 6 variables.
  \describe{
    \item{\code{sector}}{a character vector describing the sector position}
    \item{\code{genotype}}{a numeric or character vector indicating the genotype numbers.}
    \item{\code{g_alias}}{a character vector indicating the genotype identifiers.}
    \item{\code{treatment}}{a character vector indicating the experiment treatment. If not available set to NA.}
    \item{\code{timestamp}}{a character vector indicating the time of measurement in format yyyy-mm-dd hh:mm:ss.}
    \item{\code{Mass..g}}{a integer vector indicating the weight (mass) measured in g.}
  }
}

\examples{
data(lc_data)
}
\keyword{datasets}
