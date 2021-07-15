\name{wth_data}
\alias{wth_data}
\docType{data}
\title{
Example weather data
}
\description{
Example weather data
}
\usage{data("wth_data")}
\format{
  A data frame with n observations on the following 4 variables.
  \describe{
    \item{\code{sensor}}{a character vector giving the sensor information}
    \item{\code{variable}}{a character vector indicating the type of weather information. Must be one of: Temperature (Â°C), Relative humidity (\%)), Windspeed average (m/s), Windspeed max (m/s), Solar radiation (W/(s*mÂ²)), Precipitation (mm), or Wind direction (Â°).}
    \item{\code{timestamp}}{a character vector indicating the time of measurement in format yyyy-mm-dd hh:mm:ss.}
    \item{\code{value}}{a numeric vector indicating the variable value.}
  }
}

\examples{
data(wth_data)
}
\keyword{datasets}
