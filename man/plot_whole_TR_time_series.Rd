% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_plot_whole_TR_time_series.R
\name{plot_whole_TR_time_series}
\alias{plot_whole_TR_time_series}
\title{plot_whole_TR_time_series}
\usage{
plot_whole_TR_time_series(
  results,
  sector_sel,
  main = "Transpiration time series plot"
)
}
\arguments{
\item{results}{object of class TRres obtained with \code{\link{TR_data_proc}}}

\item{sector_sel}{Vector of numeric values specifying which sector need to be ploted.}

\item{main}{Title of the graph.}
}
\value{
time series plot
}
\description{
A function to plot the complete transpiration time series
obtained with \code{\link{TR_data_proc}} for selected sectors.
}
\examples{

\dontrun{

# Transpiration rate data processing obtained with
# TR_data_proc
data("TR_res")

p <- plot_whole_TR_time_series(results = TR_res, sector_sel = c(2, 8))
p

}

}
