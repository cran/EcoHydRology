\name{OwascoInlet}
\alias{OwascoInlet}
\docType{data}
\title{
Owasco Inlet data
}
\description{
Streamflow and temperature data for Owasco Inlet from USGS and NRCC
}
\usage{data(OwascoInlet)}
\format{
  A data frame with 888 observations on the following 6 variables.
  \describe{
    \item{\code{date}}{a Date}
    \item{\code{P_mm}}{a numeric vector}
    \item{\code{Streamflow_m3s}}{a numeric vector}
    \item{\code{baseflow_m3s}}{a numeric vector}
    \item{\code{Tmax_C}}{a numeric vector}
    \item{\code{Tmin_C}}{a numeric vector}
  }
}
\source{
http://www.usgs.gov/
http://www.nrcc.cornell.edu/
}
\references{
Data taken from USGS (www.usgs.gov) and NRCC (http://www.nrcc.cornell.edu)
}
\examples{
data(OwascoInlet)
## maybe str(OwascoInlet) ; plot(OwascoInlet) ...
}
\keyword{datasets}
