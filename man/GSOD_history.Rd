\name{GSOD_history}
\alias{GSOD_history}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
 ish-history.txt 
}
\description{
A station list to be used with the GSOD data files,
showing the names and locations for each station.
Note:  Global summary of day contains a subset of the 
stations listed in this station history.
}
\usage{data(GSOD_history)}
\format{
  A data frame with 30558 observations on the following 12 variables.
  \describe{
    \item{\code{USAF}}{a numeric vector}
    \item{\code{WBAN}}{a numeric vector}
    \item{\code{STATION.NAME}}{a factor with levels }
    \item{\code{CTRY}}{a factor with levels }
    \item{\code{FIPS}}{a factor with levels }
    \item{\code{STATE}}{a factor with levels }
    \item{\code{CALL}}{a factor with levels }
    \item{\code{LAT}}{a numeric vector}
    \item{\code{LON}}{a numeric vector}
    \item{\code{ELEV..1M.}}{a numeric vector}
    \item{\code{BEGIN}}{a numeric vector}
    \item{\code{END}}{a numeric vector}
  }
}
\details{
}
\source{
ftp://ftp.ncdc.noaa.gov/pub/data/gsod/ish-history.csv
}
\references{
}
\examples{
data(GSOD_history)
}
\keyword{datasets}
