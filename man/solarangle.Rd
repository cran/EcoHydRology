\name{solarangle}
\alias{solarangle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Solar Angle
}
\description{
Angle of solar inclination from horizontal at solar noon [rad]
}
\usage{
solarangle(lat, Jday)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
 latitdue [rad]
%%     ~~Describe \code{lat} here~~
}
  \item{Jday}{
%%     ~~Describe \code{Jday} here~~
 Julian date or day of the year [day]
}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(lat,Jday){
# angle of solar inclination from horizontal at solar noon [rad]

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]

# solar declination [rad]
dec<-declination(Jday)

return(asin(sin(lat)*sin(dec)+cos(lat)*cos(dec)*cos(0)))
  }
}
