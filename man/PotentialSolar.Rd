\name{PotentialSolar}
\alias{PotentialSolar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Potential Solar Radiation 
}
\description{
Potential solar radiation at the edge of the atmospher [kJ m-2 d-1]

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PotentialSolar(lat, Jday)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{Jday}{
%%     ~~Describe \code{Jday} here~~
}
}
\author{
M. Todd Walter
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(lat,Jday){
# potential solar radiation at the edge of the atmospher [kJ m-2 d-1]

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]

# solar declination [rad]
dec<-declination(Jday)

return(117500*(acos(-tan(dec)*tan(lat))*sin(lat)*sin(dec)+cos(lat)*cos(dec)*sin(acos(tan(dec)*tan(lat))))/pi)
  }
}
