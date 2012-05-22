\name{solaraspect}
\alias{solaraspect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Solar Aspect
}
\description{
Aspect angle of sun from north at solar noon [rad]

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
solaraspect(lat, Jday)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
 latitdue [rad]
}
  \item{Jday}{
 Julian date or day of the year [day]
}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(lat,Jday){
# aspect angle of sun from north at solar noon [rad]

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]

return(acos((sin(0)*sin(lat) - sin(declination(Jday)))/(cos(0)*cos(lat))))
  }
}
