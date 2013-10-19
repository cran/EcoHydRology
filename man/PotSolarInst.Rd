\name{PotSolarInst}
\alias{PotSolarInst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Instantaneous Potential Solar Radiation 
}
\description{
Potential Solar Radiation at a particular time of day.  Defaults to W/m2, can also report in kJ/m2/d if units set to kJ/m2/d
}
\usage{
PotSolarInst(Jday, hour = 12, lat = 42.44 * pi/180, sunrise = NULL,
 sunset = NULL, SolarNoon = mean(c(sunrise, sunset)), units = "Wm2",
 latUnits = "unknown")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Jday}{
Day of the year [-]
}
  \item{hour}{
Time of the day in hours [0-24 hr]
}
  \item{lat}{
latitude.  Default is radians, but will automatically convert from degrees if value is larger than 1.62 or less than -1.62
}
  \item{sunrise}{
Time of sunrise used to calculate solar noon [0-24 hr]
}
  \item{sunset}{
Time of sunset used to calculate solar noon [0-24 hr]
}
  \item{SolarNoon}{
Time of solar noon.  Can be calculated from sunrise and sunset times. [hr]
}
  \item{units}{
Units of the result.  Defaults to W/m2
}
  \item{latUnits}{
Latitude units can be explicitly stated here, options are 'radians', 'degrees' or default is 'unknown', which will assume radians unless the absolute value of lat is greater than pi/2
}
}

\references{
Lawrence Dingman. Physical Hydrology.  Waveland Press, Inc.  Illinois, 2002.
}
\author{
Josephine Archibald
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

PotSolarInst(Jday=150, hour = 15, lat = 42.44, SolarNoon = 12.5) 
PotSolarInst(Jday=c(1,50,100,150), hour = c(9,10,12,17), lat = -pi/4, SolarNoon = 12.5) 

}
