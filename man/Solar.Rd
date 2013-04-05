\name{Solar}
\alias{Solar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Solar Radiation
}
\description{
Solar radiation at the ground surface [kJ m-2 d-1]
}
\usage{
Solar(lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0, units="kJm2d", latUnits = "unknown", printWarn=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
 latitdue [rad]
}
  \item{Jday}{
 Julian date or day of the year [day]
}
  \item{Tx}{
 maximum daily temperature [C]
}
  \item{Tn}{
 minimum daily temperature [C]
}
  \item{albedo}{
 surface albedo or reflectivity [-]
}
  \item{forest}{
 forest or vegeation cover [-]
}
  \item{slope}{
 slope of the ground [rad]
}
  \item{aspect}{
 ground aspect [rad from north]
}
  \item{units}{
Units of the result.  Defaults to kJ/m2/d, changing this to "Wm2" will make output in W/m2
}
  \item{latUnits}{
Latitude units can be explicitly stated here, options are 'radians', 'degrees' or default is 'unknown', which will assume radians unless the absolute value of lat is greater than pi/2  
}
  \item{printWarn}{
Will print a warning about latitude if set to TRUE
}
}
