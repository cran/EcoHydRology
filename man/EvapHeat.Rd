\name{EvapHeat}
\alias{EvapHeat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaporative heat exchange between a wet surface and the surrounding air
}
\description{
Evaporative heat exchange between a surface and the surrounding air [kJ m-2 d-1]. This function is only intended for wet surfaces, i.e., it assumes the vapor density at the surface is the saturation vapor density

}
\usage{
EvapHeat(surftemp, airtemp, relativehumidity=NULL, Tn=NULL, wind=2)
}

\arguments{
  \item{surftemp}{
 surface temperature [C]
}
  \item{airtemp}{
 average daily air temperature [C]
}
  \item{relativehumidity}{
 relative humidity, 0-1 [-]
}
  \item{Tn}{
 minimum dailiy air temperature, assumed to be dew point temperature if relativehumidity unknown [C]
}
  \item{wind}{
 average daily windspeed [m/s]
}
}
\references{
Campbell, Gaylon S. An Introduction to Environmental Biophysics.  New York: Springer-Verlag, 1977.
}
\author{
M. Todd Walter
}
\note{
This function will use the relative humidity to esimate air vapor density if the value passed is between 0-1.  Otherwise, it will assume the minimum daily air temperature is approximately the dew point temp.
}
\examples{
	EvapHeat(surftemp=15, airtemp=5, relativehumidity=0.7)
 }

