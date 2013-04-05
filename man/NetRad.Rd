\name{NetRad}
\alias{NetRad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Daily Net Radiation
}
\description{
Daily net radiation [kJ m-2 d-1]
}
\usage{
NetRad(lat, Jday, Tx, Tn, albedo = 0.18, forest = 0, slope = 0, aspect = 0, airtemp = (Tn+Tx)/2, cloudiness = "Estimate", surfemissivity = 0.97, surftemp = (Tn+Tx)/2, units = "kJm2d")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
latitude.  Default is radians, but will automatically convert from degrees if value is larger than pi/2 or less than -pi/2
}
  \item{Jday}{
Day of the year [-]
}
  \item{Tx}{
Max temperature [C]
}
  \item{Tn}{
Min temperature [C]
}
  \item{albedo}{
Albedo (0-1) [-]
}
  \item{forest}{
Forest canopy or shade cover (0-1) [-]
}
  \item{slope}{
Slope [radians]
}
  \item{aspect}{
Ground aspect [radians from north]
}
  \item{airtemp}{
Average air temp [C]
}
  \item{cloudiness}{
Cloudiness percentage (0-1) [-] 
Default value will estimate cloudiness based on daily temperature difference. 
}
  \item{surfemissivity}{
Surface emissivity [-]
}
  \item{surftemp}{
Surface temperature [C]
}
  \item{units}{
Units of the result.  Defaults to "kJm2d" which returns result in kJ/m2/d, the other option is "Wm2", which will make output in W/m2
}

}
\author{
M. Todd Walter, Josephine Archibald
}

\examples{
NetRad(42.4*pi/180, Jday=55, Tx=2, Tn=(-3))
}
