\name{EnvirEnergy}
\alias{EnvirEnergy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Surface Net Energy Budget
}
\description{
Net Energy=S+La-Lt+S+E+R+G, where S is the net incident solar radiation, La is the atmospheric long wave radiation, Lt is the terrestrial long wave radiation, H is the sensible heat exchange, E is the energy flux associated with the latent heats of vaporization and condensation, G is ground heat conduction, P is heat added by precipitation
}
\usage{
EnvirEnergy(lat, Jday, Tx, Tn, wind, rain, relativehumidity, albedo=0.2, cloudiness=NULL, forest=0, slope=0, aspect=0, surftemp=(Tx+Tn)/2, surfemissivity=0.97)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
latitdue [rad]
}
  \item{Jday}{
Julian day / day of the year, 1-365 [day] 
}
  \item{Tx}{
Maximum daily temperature [C]
}
  \item{Tn}{
Minimum daily temperature [C]
}
  \item{wind}{
Average daily windspeed [m/s]
}
  \item{rain}{
precipitation [mm/day]
}
  \item{relativehumidity}{
Relative humidity; if negative, air vapor density will be approximated [-]
}
  \item{albedo}{
#albedo: surface albedo or reflectivity [-]
}
  \item{cloudiness}{
Fraction of the sky covered in clouds,if no value provided, cloudiness will be approximated [-]
} 
  \item{forest}{
Forest or vegeation cover [-]
}
  \item{slope}{
Slope of the ground [rad]
}
  \item{aspect}{
Ground aspect [rad from north]
}
  \item{surftemp}{
surface temperature [C]
}
  \item{surfemissivity}{
surfice emissivity: [-]
}


}
\value{
\item{NetEnergy}{Surface net energy}
.
}
\author{
Walter, M.T.,Fuka, D.R.
Maintainer: Daniel Fuka <drf28@cornell.edu>
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(lat,Jday,Tx,Tn,wind,relativehumidity,cloudiness,albedo,forest,slope,aspect,surftemp,surfemissivity){
if(cloudiness<0){cloudiness<-EstCloudiness(lat,Jday,Tx,Tn)}

airtemp<-(Tx+Tn)/2 #average daily air temperature [C]

return(Solar(lat,Jday,Tx,Tn,albedo,forest,slope,aspect)+Longwave(AtmosphericEmissivity(airtemp,cloudiness),airtemp)-Longwave(surfemissivity,surftemp)+SensibleHeat(surftemp,airtemp,wind)+EvapHeat(surftemp,airtemp,relativehumidity,Tn,wind)+RainHeat(airtemp,rain)+GroundHeat())
  }
}
