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
EnvirEnergy(lat, Jday, Tx, Tn, wind, relativehumidity, cloudiness, albedo, forest, slope, aspect, surftemp, surfemissivity,rain)
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
  \item{relativehumidity}{
Relative humidity; if negative, air vapor density will be approximated [-]
}
  \item{cloudiness}{
Fraction of the sky covered in clouds,if negative, cloudiness will be approximated [-]
}
  \item{albedo}{
#albedo: surface albedo or reflectivity [-]
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
  \item{rain}{
precipitation [mm/day]
}

}
\details{
}
\value{
\item{NetEnergy}{Surface net energy}
.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Walter, M.T.,Fuka, D.R.
Maintainer: Daniel Fuka <drf28@cornell.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
