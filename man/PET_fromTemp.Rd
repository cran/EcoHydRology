\name{PET_fromTemp}
\alias{PET_fromTemp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Priestley-Taylor Potential Evapotranspiration from temperature
}
\description{
Calculates potential Evapotranspiration (in meters) based on the Priestley-Taylor equation (1972).  We use an estimation of
net radiation based on temperature data.
}
\usage{
PET_fromTemp(Jday, Tmax_C, Tmin_C, lat_radians, 
AvgT = (Tmax_C + Tmin_C)/2, albedo = 0.18, TerrestEmiss = 0.97, 
aspect = 0, slope = 0, forest = 0, PTconstant=1.26,
AEparams=list(vp=NULL, opt="linear"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Jday}{
Day of the year
}
  \item{Tmax_C}{
Maximum daily temperature (degrees C)
}
  \item{Tmin_C}{
Minimum daily temperature (degrees C)
}
  \item{lat_radians}{
latitude (radians = decimal degrees*pi/180)
}
  \item{AvgT}{
Average daily temperature (degrees C) (if not known, will be taken as the averages of the daily extremes)
}
  \item{albedo}{
(-) average surface albedo.  Can be expressed as a single value, or as a vector with the same length as Jday, Tmax_C and Tmin_C
}
  \item{TerrestEmiss}{
(-) Surface Emissivity - defaults to 0.97
}
  \item{aspect}{
(radians) Surface aspect
}
  \item{slope}{
(radians) average slope
}
  \item{forest}{
(-) Forest or shade cover (0-1).  This modifies the amount of solar radiation reaching the location of interest.  It should always set to zero for landscape-wide processes regardless of the amount of forest present.  Only change this if calculating PET under a canopy.
}
  \item{PTconstant}{
(-) Priestley-Taylor Constant, often 1.26
}
  \item{AEparams}{
Atmospheric Emissivity options.  Defaults to linear approximation.  To use Brutsaert option, include vapor pressure values (kPa)
}
}
\value{
PET (potential evapotranspiration) in m
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Archibald JA, Walter MT.  (in review)  Towards a physically-based potential evapotranspiration estimate based on temperature.  JAWRA

Priestley and Taylor (1972).  On the assessment of surface heat flux and evaporation using large-scale parameters. 
Mon. Weath. Rev. 100: 81-92
}
\author{
Josephine Archibald, M. Todd Walter
}
\note{
We are assuming that the Ground heat flux on a daily time-step is zero  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(OwascoInlet)
head(OwascoInlet)
attach(OwascoInlet)
PETapprox <- PET_fromTemp(Jday=(1+as.POSIXlt(date)$yday), Tmax_C=Tmax_C,
 Tmin_C=Tmin_C, lat_radians=42.45*pi/180)
plot(PETapprox*1000~date, type="l")
detach(OwascoInlet)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

