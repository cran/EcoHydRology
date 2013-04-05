\name{SnowMelt}
\alias{SnowMelt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Snow Melt Calculator
}
\description{
Calculates snow pack accumulation and melt
}
\usage{
SnowMelt(Date, precip_mm, Tmax_C, Tmin_C, lat_deg, slope = 0, aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25, SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0, startingSnowDensity_kg_m3=450)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Date}{
Vector of dates (class Date or character) in this format: Y-m-d
}
  \item{precip_mm}{
Precipitation in mm
}
  \item{Tmax_C}{
Daily maximum temperature (degrees C)
}
  \item{Tmin_C}{
Daily minimum temperature (degrees C)
}
  \item{lat_deg}{
Degrees latitude
}
  \item{slope}{
Overall slope of area of interest
}
  \item{aspect}{
Aspect of the area of interest
}
  \item{tempHt}{
height of temperature measurements (m)
}
  \item{windHt}{
height of wind measurements (m)
}
  \item{groundAlbedo}{
Ground Albedo, 0-1 (-)
}
  \item{SurfEmissiv}{
Surface Emissivity, 0-1 (-)
}
  \item{windSp}{
Wind speed - either a vector of measured values or a single value of average wind speed for the site (m/s)
}
  \item{forest}{
Forest cover - use this only when determining snowmelt under a canopy, 0-1 (-)
}
  \item{startingSnowDepth_m}{
The depth of the snow pack initially (m)
}
\item{startingSnowDensity_kg_m3}{
The density of snow on the ground on the first day (kg/m3)
}
}
\value{
This will return a 10 column data frame with nrow = length of input weather data.  Column names are :
Date, MaxT_C, MinT_C, Precip_mm, Rain_mm, SnowfallWatEq_mm, SnowMelt_mm, NewSnow_m, SnowDepth_m, SnowWaterEq_mm 
}
\references{
Walter, M.T. , E.S. Brooks, D.K. McCool, L.G. King, M. Molnau, J. Boll. 2005. 
Process-based snowmelt modeling: Does it require more input data than temperature-index modeling? 
Journal of Hydrology 300(1-4): 65-75
}
\author{
Josephine Archibald, M. Todd Walter
}
\examples{
##  
data(OwascoInlet)
sm <- SnowMelt(Date=OwascoInlet$date, precip_mm=OwascoInlet$P_mm, Tmax_C=OwascoInlet$Tmax_C, Tmin_C=OwascoInlet$Tmin_C, lat_deg=42)
summary(sm)
}