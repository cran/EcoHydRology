\name{SatVapPresSlope}
\alias{SatVapPresSlope}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Slope of the relationship between Saturation Vapor Pressure and Temperature
}
\description{
Calculates the slope of the Saturation Vapor Pressure vs T (kPa/K)
}
\usage{
SatVapPresSlope(temp_C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp_C}{
Air temperature in degrees C
}
}
\references{
Dingman, S. Lawrence. 2002. Physical Hydrology.  Waveland Press Inc.
}
\author{
Josephine Archibald
}
\examples{

## The function is currently defined as
function (temp_C) 
{
    (2508.3/(temp_C + 237.3)^2) * exp(17.3 * temp_C/(temp_C + 
        237.3))
  }
}
