\name{SoilStorage}
\alias{SoilStorage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating S in the Curve Number Equation from soil water content
%%  ~~function to do ... ~~
}
\description{
This function calculates S, used in the SCS-CN equation, from the water content of the soil.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SoilStorage(S_avg, field_capacity, soil_water_content, porosity)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S_avg}{
Average S, as used normally in the CN equation, calculated from the curve number, which is based on land-use.  This is in units of depth, often mm or inches
  %%     ~~Describe \code{S.avg} here~~
}
  \item{field_capacity}{
field capacity - the amount of water that a soil can hold after drainage.  (fraction)
  %%     ~~Describe \code{field.capacity} here~~
}
  \item{soil_water_content}{
Soil water content (fraction) on a given day.  
%%     ~~Describe \code{soil.water.content} here~~
}
  \item{porosity}{
Saturated water content, approximately equal to the porosity of a soil (fraction)
%%     ~~Describe \code{porosity} here~~
}
}
\author{
Josephine Archibald
}
\note{
This equation is not the same as the one used in SWAT.  It was given to Dr. Todd Walter by Dr. Keith E. Saxton
}

\seealso{
SoilStorageSWAT
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
SoilStorage(S_avg=120, field_capacity=0.2, soil_water_content=0.1, porosity=0.3)
}
