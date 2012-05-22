\name{RainHeat}
\alias{RainHeat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Heat from Rain
}
\description{
Temperature added to the land from heat exchange with rain (usually in the context of snowmelt) [kJ m-2 d-1]
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RainHeat(airtemp, rain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{airtemp}{
average dailiy air temperature [C]
%%     ~~Describe \code{airtemp} here~~
}
  \item{rain}{
%%     ~~Describe \code{rain} here~~
depth of rainfall [m]
}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(airtemp,rain){
# temperature added to the land from heat exchange with rain (usually in the context of snowmelt) [kJ m-2 d-1]
#airtemp: average dailiy air temperature [C]
#rain: depth of rainfall [m]

heatcapacity<-4.2  #heat capacity of water [kJ kg-1 C-1]
waterdensity<-1000 #density of water [kg m-3]

return(heatcapacity*waterdensity*rain*(airtemp-0))
  }
}
