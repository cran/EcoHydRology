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
RainHeat(20,0.01)
}
