\name{SatVaporDensity}
\alias{SatVaporDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Saturated Vapor Density
}
\description{
Saturated vapor density at a given temperature

}
\usage{
SatVaporDensity(temp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp}{
 temperature [C]
}
}
\author{
M. Todd Walter
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(temp){
# saturated vapor density at a given temperature

#temp: temperature [C]

return(exp((16.78*temp-116.9)/(temp+273.3))*1/((273.15+temp)*0.4615))
  }
}
