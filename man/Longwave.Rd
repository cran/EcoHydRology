\name{Longwave}
\alias{Longwave}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Daily Longwave Radiation
}
\description{
Daily longwave radiation based on the Sephan-Boltzman equation [kJ m-2 d-1]

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Longwave(emissivity, temp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emissivity}{
emissivity [-]
}
  \item{temp}{
temperature of the emitting body [C]
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
function(emissivity,temp){
# daily longwave radiation based on the Sephan-Boltzman equation [kJ m-2 d-1]

#emissivity: [-]
#temp: temperature of the emitting body [C]

SBconstant<-0.00000489 #[kJ m-2 K-4 d-1]

tempK<-temp+273.3 #[degrees K]

return(emissivity*SBconstant*tempK^4)
  }
}
