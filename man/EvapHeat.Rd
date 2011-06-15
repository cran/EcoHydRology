\name{EvapHeat}
\alias{EvapHeat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaporative heat exchange between a surface and the surrounding air
}
\description{
Evaporative heat exchange between a surface and the surrounding air;
usually cooling [kJ m-2 d-1]
this function is only intended for wet surfaces, i.e., it assumes the vapor density at the surface is the saturation vapor density

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EvapHeat(surftemp, airtemp, relativehumidity, Tn, wind)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{surftemp}{
 surface temperature [C]
}
  \item{airtemp}{
 average dailiy air temperature [C]
}
  \item{relativehumidity}{
 relative humidity [-]
}
  \item{Tn}{
 minimum dailiy air temperature, assumed to be the dewpoint temperature [C]
}
  \item{wind}{
 average daily windspeed [m/s]
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
M. Todd Walter
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
function(surftemp,airtemp,relativehumidity,Tn,wind){
# evaporative heat exchange between a surface and the surrounding air; usually cooling [kJ m-2 d-1]
# this function is only intended for wet surfaces, i.e., it assumes the vapor density at the surface is the saturation vapor density

#surftemp: surface temperature [C]
#airtemp: average dailiy air temperature [C]
#relativehumidity: relative humidity [-]
#Tn: minimum dailiy air temperature, assumed to be the dewpoint temperature [C]
#wind: average daily windspeed [m/s]

#NOTE: this function will use the relative humidity to esimate air vapor density if the value passed is greater than zero (0)
# If the relative humidity is less than one, we will assume the minimum daily air temperature is approsimate the dew point temp.

windfunction<-5.3*(1+wind)

if(relativehumidity>0){airvapordensity<-relativehumidity*SatVaporDensity(airtemp)}
else{airvapordensity<-SatVaporDensity(Tn)}

surfacevapordensity<-SatVaporDensity(surftemp)

return(86400*windfunction*(surfacevapordensity-airvapordensity))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
