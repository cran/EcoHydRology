\name{Solar}
\alias{Solar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Solar Radiation
}
\description{
Solar radiation at the ground surface [kJ m-2 d-1]
}
\usage{
Solar(lat, Jday, Tx, Tn, albedo, forest, slope, aspect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
 latitdue [rad]
}
  \item{Jday}{
 Julian date or day of the year [day]
}
  \item{Tx}{
 maximum daily temperature [C]
}
  \item{Tn}{
 minimum daily temperature [C]
}
  \item{albedo}{
 surface albedo or reflectivity [-]
}
  \item{forest}{
 forest or vegeation cover [-]
}
  \item{slope}{
 slope of the ground [rad]
}
  \item{aspect}{
 ground aspect [rad from north]
}
}
\details{
}
\value{
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function(lat,Jday,Tx,Tn,albedo,forest,slope,aspect){
# solar radiation at the ground surface [kJ m-2 d-1]

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]
#albedo: surface albedo or reflectivity [-]
#forest: forest or vegeation cover [-]
#slope: slope of the ground [rad]
#aspect: ground aspect [rad from north]

return((1-albedo)*(1-forest)*transmissivity(lat,Jday,Tx,Tn)*PotentialSolar(lat,Jday)*slopefactor(lat,Jday,slope,aspect))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
