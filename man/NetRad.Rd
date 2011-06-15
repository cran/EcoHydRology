\name{NetRad}
\alias{NetRad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title to be replaced
Daily Net Radiation
}
\description{
Daily net radiation [kJ m-2 d-1]
}
\usage{
NetRad(lat, Jday, Tx, Tn, albedo, forest, slope, aspect, airtemp, cloudiness, surfemissivity, surftemp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{Jday}{
%%     ~~Describe \code{Jday} here~~
}
  \item{Tx}{
%%     ~~Describe \code{Tx} here~~
}
  \item{Tn}{
%%     ~~Describe \code{Tn} here~~
}
  \item{albedo}{
%%     ~~Describe \code{albedo} here~~
}
  \item{forest}{
%%     ~~Describe \code{forest} here~~
}
  \item{slope}{
%%     ~~Describe \code{slope} here~~
}
  \item{aspect}{
%%     ~~Describe \code{aspect} here~~
}
  \item{airtemp}{
%%     ~~Describe \code{airtemp} here~~
}
  \item{cloudiness}{
%%     ~~Describe \code{cloudiness} here~~
}
  \item{surfemissivity}{
%%     ~~Describe \code{surfemissivity} here~~
}
  \item{surftemp}{
%%     ~~Describe \code{surftemp} here~~
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
function(lat, Jday, Tx, Tn, albedo, forest, slope, aspect, airtemp, cloudiness, surfemissivity, surftemp){
# daily net radiation [kJ m-2 d-1]

return(Solar(lat,Jday,Tx,Tn,albedo,forest,slope,aspect)+Longwave(AtmosphericEmissivity(airtemp,cloudiness),airtemp)-Longwave(surfemissivity,surftemp))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
