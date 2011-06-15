\name{slopefactor}
\alias{slopefactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Slope Factor

}
\description{
Adjusts solar radiation for land slope and aspect relative to the sun, 1=level ground
}
\usage{
slopefactor(lat, Jday, slope, aspect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
 latitdue [rad]
}
  \item{Jday}{
 Julian date or day of the year [day]
}
  \item{slope}{
 slope of the ground [rad]
}
  \item{aspect}{
 ground aspect [rad from north]
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
function(lat,Jday,slope,aspect){
# slopefactor: adjusts solar radiation for land slope and aspect relative to the sun, 1=level ground

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#slope: slope of the ground [rad]
#aspect: ground aspect [rad from north]

return(cos(slope)-sin(slope)*1/tan(solarangle(lat,Jday))*cos(aspect-solaraspect(lat,Jday)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
