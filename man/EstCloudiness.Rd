\name{EstCloudiness}
\alias{EstCloudiness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimated Cloudiness 
}
\description{
Estimates the cloudiness of the atmosphere by scaling to atmospheric transmissivity
}
\usage{
EstCloudiness(lat, Jday, Tx, Tn)
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
function(lat,Jday,Tx,Tn){
# estimates the cloudiness of the atmosphere by scaling to atmospheric transmissivity

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]

trans<-transmissivity(lat,Jday,Tx,Tn)

if(1-(trans-0.15)/(0.75-0.15)<0){return(0)}
else{return(1-(trans-0.15)/(0.75-0.15))
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
