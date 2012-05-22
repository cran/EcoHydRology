\name{AtmosphericEmissivity}
\alias{AtmosphericEmissivity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Atmospheric Emissivity
}
\description{
The emissivity of the atmsophere [-]
}
\usage{
AtmosphericEmissivity(airtemp, cloudiness)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{airtemp}{
Air temperature: air temperature [C]
}
  \item{cloudiness}{
Cloudiness: fraction of the sky covered in clouds [-]
}
}
\value{
The emissivity of the atmsophere [-]
}
\references{
Campbell, G. S., Norman, J.M., 1998. An Introduction to 
Environmental Biophysics, seconded., Springer, New York, 
p. 286.

}
\author{
Fuka, D.R., Walter, M.T.
}

\examples{

temp=15
clouds=.5
AtmEm=AtmosphericEmissivity(temp,clouds)
print(AtmEm)


## The function is currently defined as
function(airtemp,cloudiness){
# the emissivity of the atmsophere [-]

#airtemp: air temperature [C]
#cloudiness: fraction of the sky covered in clouds [-]

return((0.72+0.005*airtemp)*(1-0.84*cloudiness)+0.84*cloudiness)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
