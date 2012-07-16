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
AtmosphericEmissivity(airtemp, cloudiness, vp=NULL, opt="linear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{airtemp}{
Air temperature: air temperature [C]
}
  \item{cloudiness}{
Cloudiness: fraction of the sky covered in clouds [-]
}
\item{vp}{
Vapor Pressure :  [kPa]
}
\item{opt}{
option: either "linear" for a linear relationshipd between clear sky emissivity and temperature, or
"Brutseart" to use Brutseart's(1975) clear sky emissivity formulation - this requires input for vapor pressure
}
}
\value{
The emissivity of the atmsophere [-]
}
\references{
Campbell, G. S., Norman, J.M., 1998. An Introduction to 
Environmental Biophysics, second ed., Springer, New York, 
p. 286.

Brutseart, W. (1975 On a Derivable Formula for Long-Wave Radiation From Clear Skies.  Water Resources Research, 11(5) 742-744

}
\author{
Fuka, D.R., Walter, M.T., Archibald, J.A.
}

\examples{

temp=15
clouds=.5
AtmEm=AtmosphericEmissivity(temp,clouds)
print(AtmEm)
}


