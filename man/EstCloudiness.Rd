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
EstCloudiness(Tx=(-999), Tn=(-999), trans=NULL, transMin = 0.15, transMax = 0.75, opt = "linear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tx}{
 maximum daily temperature [C]
}
  \item{Tn}{
 minimum daily temperature [C]
}
\item{trans}{
 transmissivity of the atmosphere (value between 0-1)
}
\item{transMin}{
 Transmissivities equal and below this value will return a cloudiness value of 1 (for linear approximation)
}
\item{transMax}{
 Transmissivities equal and above this value will return a cloudiness value of 0 (for linear approximation)
}
\item{opt}{
 Currently there are two options: "linear" (Campbell 1985) and "Black" (1956)
}
}
\references{
Campbell G.S. (1985) Soil Physics with Basic:  Transport Models for Soil Plant Systems.  Elsevier, New York

Black, J.N. (1956) The Distribution of Solar Radiation over the Earth's Surface.  Theoretical and Applied Climatology 7:2 165-189
}
\author{
M. Todd Walter, Josephine Archibald
}
\examples{
EstCloudiness(trans=0.2)
EstCloudiness(20,12,opt="Black")
}