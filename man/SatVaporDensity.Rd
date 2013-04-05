\name{SatVaporDensity}
\alias{SatVaporDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Saturated Vapor Density
}
\description{
Saturated vapor density [kg/m3] at a given temperature

}
\usage{
SatVaporDensity(T_C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T_C}{
 temperature [C]
}
}
\references{
Campbell, Gaylon S. An Introduction to Environmental Biophysics.  New York: Springer-Verlag, 1977.
}
\author{
Josephine Archibald, M. Todd Walter
}
\examples{
SatVaporDensity(seq(-5,40, by=5))
  }

