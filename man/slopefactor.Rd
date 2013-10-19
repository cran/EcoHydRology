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
\author{
M. Todd Walter, J. Archibald
}
\references{
Monteith and Unsworth (1990) Principles of Environmental Physics, 2nd ed. Chapter 5 
}
