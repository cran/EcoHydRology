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
