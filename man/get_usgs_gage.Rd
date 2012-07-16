\name{get_usgs_gage}
\alias{get_usgs_gage}
\title{
A function to grab daily stream flow measurements from the USGS waterdata.usgs.gov server.
}
\description{
A function to grab daily stream flow measurements from the USGS waterdata.usgs.gov server.
}
\usage{
get_usgs_gage(gage)
}
\arguments{
  \item{gage}{
  \code{gage} Gage is a TEXT/String rather than numeric or the query will fail on gages with leading 0s
}
}
\value{
\item{area}{Is the area above the gage}
\item{declat}{Decimal Lat}
\item{declon}{Decimal Lon}
\item{flowdata}{Dataframe with the historical flow data}
}
\author{
Daniel R Fuka
}

\examples{

flowgage_id="04216500"
flowgage=get_usgs_gage(flowgage_id)

## The function is currently defined as
}
