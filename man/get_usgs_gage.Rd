\name{get_usgs_gage}
\alias{get_usgs_gage}
\title{
A function to grab daily stream flow measurements from the USGS waterdata.usgs.gov server.
}
\description{
A function to grab daily stream flow measurements from the USGS waterdata.usgs.gov server.
}
\usage{

get_usgs_gage(flowgage_id,begin_date="1979-01-01",end_date="2013-01-01")

}
\arguments{
  \item{flowgage_id}{
  \code{flowgage_id} Gage is a TEXT/String rather than numeric or the query will fail on gages with leading 0s
}
  \item{begin_date}{
  \code{begin_date} begin_date is a TEXT/String for the start date for the data you want. Default is the beginning of the cfsr dataset.
}
  \item{end_date}{
  \code{end_date} end_date is a TEXT/String for the end date for the data you want. Default is in the future sometime
}
}
\value{
\item{area}{Is the area above the gage}
\item{declat}{Decimal Lat}
\item{declon}{Decimal Lon}
\item{flowdata}{Dataframe with the historical flow data - flow is in cubic meters per day}
}
\author{
Daniel R Fuka
}

\examples{

flowgage_id="04216500"
flowgage=get_usgs_gage(flowgage_id)

## The function is currently defined as
}
