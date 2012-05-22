\name{hydrograph}
\alias{hydrograph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a hydrograph
}
\description{
Creates a hydrograph with one or two streamflow data sets, and can include a precipitation hyetograph.  
}
\usage{
hydrograph(input = matrix(ncol = 2, nrow = 2), streamflow = input[, 2], timeSeries = input[, 1], streamflow2 = NULL, precip = NULL, begin = 1, endindex = length(streamflow), P.units = "", S.units = P.units, S1.col = "black", S2.col = "red", stream.label = paste("Streamflow ", S.units))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
This is a data frame with at least two columns of data   
First column:  Must be a time series
Second column:  If including precip, precip. Otherwise, streamflow - AND do not include any other columns
Third column: (Only if precip not included) First streamflow dataset 
Forth column:  (optional) Second streamflow dataset
}
  \item{streamflow}{
vector of streamflow values - not necessary if using "input"
}
  \item{timeSeries}{
vector of time series values - not necessary if using "input"
}
  \item{streamflow2}{
optional vector of streamflow values - not necessary if using "input"
}
  \item{precip}{
vector of precipitation values - not necessary if using "input"
}
  \item{begin}{
If you don't want to graph the whole data set, include the index where you would like to start
}
  \item{endindex}{
If you don't want to graph the whole dataset, include the index where you would like to end
}
  \item{P.units}{
Precipitation units (character)
}
  \item{S.units}{
Streamflow units (character)
}
  \item{S1.col}{
color of the line representing streamflow 
}
  \item{S2.col}{
color of the line representing second streamflow data
}
  \item{stream.label}{
character string - Label for vertical axis
}
}
\author{
Josephine Archibald
}
\note{
If you chose to use the input argument but are not including precipitation, input must only have two columns - you will
need to add the second streamflow dataset using the "streamflow2" argument.
}

\section{Warning }{
This function does not allow NA values.  Remove rows with NA values before running.
}
\examples{
data(OwascoInlet)
head(OwascoInlet)
hydrograph(OwascoInlet)
}
