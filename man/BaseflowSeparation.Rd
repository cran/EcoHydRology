\name{BaseflowSeparation}
\alias{BaseflowSeparation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Baseflow Separation
}
\description{
This function reads a streamflow dataset and produces a baseflow dataset.  It can be run using 1, 2 or 3 passes 
}
\usage{
BaseflowSeparation(streamflow, filter_parameter = 0.925, passes = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{streamflow}{
A vector containing streamflow values
}
  \item{filter_parameter}{
The value recommended by Nathan and McMahon (1990) is 0.925, however, the user might want to play with this value (0.9-0.95)
}
  \item{passes}{
The number of times you want the filter to pass over your data.  1-3
}
}
\value{
This will return a 2 column data frame with nrow = length of input streamflow data.  The first column contains
baseflow, while the second contains quickflow, both in the same units as the input.  
}
\references{
Lyne, V. D. and M. Hollick (1979). Stochastic time-variable rainfall-runoff modelling. Hydrology and Water Resources Symposium, Perth, Institution of Engineers, Australia.

Nathan, R. J. and T. A. McMahon (1990). "Evaluation of automated techniques for base flow and recession analysis." Water Resources Research 26(7): 1465-1473.
}
\author{
Josephine Archibald
}
\examples{
#########	Look at a dataset for Owasco Lake in NY:
data(OwascoInlet)
summary(OwascoInlet)

##	Get an approximation for baseflow using a 3 pass filter:
bfs<-BaseflowSeparation(OwascoInlet$Streamflow_m3s, passes=3)

##	You can check out how this looks with the hydrograph function:
hydrograph(input=OwascoInlet,streamflow2=bfs[,1])
}
