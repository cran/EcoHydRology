\name{transmissivity}
\alias{transmissivity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transmissivity
}
\description{
Transmissivity
Fraction of direct solar radiation passing through the atmosphere based on the Bristow-Campbell eqn
}
\usage{
transmissivity(Tx,Tn, A=0.75, C=2.4, opt="1day", JD=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{Tx}{
maximum daily temperature [C]
}
  \item{Tn}{
minimum daily temperature [C]
}
\item{A}{
Maximum transmissivity in a location, varies with environmental conditions and elevation
}
\item{C}{
Empirical partitioning coefficient - set to 2.4
}
\item{opt}{
Options:  "1day" uses the diurnal temperature change as Tx-Tn for any given day;
"2day" depends on an average temperature change, i.e. the max temp today minus the average of the min temps of today and tomorrow;
Users who do not have a continuous record of daily temperatures should use "missingdays" - note that this depends on JD input.
If JD is null and "missingdays" is chosen the function will default to "1day"
}
\item{JD}{
A vector of julian days corresponding to temp measurements.  Only needed if using opt="missingdays" 
}
}
\details{
Can accept either single values or vectors for Tx,Tn and JD.  
}
\references{
Bristow KL, Campbell GS. 1984. 
On the relationship between incoming solar radiation and daily maximum and minimum temperature. 
Agric Forest Meteorol; 31:150-166.
}
\author{
M. Todd Walter, Josephine Archibald
}
\examples{
##  Assuming the temperature measurements are consecutive days, default option:
transmissivity(Tx=c(10,10,10,10,10), Tn=c(1,2,6,9,2))
## Second option:
transmissivity(Tx=c(10,10,10,10,10), Tn=c(1,2,6,9,2), opt="2day")
##  When the days are not consecutive:
transmissivity(Tx=c(10,10,10,10,10), Tn=c(1,2,6,9,2), JD=c(250,265,300,320,321), opt="missingdays")
}