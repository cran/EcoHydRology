\name{declination}
\alias{declination}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Solar Declination
}
\description{
Solar declination for a given day of the year.
}
\usage{
declination(Jday)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Jday}{
Julian date or day of the year [day]
}
}
\author{
M. Todd Walter
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Jday){
# solar declination [rad]
#
#Jday: Julian date or day of the year [day]
#
return(0.4102*sin(pi*(Jday-80)/180))
  }
}
