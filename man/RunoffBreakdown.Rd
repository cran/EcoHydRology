\name{RunoffBreakdown}
\alias{RunoffBreakdown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Daily overland runoff breakdown
}
\description{
Determines the percentage of runoff that occurs on each day following a storm event, based on time to peak of a watershed.
}
\usage{
RunoffBreakdown(Tp_hr, a = 4.5, HrPrcDelay = 4, numDaysReturn = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tp_hr}{
Time to peak (hr).  
}
  \item{a}{
The ratio of the time to recession to the time to peak.  Default is 4.5
}
  \item{HrPrcDelay}{
Lag time (hr)
}
  \item{numDaysReturn}{
Number of days to return
}
}

\value{
returns a vector corresponding to the amount of runoff per day
}

\author{
J Archibald
}
\examples{
RunoffBreakdown(5)
}
