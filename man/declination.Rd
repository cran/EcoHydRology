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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
M. Todd Walter
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
