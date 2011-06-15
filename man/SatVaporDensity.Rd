\name{SatVaporDensity}
\alias{SatVaporDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Saturated Vapor Density
}
\description{
Saturated vapor density at a given temperature

}
\usage{
SatVaporDensity(temp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp}{
 temperature [C]
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
function(temp){
# saturated vapor density at a given temperature

#temp: temperature [C]

return(exp((16.78*temp-116.9)/(temp+273.3))*1/((273.15+temp)*0.4615))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
