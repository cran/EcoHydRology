\name{Longwave}
\alias{Longwave}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Daily Longwave Radiation
}
\description{
Daily longwave radiation based on the Sephan-Boltzman equation [kJ m-2 d-1]

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Longwave(emissivity, temp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emissivity}{
emissivity [-]
}
  \item{temp}{
temperature of the emitting body [C]
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
function(emissivity,temp){
# daily longwave radiation based on the Sephan-Boltzman equation [kJ m-2 d-1]

#emissivity: [-]
#temp: temperature of the emitting body [C]

SBconstant<-0.00000489 #[kJ m-2 K-4 d-1]

tempK<-temp+273.3 #[degrees K]

return(emissivity*SBconstant*tempK^4)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
