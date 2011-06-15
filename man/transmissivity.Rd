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
transmissivity(lat, Jday, Tx, Tn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
latitdue [rad]
}
  \item{Jday}{
Julian date or day of the year [day]
}
  \item{Tx}{
maximum daily temperature [C]
}
  \item{Tn}{
minimum daily temperature [C]
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
function(lat,Jday,Tx,Tn){
# fraction of direct solar radiation passing through the atmosphere based on the Bristow-Campbell eqn

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]

# B constant based on work by Ndlvou
if(Jday>80 & Jday<262){
  B<-0.282*lat^-0.431} #summer
else{
  B<-0.170*lat^-0.979} #winter

#Potential solar radiation 30 days ago
if(Jday>30){
  PotSolar<-PotentialSolar(lat,Jday-30)}
else{
  PotSolar<-PotentialSolar(lat,365-(30-Jday))}

return(0.75*(1-exp(-B*(Tx-Tn)^2/(PotSolar/1000))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
