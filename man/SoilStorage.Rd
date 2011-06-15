\name{SoilStorage}
\alias{SoilStorage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating S in the Curve Number Equation from soil water content
%%  ~~function to do ... ~~
}
\description{
This function calculates S, used in the SCS-CN equation, from the water content of the soil.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SoilStorage(S_avg, field_capacity, soil_water_content, porosity)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S_avg}{
Average S, as used normally in the CN equation, calculated from the curve number, which is based on land-use.  This is in units of depth, often mm or inches
  %%     ~~Describe \code{S.avg} here~~
}
  \item{field_capacity}{
field capacity - the amount of water that a soil can hold after drainage.  (fraction)
  %%     ~~Describe \code{field.capacity} here~~
}
  \item{soil_water_content}{
Soil water content (fraction) on a given day.  
%%     ~~Describe \code{soil.water.content} here~~
}
  \item{porosity}{
Saturated water content, approximately equal to the porosity of a soil (fraction)
%%     ~~Describe \code{porosity} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Produces a value for S in the CN equation for any given day, based on the soil moisture of that day. 
The resulting units will be the same as for S_avg 
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
Josephine Archibald
%%  ~~who you are~~
}
\note{
This equation is not the same as the one used in SWAT.  It was given to Dr. Todd Walter by Dr. Keith E. Saxton
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
SoilStorageSWAT
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(S.avg,field.capacity, soil.water.content, porosity){
w2<-(log(1-(field.capacity/(1-0.4348*S.avg/(2.381*S.avg)))-field.capacity)-log(1-(porosity/(1-2.54/(2.381*S.avg)))-porosity
))/(porosity-field.capacity)##
w1<-log(1-field.capacity/(1-0.4348*S.avg/(2.381*S.avg))-field.capacity)+w2*field.capacity
return(2.381*S.avg*(1-(soil.water.content/(soil.water.content+exp(w1-w2*soil.water.content)))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
