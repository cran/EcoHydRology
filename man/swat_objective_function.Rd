\name{swat_objective_function}
\alias{swat_objective_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A basic objective function to calibrate parameters in SWAT
}
\description{
A basic objective function to calibrate parameters in SWAT
}
\usage{
swat_objective_function(x, calib_range, change_params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{calib_range}{
%%     ~~Describe \code{calib_range} here~~
}
  \item{change_params}{
%%     ~~Describe \code{change_params} here~~
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
%%  ~~who you are~~
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
function(x,calib_range,change_params){
library(EcoHydRology)
start_date<-"1993-12-31"
change_params$current<-x
alter_files(change_params)
runSWAT2005()
        sim=read.fortran(file="output.rch",c("X5","F5","X9","F6","X12","F12"),skip=9)
        sim$V4=as.Date(as.integer((row(sim)[,1]+(sim$V2[1]*max(sim$V1))-1)/max(sim$V1)),origin=start_date)
        sim_df=data.frame(date=sim$V4,sim_flow=sim$V3,charstr=sim$V1)
        gdata_df=read.csv("BN_ElDiem_Flow-1-1-1999_12-31-2004ForDan.csv",header=T)
#       gdata=read.table(file="gage.dat",skip=2)
#       gdata_df=data.frame(date=as.Date(gdata$V3,"\%Y-\%m-\%d"),obs_flow=as.numeric(as.character(gdata$V4))*0.0283168466,quality=gdata$V5)
#       gdata_df=data.frame(date=as.Date(gdata$V3,"\%Y-\%m-\%d"),obs_flow=as.numeric(as.character(gdata$V4))*0.0283168466,quality=gdata$V5)
        gdata_df$date=as.Date(gdata_df$Date)
        gdata_df$obs_flow=gdata_df$Flow.cms.
        sim_df=subset(sim_df,date>calib_range[1] & date < calib_range[2] & charstr==5)
        obj_df=merge(sim_df,gdata_df)
#       par(mfrow=c(2,2))
#       plot(obj_df$date,obj_df$obs_flow)
#       plot(obj_df$date,obj_df$sim_flow)
#       plot(obj_df$obs_flow,obj_df$sim_flow)
        NS<-NSeff(obj_df$obs_flow,obj_df$sim_flow)
        print(NS)
        abs(NS-1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
