\name{ConvertFlowUnits}
\alias{ConvertFlowUnits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Unit conversion for flow rates
}
\description{
Converts volumetric flow (cfs, cms, cmd) to depth flow rate over a watershed (mm/d).  
Or, it converts a depth flow rate to volumetric (mm/d to cfs)
}
\usage{
ConvertFlowUnits(cfs = NULL, cmd=NULL, cms = NULL, WA, mmd = NULL, AREAunits = "mi2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cfs}{
Input flow in cubic feet per second
}
  \item{cmd}{
Input flow in cubic meters per day
}
  \item{cms}{
Input flow in cubic meters per second
}
  \item{WA}{
Watershed area.  Can be entered in square miles (default), or square km (in this case, change the AREAunits to "km2")
}
  \item{mmd}{
Input flow in mm/d
}
  \item{AREAunits}{
Units of the watershed area ("mi2" or "km2")
}
}
\details{
Note, only one flow input should be used.  (i.e., user should define cfs OR cmd OR cms OR mmd).  Watershed area must always be defined.
}
\value{
converted flow rate in either mm/d (if converting from volumetric flow) or cfs (if converting from flow depth in mm/d)
}
\author{
Josephine Archibald
}


\examples{
data(OwascoInlet)
OwascoInlet$Streamflow_mmd<-ConvertFlowUnits(cms=OwascoInlet$Streamflow_m3s,WA=271.,AREAunits="km2")

##  The following commented example isn't currently working in Linux systems, but should 
##  work in windows/macs
##
##  Get some streamflow (reported in cubic meters per day here):
# OI <- get_usgs_gage("04235299", "2013-03-01", "2013-05-20")
# FC <- get_usgs_gage("04234000", "2013-03-01", "2013-05-20")

##  Convert to mm/d
# OwascoInlet_mmd <- ConvertFlowUnits(cmd=OI$flowdata$flow, WA=OI$area, AREAunits="km2")
# FallCreek_mmd <- ConvertFlowUnits(cmd=FC$flowdata$flow, WA=FC$area, AREAunits="km2")

##  Compare the watershed area normalized flow depth for two watersheds near Ithaca NY :
# hydrograph(streamflow=OwascoInlet_mmd, streamflow2=FallCreek_mmd, timeSeries=FC$flowdata$mdate, 
# stream.label="flow depth (mm/d)")
# legend("topright", legend=c("Owasco Inlet", "Fall Creek"), lty=c(1,2), col=c("black", "red"))
}
