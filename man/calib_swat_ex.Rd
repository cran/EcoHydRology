\name{calib_swat_ex}
\alias{calib_swat_ex}
\title{
A function demostrate an example series of steps to calibrate a reach in the SWAT2005 watershed model.
}
\description{
A function demostrate an example series of steps to calibrate a reach in the SWAT2005 watershed model.
This should act only as a simple demonstration, and should be modified to inidivduals use.
}
\usage{
calib_swat_ex(flowgage, rch = 3)
}
\arguments{
  \item{flowgage}{
A list in the format that would be returned from the get_usgs_gage
}
  \item{rch}{
The reach in the output.rch file you wish to calibrate against.
}
}
\details{
This should act only as a simple demonstration, and should be modified to inidivduals use.
}
\value{
List (of length 2) containing the results of the internally called DEoptim function. See DEoptim for more information.
}
\author{
Daniel R. Fuka 
}
\examples{
\dontrun{
#
install.packages("SWATmodel")
#
# Load library, test SWAT numerics, define a flow gage id for USGS gage, or build similar list
# of data as presented in ?get_usgs_gage
#
library(SWATmodel)
testSWAT2005()
flowgage_id="04216500"
flowgage=get_usgs_gage(flowgage_id)
#
# Obtain basic set of historical forcing data from TAMU/Cornell/IWMI CFSR repository
#
hist_wx=get_cfsr_latlon(flowgage$declat,flowgage$declon)
#
# Build a generic 9 HRU SWAT initialization, which builds and changes into directory
# named as flowgage_id above.
#
build_swat_basic(dirname=flowgage_id,iyr="1979",nbyr=6,flowgage$area,
flowgage$elev,flowgage$declat,flowgage$declon,hist_wx=hist_wx)
#
# Simple calibration based on the outflow from RCH 3 of the simple SWAT initialization
#
calib_swat_results=calib_swat_ex(flowgage,rch=3)
#
# Graph output as hydro graph and pred/obs graphics
plot(calib_swat_results$flowdata,flowgage$flowdata)

}
}
