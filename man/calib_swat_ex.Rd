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

