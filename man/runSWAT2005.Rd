\name{runSWAT2005}
\alias{runSWAT2005}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This runs the SWAT2005 executable in the current directory.
}
\description{
This function runs the SWAT2005 executable in the current directory.
}
\usage{
runSWAT2005()
}
\author{
Daniel R. Fuka
}
\examples{

## The function is currently defined as
function(){
Sys.setenv(GFORTRAN_STDIN_UNIT=-1)
library.dynam("rswat","EcoHydRology")
.Fortran("swat2005",1)
  }
}
