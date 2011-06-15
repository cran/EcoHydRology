runSWAT2005 <-
function(){
Sys.setenv(GFORTRAN_STDIN_UNIT=-1)
Sys.setenv(GFORTRAN_STDOUT_UNIT=-1)
Sys.setenv(GFORTRAN_STDERR_UNIT=-1)
library.dynam("EcoHydRology",package="EcoHydRology")
.Fortran("swat2005",PACKAGE="EcoHydRology",1)
library.dynam.unload("EcoHydRology",grep("EcoHydRology",searchpaths(),value=T))
}

