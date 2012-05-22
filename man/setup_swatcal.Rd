\name{setup_swatcal}
\alias{setup_swatcal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to setup a swat calibration
%%  ~~function to do ... ~~
}
\description{
A function to setup a swat calibration, building the template *.unixorig template files, and cleaning up oddities we find in the default input files. (eg file.cio has cst.cst file which appears to be a bug)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setup_swatcal(change_params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{change_params}{
%%     ~~Describe \code{change_params} here~~
}
}
\author{
Daniel Fuka drf28@cornell.edu
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(change_params){
library(operators)
for(ft in unique(change_params$filetype)){
        print(ft)
        files=list.files(,paste(ft,"$",sep=""))
        for (file in files) {
                junk\%<\%file
                junk=gsub("\r","",junk)
                file_swatcal=paste(file,".unixorig",sep="");
                cat(junk,file=file_swatcal,sep="\n")
        }
  }
  }
}
