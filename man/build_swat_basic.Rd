\name{build_swat_basic}
\alias{build_swat_basic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function builds a basic SWAT initialization. 
}
\description{
This function builds a basic SWAT initialization. It will ask for a directory to put the data into, and then change into that directory. You can then run the example using runSWAT() function. 
}
\usage{
build_swat_basic()
}
%- maybe also 'usage' for other objects documented here.
\author{
Daniel R. Fuka
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(){
tmpdir=readline("Please enter a temp directory where you want to build your run...\n")
dir.create(tmpdir)
setwd(tmpdir)
for (file in names(swat_general)){print(file); cat(unlist(swat_general[file]),file=file,sep="\n")}
  }
}
