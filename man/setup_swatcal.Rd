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
Daniel Fuka drf28@cornell.edu
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
