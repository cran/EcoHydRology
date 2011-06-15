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
function(){
tmpdir=readline("Please enter a temp directory where you want to build your run...\n")
dir.create(tmpdir)
setwd(tmpdir)
for (file in names(swat_general)){print(file); cat(unlist(swat_general[file]),file=file,sep="\n")}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
