\name{alter_files}
\alias{alter_files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to alter files called from swat_objective_function
}
\description{
A function to alter files called from swat_objective_function
}
\usage{
alter_files(change_params)
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
Daniel R. Fuka
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
function(change_params){
library(operators)
for(ft in unique(change_params$filetype)){
        print(ft)
        files=list.files(,paste(ft,"$",sep=""))
        for (file in files) {
                fileorig=paste(file,".unixorig",sep="");
                junk\%<\%fileorig
                file_change_params=subset(change_params,filetype==ft)
                for( i in 1:length(rownames(file_change_params))){
                        startstr=file_change_params[i,"startstr"]
                        endstr=file_change_params[i,"endstr"]
                        current=file_change_params[i,"current"]
                        param=file_change_params[i,"parameter"]
                        multi=file_change_params[i,"multi"]
                        alter_type=file_change_params[i,"alter_type"]
                        frformat=unlist(strsplit(as.character(file_change_params[i,"frformat"]),","))
                        if(multi==F & alter_type=="new"){
                                junk=gsub(paste(".*",param,".*",sep=""),sprintf("\%s\%16.4f\%s",startstr,as.real(current),endstr),junk)
                        }
                        if(multi==T & alter_type=="percent"){
                                junkline=grep(param,junk,value=T)
                                #print(paste(i,junkline,frformat))
                                a=read.fortran(con1<-textConnection(junkline),frformat)
                                close(con1)
                                stringb=sprintf("\%s",a[1])
                                j=2
                                while ( !is.na(a[j])) {
                                        stringb=paste(stringb,sprintf("\%12.2f",a[j]*current),sep="")
                                        j=j+1
                                }
                                junk=gsub(paste(".*",param,".*",sep=""),stringb,junk)
                        }
                }
                cat(junk,file=file,sep="\n")
        }
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
