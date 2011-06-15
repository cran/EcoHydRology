\name{get_gsod_stn}
\alias{get_gsod_stn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain Global Summary of Day data
}
\description{
This function will grab available GSOD data for stations and years
for which the data is available.
}
\usage{
get_gsod_stn()
}
\details{
}
\value{
}
\references{
}
\author{
%%  ~~who you are~~
Daniel R. Fuka
}
\note{
}

\seealso{
}
\examples{
# Example of downloading data for ADDIS ABABA
\dontrun{get_gsod_stn()
# Please enter a temp directory ....
addis
# What is the station name you want to look for?
ADDIS\ A
#        USAF  WBAN     STATION.NAME ....
#13582 634500 99999 ADDIS ABABA-BOLE ....
#Good Enough?
y
#Given the start and end years, ....
n
#Date range available is 1957 to 2011 , what start year would you like?
1990
#Date range available is 1957 to 2011 , what end year would you like?
1992
#Good enough?
y
}

## The function is currently defined as
function(){
  tmpdir=readline("Please enter a temp directory to store the datafiles in, remember this directory when parsing with build_gsod_forcing_data()? \n")
  dir.create(tmpdir)
  stnlist=GSOD_history
  ANSWER="n"
  while(ANSWER=="n"){
  stnreq=readline("What is the station name you want to look for? \n")
   test=grep(stnreq,stnlist$STATION.NAME)
  print(stnlist[test,])
  ANSWER <- readline("Good enough? ")
  }
  ANSWER <- readline("Given the start and end years, would you like to update the stnlist file from NDCD? (may take a few minutes) ")
  ## a better version would check the answer less cursorily, and
  ## perhaps re-prompt
  if (substr(ANSWER, 1, 1) == "y")
  stnlist=read.csv(url("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/ish-history.csv"))
#
# Add one more request to make sure the number of years is ok as some stations have really long histories.
#
#
  working_tmp <- stnlist$STATION.NAME
  has_stn<- working_tmp \%in\% grep(stnreq, working_tmp, value=TRUE)
  dl_wbans=subset(stnlist, has_stn , USAF:END)
  start_year=min(trunc(subset(stnlist, has_stn, BEGIN)/10000))
  end_year=max(trunc(subset(stnlist, has_stn , END)/10000))
  start_year_rec<- readline(paste("Date range available is",start_year,"to",end_year,", what start year would you like?"))
  end_year_rec<- readline(paste("Date range available is",start_year,"to",end_year,", what end year would you like?"))
  for (wbans in row.names(dl_wbans)){
    start_year=max(start_year_rec,substr(dl_wbans[wbans,"BEGIN"],1,4))
    end_year=min(end_year_rec,substr(dl_wbans[wbans,"END"],1,4))
    print(c(start_year,end_year))
    if(start_year > end_year) next()
    for (year in start_year:end_year){
      dlurl=paste("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/",year,"/",dl_wbans[wbans,1],"-",dl_wbans[wbans,2],"-",year,".op.gz",sep="")
      dlfile=paste(tmpdir,"/",dl_wbans[wbans,1],"-",dl_wbans[wbans,2],"-",year,".op.gz",sep="")
      print(dlurl)
      try(download.file(dlurl,dlfile),silent=T)

    }
  }
  files=dir(tmpdir,"*op.gz")
  print(files)
  ANSWER <- readline("Good enough? ")

  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
