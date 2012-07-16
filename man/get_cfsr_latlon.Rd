\name{get_cfsr_latlon}
\alias{get_cfsr_latlon}
\title{
This is a funciton to grab daily summaries of the CFSR from the drfuka.org service
}
\description{
This is a funciton to grab daily summaries of the CFSR from the drfuka.org service
}
\usage{
get_cfsr_latlon(declat, declon, emailaddr, timeoff = 0, interppow = 2)
}
\arguments{
  \item{declat}{
%%     ~~Describe \code{declat} here~~
}
  \item{declon}{
%%     ~~Describe \code{declon} here~~
}
  \item{emailaddr}{
%%     ~~Describe \code{emailaddr} here~~
}
  \item{timeoff}{
%%     ~~Describe \code{timeoff} here~~
}
  \item{interppow}{
%%     ~~Describe \code{interppow} here~~
}
}
\author{
Daniel R Fuka
}

\examples{
\dontrun{

hist_wx=get_cfsr_latlon(45,-72,"dan@dan.com",timeoff=0,interppow=2)
plot(hist_wx$TMX)

}
## The function is currently defined as
function (declat, declon, emailaddr, timeoff = 0, interppow = 2) 
{
    library(XML)
    options(timeout = 120)
    url = paste("http://www.cfsr.tamu-cornell.drfuka.org/swat-cfsr-v02.pl?lat=", 
        declat, "&lon=", declon, "&timeoff=", timeoff, "&interppow=", 
        interppow, "&.submit=Submit", sep = "")
    hist_wx = readHTMLTable(url, which = 1, header = T, colClasses = c("character", 
        "numeric", "numeric", "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric"))
    hist_wx$DATE = as.Date(hist_wx$DATE, format = "\%Y-\%m-\%d")
    return(hist_wx)
  }
}
