get_cfsr_latlon<-function(declat,declon,emailaddr,timeoff=0,interppow=2){
#
# Grabs historical CFSR data through time for a given lat and lon (over unfrozen land surface) using the service at: drfuka.org
#
library(XML)
options(timeout=120)
url=paste("http://www.cfsr.tamu-cornell.drfuka.org/swat-cfsr-v02.pl?lat=",declat,"&lon=",declon,"&timeoff=",timeoff,"&interppow=",interppow,"&.submit=Submit",sep="")
hist_wx=readHTMLTable(url,which=1,header=T,colClasses=c("character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
hist_wx$DATE=as.Date(hist_wx$DATE,format="%Y-%m-%d")
return(hist_wx)
}


