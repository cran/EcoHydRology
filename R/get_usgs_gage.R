get_usgs_gage<-function(gage){
#
# Grabs USGS stream flow data for 1979 to present... to align with CFSR datasets.
#
library(XML)
Sys.setlocale('LC_ALL','C')
url=paste("http://waterdata.usgs.gov/nwis/inventory?search_site_no=",gage,"&search_site_no_match_type=exact&sort_key=site_no&group_key=NONE&format=sitefile_output&sitefile_output_format=html_table&column_name=station_nm&column_name=site_tp_cd&column_name=dec_lat_va&column_name=dec_long_va&column_name=alt_va&column_name=drain_area_va&column_name=contrib_drain_area_va&column_name=rt_bol&list_of_search_criteria=search_site_no",sep="")

tempdf=readHTMLTable(url,which=6,colClasses=c("character","character","numeric","numeric","character","character","numeric","numeric","character","numeric"))
colnames(tempdf)=gsub(" ","",gsub("\\.","",colnames(tempdf)))

area=tempdf$Drainage*1.6^2
declat=tempdf$DecLat
declon=tempdf$DecLon
elev=tempdf$Altitude*12/25.4
gagename=tempdf$SiteName

url=paste("http://nwis.waterdata.usgs.gov/nwis/dv?format=rdb&begin_date=1979-01-01&end_date=2013-01-01&site_no=",gage,sep="")
flowdata=read.table(url,skip=28,col.names=c("agency","site_no","date","flow","quality"),colClasses=c("character","numeric","character","character","character"),fill=T)
flowdata$mdate=as.Date(flowdata$date,format="%Y-%m-%d")-1

flowdata$flow=as.numeric(as.character(flowdata$flow))*12^3*2.54^3/100^3*24*3600
flowdata=na.omit(flowdata)
returnlist=list(declat=declat,declon=declon,flowdata=flowdata,area=area,elev=elev,gagename=gagename)
return(returnlist)
}


