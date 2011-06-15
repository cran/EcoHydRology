slopefactor <-
function(lat,Jday,slope,aspect){
# slopefactor: adjusts solar radiation for land slope and aspect relative to the sun, 1=level ground

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#slope: slope of the ground [rad]
#aspect: ground aspect [rad from north]

return(cos(slope)-sin(slope)*1/tan(solarangle(lat,Jday))*cos(aspect-solaraspect(lat,Jday)))
}

