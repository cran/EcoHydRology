solaraspect <-
function(lat,Jday){
# aspect angle of sun from north at solar noon [rad]

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]

return(acos((sin(0)*sin(lat) - sin(declination(Jday)))/(cos(0)*cos(lat))))
}

