Solar <-
function(lat,Jday,Tx,Tn,albedo,forest,slope,aspect){
# solar radiation at the ground surface [kJ m-2 d-1]

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]
#albedo: surface albedo or reflectivity [-]
#forest: forest or vegeation cover [-]
#slope: slope of the ground [rad]
#aspect: ground aspect [rad from north]

return((1-albedo)*(1-forest)*transmissivity(lat,Jday,Tx,Tn)*PotentialSolar(lat,Jday)*slopefactor(lat,Jday,slope,aspect))
}

