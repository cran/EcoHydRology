NetRad <-
function(lat, Jday, Tx, Tn, albedo, forest, slope, aspect, airtemp, cloudiness, surfemissivity, surftemp){
# daily net radiation [kJ m-2 d-1]

return(Solar(lat,Jday,Tx,Tn,albedo,forest,slope,aspect)+Longwave(AtmosphericEmissivity(airtemp,cloudiness),airtemp)-Longwave(surfemissivity,surftemp))
}

