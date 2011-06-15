EvapHeat <-
function(surftemp,airtemp,relativehumidity,Tn,wind){
# Evaporative heat exchange between a surface and the surrounding air; usually cooling [kJ m-2 d-1]
# this function is only intended for wet surfaces, i.e., it assumes the vapor density at the surface is the saturation vapor density

#surftemp: surface temperature [C]
#airtemp: average dailiy air temperature [C]
#relativehumidity: relative humidity [-]
#Tn: minimum dailiy air temperature, assumed to be the dewpoint temperature [C]
#wind: average daily windspeed [m/s]

#NOTE: this function will use the relative humidity to esimate air vapor density if the value passed is greater than zero (0)
# If the relative humidity is less than one, we will assume the minimum daily air temperature is approsimate the dew point temp.

windfunction<-5.3*(1+wind)

if(relativehumidity>0){airvapordensity<-relativehumidity*SatVaporDensity(airtemp)}
else{airvapordensity<-SatVaporDensity(Tn)}

surfacevapordensity<-SatVaporDensity(surftemp)

return(86400*windfunction*(surfacevapordensity-airvapordensity))
}

