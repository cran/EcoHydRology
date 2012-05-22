PET_fromTemp <-
function(Jday, Tmax_C, Tmin_C, lat_radians, AvgT=(Tmax_C+Tmin_C)/2, albedo=0.18, TerrestEmiss=0.97, aspect=0, 
slope=0, forest=0){
# Returns PET in mm
# forest = canopy cover [-]
# lat_radians: in radians (degLat*pi/180)
# albedo can be a vector or single value

if (length(Jday)!=length(Tmax_C) | length(Jday)!=length(Tmin_C)){ 
cat("Warning, input vectors unequal length:  Longer data sets truncated.\n")
length(Jday)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
length(Tmax_C)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
length(Tmin_C)<-min(length(Jday), length(Tmax_C), length(Tmin_C))
}

cloudiness<-EstCloudiness(Tmax_C,Tmin_C)
DailyRad<-NetRad(lat_radians,Jday,Tmax_C,Tmin_C,albedo,forest,slope,aspect,AvgT,cloudiness,TerrestEmiss,AvgT)

Qn<-DailyRad	## Assumes that the Ground heat flux on daily time-step is zero

#####Constants
PTconstant<-1.26 # [-] Generic Priestly-Taylor constant
LatentHtEvap<-2500 # [kJ/kg]
DensityWater<-1000 # [kg/m3]
PsychConstant<-0.066#[kPa/K]

potentialET<-PTconstant*SatVapPresSlope(AvgT)*Qn/((SatVapPresSlope(AvgT)+PsychConstant)*(LatentHtEvap*DensityWater))
potentialET[which(potentialET<0)]<-0
potentialET[which(Tmax_C==-999 | Tmin_C==-999)]<-(-999)
return(potentialET)
}
