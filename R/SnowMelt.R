SnowMelt<-function(Date, precip_mm, Tmax_C, Tmin_C, lat_deg, slope=0, aspect=0, tempHt=1, windHt=2, groundAlbedo=0.25, 
					SurfEmissiv=0.95, windSp=2, forest=0, startingSnowDepth_m=0){	
## Constants :
	WaterDens<-1000			# kg/m3
	lambda<-3.35*10^5		# latent heat of fusion (kJ/m3)
	lambdaV<-2500			# (kJ/kg) latent heat of vaporization
	SnowHeatCap<-2.1		#kJ/kg/C
	LatHeatFreez<-333.3		# kJ/kg
	
##	Converted Inputs :
	Tav<-(Tmax_C+Tmin_C)/2		# degrees C
	precip_m<-precip_mm*0.001	# precip in m 
	R_m<-precip_m			# (m) depth of rain
	R_m[which(Tav<0)]<-0
	NewSnowDensity<-50+3.4*(Tav+15)		# kg/m3
	NewSnowDensity[which(NewSnowDensity<50)]<-50
	NewSnowWatEq<-precip_m				# m
	NewSnowWatEq[which(Tav>0)]<-0		
	NewSnow<-NewSnowWatEq*WaterDens/NewSnowDensity		# m
	JDay<-strptime(Date, format="%Y-%m-%d")$yday+1
	lat<-lat_deg*pi/180		#	latitude in radians
	rh<-log((windHt+0.001)/0.001)*log((tempHt+0.0002)/0.0002)/(0.41*0.41*windSp*86400)	# (day/m) Thermal Resistance	 
	if (length(windSp)==1) rh<-rep(rh,length(precip_mm))	##	creates a vector of rh values
	cloudiness<-vector(length=length(precip_mm))
	cloudiness<-EstCloudiness(Tmax_C,Tmin_C)
	AE<-AtmosphericEmissivity(Tav, cloudiness)	# (-) Atmospheric Emissivity
	
	SnowTemp<-rep(0,length(precip_m)) 	# Degrees C
	rhos<-SatVaporDensity(SnowTemp)	# 	vapor density at surface (kg/m3)
	rhoa<-SatVaporDensity(Tav)		#	vapor density of atmoshpere (kg/m3) 
						#Note, this differs from the paper, which uses Tmin	
	SnowWaterEq<-vector(length=length(precip_mm))
	SnowWaterEq[1]<-startingSnowDepth_m/10	##	Assume snow is 10% water
	TE<-rep(SurfEmissiv,length(precip_mm))
	DCoef<-rep(0,length(precip_mm))			##	Rough Approximation 
	SnowDensity<-rep(450,length(precip_mm))	## kg/m3.  Max density is 450
	SnowDepth<-vector(length=length(precip_mm))
	SnowDepth[1]<-startingSnowDepth_m			##	m
	SnowMelt<-rep(0,length(precip_mm))			## 	m
	Albedo<-rep(groundAlbedo,length(precip_mm))
	
##	Energy Terms
	H<-vector(length=length(precip_mm))	#	Sensible Heat exchanged (kJ/m2/d)
	E<-vector(length=length(precip_mm))	#	Vapor Energy	(kJ/m2/d)
	S<-vector(length=length(precip_mm))	#	Solar Radiation (kJ/m2/d)
	La<-Longwave(AE, Tav)				#	Atmospheric Longwave Radiation (kJ/m2/d)
	Lt<-vector(length=length(precip_mm))	#	Terrestrial Longwave Radiation (kJ/m2/d)
	G<-173								#	Ground Condution (kJ/m2/d) 
	P<-4.2*10^3*R_m*Tav					# 	Precipitation Heat (kJ/m2/d)
	Energy<-vector(length=length(precip_mm))	# Net Energy (kJ/m2/d)
	Energy[1]<-0
	

	
	for (i in 2:length(precip_m)){
		if (NewSnow[i]>0){ 
			Albedo[i]<-0.98-(0.98-Albedo[i-1])*exp(-4*NewSnow[i]*10)
		}else if (SnowDepth[i-1]<0.1){ 
			Albedo[i]<-max(0.25, Albedo[i-1]+(groundAlbedo-0.85)/10)
		} else Albedo[i]<-0.35-(0.35-0.98)*exp(-1*(0.177+(log((-0.3+0.98)/(Albedo[i-1]-0.3)))^2.16)^0.46)
		S[i]<-Solar(lat=lat,Jday=JDay[i], Tx=Tmax_C[i], Tn=Tmin_C[i], albedo=Albedo[i-1], forest=forest, aspect=aspect, slope=slope)
		if(SnowDepth[i-1]>0) TE[i]<-0.97 	#	(-) Terrestrial Emissivity
		if(SnowWaterEq[i-1]>0 | NewSnowWatEq[i]>0) {
			DCoef[i]<-6.2
			if(SnowMelt[i-1]==0){ 
				SnowTemp[i]<-max(min(0,Tmin_C[i]),min(0,(SnowTemp[i-1]+min(-SnowTemp[i-1],Energy[i-1]/((SnowDensity[i-1]*
					SnowDepth[i-1]+NewSnow[i]*NewSnowDensity[i])*SnowHeatCap*1000)))))
			}
		}
		rhos[i]<-SatVaporDensity(SnowTemp[i])
		H[i]<-1.29*(Tav[i]-SnowTemp[i])/rh[i] 
		E[i]<-lambdaV*(rhoa[i]-rhos[i])/rh[i]
		Lt[i]<-Longwave(TE[i],SnowTemp[i])
		Energy[i]<-S[i]+La[i]-Lt[i]+H[i]+E[i]+G+P[i]
		if (Energy[i]>0) k<-2 else k<-1
		SnowDensity[i]<-ifelse((SnowDepth[i-1]+NewSnow[i])>0, 
			min(450,((SnowDensity[i-1]+k*30*(450-SnowDensity[i-1])*exp(-DCoef[i]))*SnowDepth[i-1]+
			NewSnowDensity[i]*NewSnow[i])/(SnowDepth[i-1]+NewSnow[i])),450)
		SnowMelt[i]<-max(0,min((SnowWaterEq[i-1]+NewSnowWatEq[i]),(Energy[i]-SnowHeatCap*
			(SnowWaterEq[i-1]+NewSnowWatEq[i])*WaterDens*(0-SnowTemp[i]))/(LatHeatFreez*WaterDens))) 
		SnowDepth[i]<-(SnowWaterEq[i-1]+NewSnowWatEq[i]-SnowMelt[i])*WaterDens/SnowDensity[i]
		SnowWaterEq[i]<-max(0,SnowWaterEq[i-1]-SnowMelt[i]+NewSnowWatEq[i])	# (m) Equiv depth of water 	
	}
	
	Results<-data.frame(Date, Tmax_C, Tmin_C, R_m*1000, NewSnowWatEq*1000,SnowMelt*1000, NewSnow, SnowDepth)
	colnames(Results)<-c("Date", "MaxT_C", "MinT_C", "Rain_mm", "SnowfallWatEq_mm", "SnowMelt_mm", "NewSnow_m", "SnowDepth_m")
	return(Results)
}	
