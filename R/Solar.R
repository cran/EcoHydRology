Solar <-  function (lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0, units="kJm2d", latUnits = "unknown", printWarn=TRUE) {
	
	if ((abs(lat) > pi/2 & latUnits == "unknown") | latUnits == "degrees" ){
		if (printWarn==TRUE) print("Note: Latitude in degrees")
		lat <- lat*pi/180
	} else if (latUnits == "unknown"){
		if (printWarn==TRUE) print("Input latitude in radians")
	}
	
	if (units == "kJm2d") convert <- 1 else convert <- 86.4  # can convert to W/m2
    return( signif((1 - albedo) * (1 - forest) * transmissivity(Tx, Tn) * 
        PotentialSolar(lat, Jday) * slopefactor(lat, Jday, slope, aspect) / convert , 2 ))
}