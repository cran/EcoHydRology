NetRad <- function (lat, Jday, Tx, Tn, albedo = 0.18, forest = 0, slope = 0, aspect = 0, airtemp = (Tn+Tx)/2, cloudiness = "Estimate", surfemissivity = 0.97, surftemp=(Tn+Tx)/2, units = "kJm2d") {
		#  units : kJm3d or Wm2
	if (cloudiness == "Estimate") {
		cloudiness <- EstCloudiness(Tx, Tn)
	}
    if (units == "kJm2d") convert <- 1 else convert <- 86.4
	return( signif((Solar(lat, Jday, Tx, Tn, albedo, forest, slope, aspect) + Longwave(AtmosphericEmissivity(airtemp, cloudiness),airtemp) - Longwave(surfemissivity, surftemp)) / convert ,2 ) )
}