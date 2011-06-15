AtmosphericEmissivity <-
function(airtemp,cloudiness){
# the emissivity of the atmsophere [-]

#airtemp: air temperature [C]
#cloudiness: fraction of the sky covered in clouds [-]

return((0.72+0.005*airtemp)*(1-0.84*cloudiness)+0.84*cloudiness)
}

