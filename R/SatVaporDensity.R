SatVaporDensity <-
function(temp){
# saturated vapor density at a given temperature

#temp: temperature [C]

return(exp((16.78*temp-116.9)/(temp+273.3))*1/((273.15+temp)*0.4615))
}

