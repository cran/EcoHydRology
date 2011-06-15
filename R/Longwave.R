Longwave <-
function(emissivity,temp){
# daily longwave radiation based on the Sephan-Boltzman equation [kJ m-2 d-1]

#emissivity: [-]
#temp: temperature of the emitting body [C]

SBconstant<-0.00000489 #[kJ m-2 K-4 d-1]

tempK<-temp+273.3 #[degrees K]

return(emissivity*SBconstant*tempK^4)
}

