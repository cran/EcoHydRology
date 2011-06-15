transmissivity <-
function(lat,Jday,Tx,Tn){
# fraction of direct solar radiation passing through the atmosphere based on the Bristow-Campbell eqn

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]

# B constant based on work by Ndlvou
if(Jday>80 & Jday<262){
  B<-0.282*lat^-0.431} #summer
else{
  B<-0.170*lat^-0.979} #winter

#Potential solar radiation 30 days ago
if(Jday>30){
  PotSolar<-PotentialSolar(lat,Jday-30)}
else{
  PotSolar<-PotentialSolar(lat,365-(30-Jday))}

return(0.75*(1-exp(-B*(Tx-Tn)^2/(PotSolar/1000))))
}

