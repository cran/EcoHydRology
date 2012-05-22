EstCloudiness <-
function(Tx,Tn){
# estimates the cloudiness of the atmosphere by scaling to atmospheric transmissivity
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]
trans<-transmissivity(Tx,Tn)
cloudiness<-1-(trans-0.15)/(0.75-0.15)
cloudiness[which(cloudiness>1)]<-1
cloudiness[which(cloudiness<0)]<-0
return(cloudiness)		
}

