transmissivity <-
function(Tx,Tn){
len<-length(Tx)
if(len<30){ avDeltaT<-mean(Tx-Tn)
}else {avDeltaT<-vector(length=len)
	avDeltaT[1:14]<-mean(Tx[1:30]-Tn[1:30])
	avDeltaT[(len-14:len)]<-mean(Tx[(len-30:len)]-Tn[(len-30:len)])
	for (i in 15:(len-15)){
		avDeltaT[i]<-mean(Tx[(i-14):(i+15)]-Tn[(i-14):(i+15)])
	}
}
B<-0.036*exp(-0.154*avDeltaT)
return(0.75*(1-exp(-B*(Tx-Tn)^2.4)))
}

