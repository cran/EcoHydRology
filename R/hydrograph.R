hydrograph <-
function(input=matrix(ncol=2,nrow=2),streamflow=input[,2],timeSeries=input[,1],streamflow2=NULL,precip=NULL,begin=1,
endindex=length(streamflow), P.units="",S.units=P.units, S1.col="black", S2.col="red", stream.label=paste("Streamflow ",S.units)){
if (is.null(streamflow2) & (ncol(input)>3)) streamflow2<-input[,4]## allows data frame to be entered with one parameter
if (is.null(precip) & (ncol(input)>2)){ 
precip<-input[,2]
streamflow<-input[,3]
}## assumes that if precip is included in the input data, it is in the second column, and streamflow is in third
if (is.null(precip)) NULL##If no precip, then skip the first step
else{
par(mar=c(3, 5,1,4))
barplot(precip[begin:endindex],yaxt="n",space=NULL,ylim = rev(c(0,4*max(precip[begin:endindex]))), xaxt="n")
axis(side=3, pos=0, tck=0)
axis(side=4, at = seq(0,floor(max(precip[begin:endindex])+1),length=(1+ifelse(floor(max(precip[begin:endindex])+1)<10,floor(max(precip[begin:endindex])+1),4))), 
labels=as.integer(seq(0,floor(max(precip[begin:endindex])+1),length=(1+ifelse(floor(max(precip[begin:endindex])+1)<10,floor(max(precip[begin:endindex])+1),4)))))
mtext(paste("Precipitation ",P.units),4,line=2, cex=0.9, adj=1)
par(new=T)
}
plot(streamflow[begin:endindex], col=S1.col, type="l",lwd=1, ylab=stream.label, xaxt="n",xlab="date",
ylim=c(0,1.2*max(streamflow[begin:endindex],streamflow2[begin:endindex])),axes=FALSE )
lines(streamflow2[begin:endindex], col=S2.col, lwd=1, lty=2, xaxt="n")
axis(side=1, at=seq(1,(endindex-begin+1),length=14), pos=0,
labels=format(timeSeries[seq(begin,endindex,length=14)],"%d-%b-%y"))
axis(side=2, pos=0)
}

