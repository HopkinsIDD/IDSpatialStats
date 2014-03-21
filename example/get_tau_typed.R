data(DengueSimulationR01)

r.max<-seq(20,1000,20)
r.min<-seq(0,980,20)
r.mid<-(r.max+r.min)/2

#Lets see if there's a difference in spatial dependence by time case occurs
type<-2-(DengueSimulationR01[,"time"]<75)
tmp<-cbind(DengueSimulationR01,type=type)

typed.tau.R01<-get.tau.typed(tmp,typeA=1,typeB=2,r=r.max,r.low=r.min)

plot(r.mid,typed.tau.R01,log="y",cex.axis=1.25,xlab="Distance (m)",ylab="Tau",cex.main=0.9,lwd=2,type="l")
abline(h=1,lty=2)
