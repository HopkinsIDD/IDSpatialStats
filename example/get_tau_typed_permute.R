data(DengueSimulationR01)

r.max<-seq(20,1000,20)
r.min<-seq(0,980,20)
r.mid<-(r.max+r.min)/2

#Lets see if there's a difference in spatial dependence by time case occurs
type<-2-(DengueSimulationR01[,"time"]<75)
tmp<-cbind(DengueSimulationR01,type=type)

typed.tau.R01<-get.tau.typed(tmp,typeA=1,typeB=2,r=r.max,r.low=r.min)
typed.tau.type.null<-get.tau.typed.permute(tmp,typeA=1,typeB=2,r=r.max,r.low=r.min,permutations=100)

null.ci<-apply(typed.tau.type.null,2,quantile,probs=c(0.025,0.975))

plot(r.mid,typed.tau.R01,ylim=range(c(null.ci,typed.tau.R01)),log="y",cex.axis=1.25,,xlab="Distance (m)",ylab="Tau",cex.main=0.9,lwd=2,type="n")
abline(h=1,lty=1)
lines(r.mid,typed.tau.R01,pch=20,col=1,lwd=3)
lines(r.mid, null.ci[1,] , lty=2)
lines(r.mid, null.ci[2,] , lty=2)
