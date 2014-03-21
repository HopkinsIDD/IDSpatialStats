data(DengueSimulationR01)
data(DengueSimulationR02)

r.max<-seq(20,1000,20)
r.min<-seq(0,980,20)
r.mid<-(r.max+r.min)/2

sero.type.func<-function(a,b,tlimit=20){
  if(a[5]==b[5]&(abs(a[3]-b[3])<=tlimit)){rc=1}
  else{rc=2}
  return(rc)
}

geno.type.func<-function(a,b,tlimit=20){
  if(a[4]==b[4]&(abs(a[3]-b[3])<=tlimit)){rc=1}
  else{rc=2}
  return(rc)
}

sero.tau.R01<-get.tau(DengueSimulationR01,sero.type.func,r=r.max,r.low=r.min)
geno.tau.R01<-get.tau(DengueSimulationR01,geno.type.func,r=r.max,r.low=r.min)

sero.tau.R02<-get.tau(DengueSimulationR02,sero.type.func,r=r.max,r.low=r.min)
geno.tau.R02<-get.tau(DengueSimulationR02,geno.type.func,r=r.max,r.low=r.min)

plot(r.mid,sero.tau.R01,ylim=c(0.3,max(geno.tau.R01)),log="y",cex.axis=1.25,col=rgb(t(col2rgb("blue")/255),alpha=0.6),xlab="Distance (m)",ylab="Tau",cex.main=0.9,lwd=2,type="n")
abline(h=1,lty=2)
abline(v=100,lty=1,lwd=3)
lines(r.mid,sero.tau.R01,pch=20,col=rgb(t(col2rgb("blue")/255),alpha=0.6),lwd=5)
lines(r.mid,geno.tau.R01,pch=20,col=rgb(t(col2rgb("dark green")/255),alpha=0.6),lwd=5)
lines(r.mid,sero.tau.R02,pch=20,col=rgb(t(col2rgb("blue")/255),alpha=0.6),lwd=5,lty=2)
lines(r.mid,geno.tau.R02,pch=20,col=rgb(t(col2rgb("dark green")/255),alpha=0.6),lwd=5,lty=2)
legend("topright",legend=c("Genotype (R0=1)","Serotype (R0=1)","Genotype (R0=2)","Serotype (R0=2)","Max transmission distance"),lwd=3,col=c("dark green","blue","dark green","blue","black"),lty=c(1,1,2,2,1),bty="n",cex=1.25)
