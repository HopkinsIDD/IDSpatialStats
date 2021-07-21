\donttest{

#compare normally distributed with uniform points
x<-cbind(1,runif(100,-100,100), runif(100,-100,100))
x<-rbind(x, cbind(2,rnorm(100,0,20), rnorm(100,0,20)))
colnames(x) <- c("type","x","y")

fun<-function(a,b) {
     if(a[1]!=2) return(3)
     if (b[1]==2) return(1)
     return(2)
}

r.max<-seq(10,100,10)
r.min<-seq(0,90,10)
r.mid <- (r.max+r.min)/2

tau.CI <- get.tau.ci(x,fun,r=r.max,r.low=r.min,boot.iter=50, comparison.type = "representative")

## plot.tau() method
tau = get.tau(x,fun,r=r.max,r.low=r.min, comparison.type = "representative")
plot.tau(x = tau, ptwise.CI = tau.CI)

## previous plot() method
plot(r.mid, tau.CI$pt.est, ylim=c(min(tau.CI$pt.est,tau.CI$ci.low), 
                                  max(tau.CI$pt.est,tau.CI$ci.high)), type="l", xlab="Distance", 
     ylab="Tau")
lines(r.mid, tau.CI$ci.low , lty=2)
lines(r.mid, tau.CI$ci.high, lty=2)
lines(c(0,100),c(1,1), lty=3, col="grey")
}