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

tau <- get.tau.ci(x,fun,r=r.max,r.low=r.min,boot.iter=50)

plot(r.mid, tau$pt.est, ylim=c(1/max(tau[,3:5]), max(tau[,3:5])), type="l", log="y",
     xlab="Distance", ylab="Tau")
lines(r.mid, tau$ci.low , lty=2)
lines(r.mid, tau$ci.high, lty=2)
lines(c(0,100),c(1,1), lty=3, col="grey")

}