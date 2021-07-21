\donttest{

# compare normally distributed with uniform points
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

tau<-get.tau(x,fun,r=r.max,r.low=r.min,"representative", data.frame = TRUE)
tau.ci = get.tau.ci(x, fun, r.max, r.min, 50, "representative", 0.95, data.frame = TRUE)

## plot.tau() method
plot.tau(tau, r.mid = TRUE, ptwise.CI = tau.ci)

## previous plot() method using connected lines to join the top and bottoms of the pointwise CIs. 
#This may lead the user to perform graphical hypothesis testing using this plot without considering 
#the specific distance band of interest before plotting.
plot(r.mid, tau$tau.pt.est ,ylim=c(min(tau.ci$ci.low),max(tau.ci$ci.high)), type="l", log="y")
lines(c(0,100),c(1,1), lty=3, col="grey")
lines(r.mid, tau.ci$ci.low, lty=2)
lines(r.mid, tau.ci$ci.high, lty=2)
}