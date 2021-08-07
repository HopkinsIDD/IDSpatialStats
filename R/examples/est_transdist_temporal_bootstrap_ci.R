\donttest{

set.seed(123)

# Exponentially distributed transmission kernel with mean and standard deviation = 100
dist.func <- alist(n=1, a=1/100, rexp(n, a)) 

# Simulate epidemic
a <- sim.epidemic(R=2,
                  gen.t.mean=7,
                  gen.t.sd=2,
                  tot.generations=8,
                  min.cases=30,
                  trans.kern.func=dist.func)

a <- a[sample(1:nrow(a), 70),] # subsample a to 70 observations

# Estimate change in mean transmission kernel over time with confidence intervals
b <- est.transdist.temporal.bootstrap.ci(epi.data=a,
                                         gen.t.mean=7,
                                         gen.t.sd=2,
                                         t1=0,
                                         max.sep=1e10,
                                         max.dist=1e10,
                                         n.transtree.reps=10,
                                         mean.equals.sd=TRUE,
                                         boot.iter=10,
                                         ci.low=0.025,
                                         ci.high=0.975,
                                         n.cores=2)

plot(b[,2], pch=19, col='grey', ylim=c(min(b[,1:3], na.rm=TRUE), max(b[,2:4], na.rm=TRUE)), 
     xlab='Time step', ylab='Estimated mean of transmission kernel')
abline(h=100, col='red', lty=2)
axis(3, 1:nrow(b), b[,5])

low <- loess(b[,2] ~ as.vector(1:nrow(b)), span=1)
low <- predict(low, newdata=data.frame(as.vector(1:nrow(b))))
lines(low, lwd=3, col='blue')

for(i in 3:4) {
     low <- loess(b[,i] ~ as.vector(1:nrow(b)), span=1)
     low <- predict(low, newdata=data.frame(as.vector(1:nrow(b))))
     lines(low, lty=2, lwd=3, col='blue')
}

}
