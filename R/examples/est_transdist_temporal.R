set.seed(123)

# Exponentially distributed transmission kernel with mean and standard deviation = 100
dist.func <- alist(n=1, a=1/100, rexp(n, a)) 

# Simulate epidemic
a <- sim.epidemic(R=2,
                  gen.t.mean=7,
                  gen.t.sd=2,
                  tot.generations=7,
                  min.cases=30,
                  trans.kern.func=dist.func)

a <- a[sample(1:nrow(a), 50),] # subsample a to 50 observations

# Estimate mean transmission kernel over time
b <- est.transdist.temporal(epi.data=a,
                            gen.t.mean=7,
                            gen.t.sd=2,
                            t1=0,
                            max.sep=1e10,
                            max.dist=1e10,
                            n.transtree.reps=3,
                            parallel=FALSE)

plot(b, pch=19, col='grey', ylim=c(min(b, na.rm=TRUE), max(b, na.rm=TRUE)), 
     xlab='Time step', ylab='Estimated mean of transmission kernel')

low <- loess(b ~ as.vector(1:length(b)))
low <- predict(low, newdata=data.frame(as.vector(1:length(b))))
lines(low, lwd=3, col='blue')
