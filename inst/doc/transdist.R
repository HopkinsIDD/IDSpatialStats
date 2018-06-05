## ---- echo=F, message=F--------------------------------------------------
library(IDSpatialStats)

## ---- fig.width=7.25, fig.height=4, cache=FALSE, message=F, fig.cap='\\label{fig:sim1} **Left:** the spatial distribution of simulated cases with the red cross showing the index case. **Right:** the simulated epidemic curve giving case counts over time'----
set.seed(123)

# Exponential transmission kernel with mean = sd = 100
dist.func <- alist(n=1, a=1/100, rexp(n, a))

# Simulate epidemic with constant R value
a <- sim.epidemic(R=1.5,
                  gen.t.mean=7,
                  gen.t.sd=2,
                  min.cases=100,
                  tot.generations=10,
                  trans.kern.func=dist.func)
sim.plot(a)

## ---- cache=FALSE--------------------------------------------------------
gen.time <- 1
n.gen <- round((max(case.times) - min(case.times)) / gen.time) + 1 # Number of generations

c <- get.transdist.theta(wal.teun.mat=b,
                         cases=case.times,
                         gen.t.mean=gen.time,
                         max.sep=n.gen*2)

## ---- cache=FALSE--------------------------------------------------------
d <- est.transdist.theta.weights(case.times=case.times,
                                 n.rep=5,
                                 gen.t.mean=gen.time,
                                 t1=0,
                                 t.density=g.x)

## ---- cache=FALSE--------------------------------------------------------
set.seed(123)

dist.func <- alist(n=1, a=1/100, rexp(n, a))

# Simulate epidemic
a <- sim.epidemic(R=2,
                  gen.t.mean=7,
                  gen.t.sd=2,
                  min.cases=100,
                  tot.generations=8,
                  trans.kern.func=dist.func)

# Estimate mean and standara deviation of transmission kernel
b <- est.transdist(epi.data=a,
                   gen.t.mean=7,
                   gen.t.sd=2,
                   t1=0,
                   max.sep=1e10,
                   max.dist=1e10,
                   n.transtree.reps=10)
b

## ---- cache=FALSE--------------------------------------------------------
t <- Sys.time()
b <- est.transdist.bootstrap.ci(epi.data=a,
                                gen.t.mean=7,
                                gen.t.sd=2,
                                t1=0,
                                max.sep=1e10,
                                max.dist=1e10,
                                n.transtree.reps=10,
                                boot.iter=10,
                                ci.low=0.025,
                                ci.high=0.975)
Sys.time() - t
b

## ---- cache=FALSE--------------------------------------------------------
nc <- parallel::detectCores() # use all available cores

t <- Sys.time()
b <- est.transdist.bootstrap.ci(epi.data=a,
                                gen.t.mean=7,
                                gen.t.sd=2,
                                t1=0,
                                max.sep=1e10,
                                max.dist=1e10,
                                n.transtree.reps=10,
                                boot.iter=10,
                                ci.low=0.025,
                                ci.high=0.975,
                                parallel=TRUE,
                                n.cores=nc)
Sys.time() - t
b

## ---- cache=FALSE, fig.height=5, fig.width=6, fig.cap='Output from the `est.transdist.temporal` function showing the change in the mean transmission distance over the course of a simulated epidemic. Point estimates are plotted as grey circles and a loess smooth of the mean estimate is plotted with the blue line.'----
t <- Sys.time()
c <- est.transdist.temporal(epi.data=a,
                            gen.t.mean=7,
                            gen.t.sd=2,
                            t1=0,
                            max.sep=1e10,
                            max.dist=1e10,
                            n.transtree.reps=10)
Sys.time() - t

# Run in parallel
nc <- parallel::detectCores()

t <- Sys.time()
c <- est.transdist.temporal(epi.data=a,
                            gen.t.mean=7,
                            gen.t.sd=2,
                            t1=0,
                            max.sep=1e10,
                            max.dist=1e10,
                            n.transtree.reps=10,
                            parallel=TRUE,
                            n.cores=nc)
Sys.time() - t

plot(c, pch=19, col='grey', ylim=c(min(c, na.rm=TRUE), max(c, na.rm=TRUE)), xlab='Time step', ylab='Estimated mean of transmission kernel')
low <- loess(c ~ as.vector(1:length(c)))
low <- predict(low, newdata=data.frame(as.vector(1:length(c))))
lines(low, lwd=3, col='blue')
abline(h=100, col='red', lty=2)

## ---- cache=FALSE, fig.height=5, fig.width=6, fig.cap='Output from the `est.transdist.temporal.bootstrap.ci` function showing the change in the mean transmission distance over the course of a simulated epidemic. The point estimates are plotted as grey circles and a loess smooth of the mean estimate is plotted (blue line) along with its 95% bootstrapped confidence intervals (dashed blue lines).'----
nc <- parallel::detectCores()

t <- Sys.time()
c <- est.transdist.temporal.bootstrap.ci(epi.data=a,
                                         gen.t.mean=7,
                                         gen.t.sd=2,
                                         t1=0,
                                         max.sep=1e10,
                                         max.dist=1e10,
                                         n.transtree.reps=5,
                                         boot.iter=10,
                                         ci.low=0.025,
                                         ci.high=0.975,
                                         parallel=TRUE,
                                         n.cores=nc)
Sys.time() - t

plot(c[,1], pch=19, col='grey', ylim=c(min(c, na.rm=TRUE), max(c, na.rm=TRUE)),
     xlab='Time step', ylab='Estimated mean of transmission kernel')

low <- loess(c[,1] ~ as.vector(1:nrow(c)), span=1)
low <- predict(low, newdata=data.frame(as.vector(1:nrow(c))))
lines(low, lwd=3, col='blue')

for(i in 2:3) {
low <- loess(c[,i] ~ as.vector(1:nrow(c)), span=1)
low <- predict(low, newdata=data.frame(as.vector(1:nrow(c))))
lines(low, lty=2, lwd=3, col='blue')
}
abline(h=100, col='red', lty=2)

## ---- eval=F-------------------------------------------------------------
#  data("FMD2001")
#  
#  # Estimate transmission distance
#  td <- est.transdist(epi.data=FMD2001,
#                      gen.t.mean=6.1,
#                      gen.t.sd=4.6,
#                      t1=0,
#                      max.sep=1e10,
#                      max.dist=1e10,
#                      n.transtree.reps=10)
#  
#  nc <- parallel::detectCores()
#  
#  # Bootstrapped confidence intervals
#  ci <- est.transdist.bootstrap.ci(epi.data=FMD2001,
#                                   gen.t.mean=6.1,
#                                   gen.t.sd=4.6,
#                                   t1=0,
#                                   max.sep=1e10,
#                                   max.dist=1e10,
#                                   n.transtree.reps=10,
#                                   boot.iter=10,
#                                   ci.low=0.025,
#                                   ci.high=0.975,
#                                   parallel=TRUE,
#                                   n.cores=nc)
#  
#  # Change in transmission distance and its bootstrapped confidence intervals over time
#  tci <- est.transdist.temporal.bootstrap.ci(epi.data=FMD2001,
#                                             gen.t.mean=6.1,
#                                             gen.t.sd=4.6,
#                                             t1=0,
#                                             max.sep=1e10,
#                                             max.dist=1e10,
#                                             n.transtree.reps=10,
#                                             boot.iter=10,
#                                             ci.low=0.025,
#                                             ci.high=0.975,
#                                             parallel=TRUE,
#                                             n.cores=nc)

## ---- echo=FALSE, out.width='100%', fig.cap='Output from the `est.transdist.temporal.bootstrap.ci` function showing the change in the mean transmission distance over the course of the 2001 foot-and-mouth disease epidemic among farms in the UK. The point estimates are plotted as grey circles and a loess smooth of the mean estimate is plotted (blue line) along with its 95% bootstrapped confidence intervals (dashed blue lines).'----
knitr::include_graphics('transdist_files/figure-html/FMDtemporal.png')

