\donttest{# Simulate cases (type = 2, Normally-distributed points) and 
# simulated non-cases (type = 1, Uniformally-distributed)
X <- cbind(1, runif(100,-100,100), runif(100,-100,100))
X <- rbind(X, cbind(2,rnorm(100,0,20), rnorm(100,0,20)))
colnames(X) <- c("type","x","y")

fun <- function(a,b) {
    # possible 'ab' pair types {'11'; '12'; '21'; '22'}
    if(a[1]!=2) return(3) # it's {'11' or '12'} so ignore
    if(b[1]==2) return(1) # it's '22' so count as a case-case pair in numerator and denominator
    # else it's a '21' ie a case-non-case pair
    return(2) # so count in denominator
}

# define distance band set
r.max <- seq(10,100,10)
r.min <- seq(0,90,10)
r.mid <- (r.max + r.min)/2

# compute the pi point estimate and its 95% BCa CI
pi <- get.pi(X, fun, r=r.max, r.low = r.min)
pi.ci <- get.pi.ci(X, fun, r = r.max, r.low = r.min, boot.iter = 100)

# plot the pi point estimate with its CI, at the midpoints of each distance band
plot(r.mid, pi$pi, type="l")
lines(r.mid, pi.ci$ci.low, lty=2)
lines(r.mid, pi.ci$ci.high, lty=2)}