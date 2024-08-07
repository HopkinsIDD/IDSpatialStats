% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialfuncs.r
\name{get.pi.ci}
\alias{get.pi.ci}
\title{Calculate bootstrapped confidence intervals for \code{get.pi} values.}
\usage{
get.pi.ci(
  posmat,
  fun,
  r = 1,
  r.low = rep(0, length(r)),
  boot.iter = 1000,
  ci.low = 0.025,
  ci.high = 0.975,
  data.frame = TRUE
)
}
\arguments{
\item{posmat}{a matrix with columns type, x and y}

\item{fun}{the function to decide relationships}

\item{r}{the series of spatial distances wer are interested in}

\item{r.low}{the low end of each range. 0 by default}

\item{boot.iter}{the number of bootstrap iterations}

\item{ci.low}{the low end of the ci...0.025 by default}

\item{ci.high}{the high end of the ci...0.975 by default}

\item{data.frame}{logical indicating whether to return results as a data frame (default = TRUE)}
}
\value{
a matrix with a row for the high and low values and
    a column per distance
}
\description{
Wrapper to \code{get.pi.bootstrap} that takes care of calculating the
confidence intervals based on the bootstrapped values..
}
\examples{
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


pi<-get.pi(x,fun,r=r.max,r.low=r.min)
pi.ci<-get.pi.ci(x,fun,r=r.max,r.low=r.min,boot.iter=100)

plot(r.mid, pi$pi, type="l")
lines(r.mid, pi.ci[,2] , lty=2)
lines(r.mid, pi.ci[,3] , lty=2)

}
}
\seealso{
Other get.pi: 
\code{\link{get.pi}()},
\code{\link{get.pi.bootstrap}()},
\code{\link{get.pi.permute}()},
\code{\link{get.pi.typed}()},
\code{\link{get.pi.typed.bootstrap}()},
\code{\link{get.pi.typed.permute}()}
}
\author{
Justin Lessler
}
\concept{get.pi}
