% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialfuncs.r
\name{get.pi.typed}
\alias{get.pi.typed}
\title{Optimized version of \code{get.pi} for typed data.}
\usage{
get.pi.typed(
  posmat,
  typeA = -1,
  typeB = -1,
  r = 1,
  r.low = rep(0, length(r)),
  data.frame = TRUE
)
}
\arguments{
\item{posmat}{a matrix with columns type, x and y}

\item{typeA}{the "from" type that we are interested in, -1 is wildcard}

\item{typeB}{the "to" type that we are interested i, -1 is wildcard}

\item{r}{the series of spatial distances wer are interested in}

\item{r.low}{the low end of each range....0  by default}

\item{data.frame}{logical indicating whether to return results as a data frame (default = TRUE)}
}
\value{
pi values for all the distances we looked at
}
\description{
Version of the \code{get.pi} function that is optimized for statically typed data. That is
data where we are interested in the probability of points within some distance of points of
typeA are of typeB.
}
\examples{
\donttest{

data(DengueSimR02)

r.max<-seq(20,1000,20)
r.min<-seq(0,980,20)

#Lets see if there's a difference in spatial dependence by time case occurs
type<-2-(DengueSimR02[,"time"]<120)
tmp<-cbind(DengueSimR02,type=type)

typed.pi<-get.pi.typed(tmp,typeA=1,typeB=2,r=r.max,r.low=r.min)

}
}
\seealso{
Other get.pi: 
\code{\link{get.pi}()},
\code{\link{get.pi.bootstrap}()},
\code{\link{get.pi.ci}()},
\code{\link{get.pi.permute}()},
\code{\link{get.pi.typed.bootstrap}()},
\code{\link{get.pi.typed.permute}()}
}
\author{
Justin Lessler and Henrik Salje
}
\concept{get.pi}
