test_that("get.pi returns 1 when labels are ignored", {

    #generate a set of 100 random points even labeled between the two
    x<-cbind(rep(c(1,2),50), x=runif(100,0,100), y=runif(100,0,100))

    colnames(x) <-c("type","x","y")

    test <- function(a,b) {return(1)}

    #with no lower limit
    res <- get.pi(x,test,seq(10,100,10))$pi
    expect_that(res,equals(rep(1,10)))

    #with lower and upper limit
    res <- get.pi(x,test,seq(10,100,10), seq(0,90,10))$pi
    expect_that(res,equals(rep(1,10)))
})

test_that("get.pi returns appropriate values for cannonical test case 1 (equilateral triangle)", {

    x <- rbind(c(1,0,0), c(1,1,0),c(2,.5,sqrt(.75)))
    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }

    #first no lower limit
    res <- get.pi(x,test,1.5)$pi
    res2 <- get.pi.typed(x,1,2,1.5)$pi

    expect_that(res, equals(.5))
    expect_that(res2, equals(.5))

    #now with a lower limit

    res <- get.pi(x,test,1.5,.5)$pi
    res2 <- get.pi.typed(x,1,2,1.5,.5)$pi

    expect_that(res, equals(.5))
    expect_that(res2, equals(.5))

})

test_that("get.pi returns appropriate values cannonical test case 2 (points on a line)", {
    x<-rbind(c(1,0,0), c(2,1,0), c(2,-1,0), c(3,2,0),
             c(2,-2,0), c(3,3,0),c(3,-3,0))

    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }

    #pi 0,1.5 should be 1, 1.5-2.5 should be 0.5 and 2.5+ should be 0
    res <- get.pi(x, test, c(1.5,2.5,Inf), c(0,1.5,2.5))$pi
    res2 <- get.pi.typed(x, 1, 2, c(1.5,2.5,1000), c(0,1.5,2.5))$pi

    expect_that(res,equals(c(1,0.5,0)))
    expect_that(res2,equals(c(1,0.5,0)))

})


test_that("get.pi and get.pi.typed have same behavior on random data", {

    #generate a set of 1000 random points even labeled between the two
    x<-cbind(rep(c(1,2),50), x=runif(100,0,100), y=runif(100,0,100))

    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 1) return(1)
        return(2)
    }

    #no lower limit
    res1 <- get.pi(x,test,seq(10,100,10))$pi
    res2 <- get.pi.typed(x, 1,1, seq(10,100,10))$pi
    expect_that(res1,equals(res2))

    #lower limit
    res1 <- get.pi(x,test,seq(10,100,10), seq(0,90,10))$pi
    res2 <- get.pi.typed(x, 1,1, seq(10,100,10), seq(0,90,10))$pi
    expect_that(res1,equals(res2))
})

test_that("get.pi returns identical results regardless of column order",
          {
              x<-cbind(rep(c(1,2),50), x=runif(100,0,100),
                       y=runif(100,0,100))

              colnames(x) <-c("type","x","y")

              test <- function(a,b) {
                  if (a[1] != 1) return(3)
                  if (b[1] == 1) return(1)
                  return(2)
              }

              res1 <- get.pi(x,test,seq(10,100,10), seq(0,90,10))$pi

              test <- function(a,b) {
                  if (a[3] != 1) return(3)
                  if (b[3] == 1) return(1)
                  return(2)
              }

              res2 <- get.pi(x[,c(3,2,1)],test,seq(10,100,10), seq(0,90,10))$pi

              test <- function(a,b) {
                  if (a[2] != 1) return(3)
                  if (b[2] == 1) return(1)
                  return(2)
              }

              res3 <- get.pi(x[,c(2,1,3)],test,seq(10,100,10), seq(0,90,10))$pi

              expect_that(res1, equals(res2))
              expect_that(res2, equals(res3))

          })


test_that ("get.pi fails nicely if x and y column names are not provided", {
    x<-cbind(rep(c(1,2),500), a=runif(1000,0,100), b=runif(1000,0,100))

    test <- function(a,b) {
        if (a[1] != 2) return(3)
        if (b[1] == 3) return(1)
        return(2)
    }

    expect_that(get.pi(x,test,seq(10,50,10), seq(0,40,10))$pi,
                throws_error("unique x and y columns must be defined"))

})

