test_that("get.tau returns 1 when labels are ignored", {

    #generate a set of 1000 random points even labeled between the two
    x<-cbind(rep(c(1,2),50), x=runif(100,0,100), y=runif(100,0,100))

    colnames(x) <-c("type","x","y")

    test <- function(a,b) {return(1)}

    #######FIRST WITH REPRESENTATIVE SAMPLE ASSUMED
    #with no lower limit
    res <- get.tau(x,test,seq(10,100,10))$tau
    expect_that(res,equals(rep(1,10)))

    #with lower and upper limit
    res <- get.tau(x,test,seq(10,100,10), seq(0,90,10))$tau
    expect_that(res,equals(rep(1,10)))

    #######SECOND WITH INDEPENDENT PROCESS ASSUMED
    #with no lower limit
    res <- get.tau(x,test,seq(10,100,10), comparison.type="independent")$tau
    expect_that(res,equals(rep(1,10)))

    #with lower and upper limit
    res <- get.tau(x,test,seq(10,100,10), seq(0,90,10), comparison.type="independent")$tau
    expect_that(res,equals(rep(1,10)))
})

test_that("correct results for test case 1 (equilateral triangle)", {
    x <- rbind(c(1,0,0), c(1,1,0),c(2,.5,sqrt(.75)))
    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }

    #######FIRST WITH REPRESENTATIVE SAMPLE ASSUMED
    #first no lower limit
    res <- get.tau(x,test,1.5)$tau
    res2 <- get.tau.typed(x,1,2,1.5)$tau

    expect_that(res, equals(1))
    expect_that(res2, equals(1))

    #now with a lower limit

    res <- get.tau(x,test,1.5,.5)$tau
    res2 <- get.tau.typed(x,1,2,1.5,.5)$tau

    expect_that(res, equals(1))
    expect_that(res2, equals(1))

    #######SECOND WITH INDEPENDENT PROCESS ASSUMED
    #first no lower limit
    res <- get.tau(x,test,1.5, comparison.type="independent")$tau
    res2 <- get.tau.typed(x,1,2,1.5, comparison.type="independent")$tau

    expect_that(res, equals(1))
    expect_that(res2, equals(1))

    #now with a lower limit

    res <- get.tau(x,test,1.5,.5, comparison.type="independent")$tau
    res2 <- get.tau.typed(x,1,2,1.5,.5, comparison.type="independent")$tau

    expect_that(res, equals(1))
    expect_that(res2, equals(1))


})


test_that("get.tau returns appropriate values cannonical test case 2 (points on a line)", {
    x<-rbind(c(1,0,0), c(2,1,0), c(2,-1,0), c(3,2,0),
             c(2,-2,0), c(3,3,0),c(3,-3,0))

    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }


    #######FIRST WITH REPRESENTATIVE SAMPLE ASSUMED
    #tau 0,1.5 should be 2, 1.5-2.5 should be 1 and 2.5+ should be 0
    res <- get.tau(x, test, c(1.5,2.5,Inf), c(0,1.5,2.5))$tau
    res2 <- get.tau.typed(x, 1, 2, c(1.5,2.5,1000), c(0,1.5,2.5))$tau

    expect_that(res,equals(c(2,1,0)))
    expect_that(res2,equals(c(2,1,0)))

    #######SECOND WITH INDEPENDENT PROCESS ASSUMED
    #tau 0,1.5 should be Inf, 1.5-2.5 should be 1 and 2.5+ should be 0
    res <- get.tau(x, test, c(1.5,2.5,Inf), c(0,1.5,2.5), comparison.type="independent")$tau
    res2 <- get.tau.typed(x, 1, 2, c(1.5,2.5,1000), c(0,1.5,2.5), comparison.type="independent")$tau

    expect_that(res,equals(c(Inf,1,0)))
    expect_that(res2,equals(c(Inf,1,0)))
})




test_that("get.tau returns identical results regardless of column order",
          {
              x<-cbind(rep(c(1,2),50), x=runif(100,0,100), y=runif(100,0,100))
              colnames(x) <-c("type","x","y")

              test <- function(a,b) {
                  if (a[1] != 1) return(3)
                  if (b[1] == 1) return(1)
                  return(2)
              }

              res1 <- get.tau(x,test,seq(10,100,10), seq(0,90,10))

              test <- function(a,b) {
                  if (a[3] != 1) return(3)
                  if (b[3] == 1) return(1)
                  return(2)
              }

              res2 <- get.tau(x[,c(3,2,1)],test,seq(10,100,10), seq(0,90,10))

              test <- function(a,b) {
                  if (a[2] != 1) return(3)
                  if (b[2] == 1) return(1)
                  return(2)
              }

              res3 <- get.tau(x[,c(2,1,3)],test,seq(10,100,10), seq(0,90,10))

              expect_that(res1, equals(res2))
              expect_that(res2, equals(res3))

          })


test_that ("get.tau fails nicely if x and y column names are not provided", {

    x<-cbind(rep(c(1,2),50), a=runif(100,0,100), b=runif(100,0,100))

    test <- function(a,b) {
        if (a[1] != 2) return(3)
        if (b[1] == 3) return(1)
        return(2)
    }

    expect_that(get.tau(x,test,seq(10,50,10), seq(0,40,10)),
                throws_error("unique x and y columns must be defined"))

})


test_that ("selection of an invalid comparison type fails nicely", {
    x<-rbind(c(1,0,0), c(2,1,0), c(2,-1,0), c(3,2,0),
             c(2,-2,0), c(3,3,0),c(3,-3,0))

    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }


    expect_that( get.tau(x, test, c(1.5,2.5,Inf), c(0,1.5,2.5), comparison.type="foobar"),
                throws_error("unkown comparison type specified"))
    expect_that( get.tau.typed(x, 1, 2, c(1.5,2.5,1000), c(0,1.5,2.5), comparison.type="foobar"),
                throws_error("unkown comparison type specified"))
})

