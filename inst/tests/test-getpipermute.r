

test_that("get.pi.permute returns appropriate values for test case 1 (equilateral triangle)" ,{

    x <- rbind(c(1,0,0), c(1,1,0),c(2,.5,sqrt(.75)))
    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }


    #should return .5 for every permutation
    res <- get.pi.permute(x, test, 1.5, 0, 500)[,-(1:2)]
    res2 <- get.pi.typed.permute(x, 1, 2, 1.5, 0, 500)[,-(1:2)]

    expect_that(as.numeric(res), equals(rep(0.5,500)))
    expect_that(as.numeric(res2), equals(rep(0.5,500)))

})

test_that("get.pi.permute returns appropriate values for test case 2 (points on a line)" ,{
    x<-rbind(c(1,0,0), c(2,1,0), c(2,-1,0), c(3,2,0),
             c(2,-2,0), c(3,3,0),c(3,-3,0))

    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }

    #the mean of the null distribution should be 0.5
    #the 95% CI equals 0,1 with windows
    res <- get.pi.permute(x, test, c(1.5,2.5,3.5), c(0,1.5,2.5), 500)[,-(1:2)]
    res2 <- get.pi.typed.permute(x, 1, 2, c(1.5,2.5,3.5), c(0,1.5,2.5), 500)[,-(1:2)]

    expect_that(rowMeans(res,na.rm=T), equals(rep(.5,3), tolerance=0.1))
    expect_that(rowMeans(res2, na.rm=T), equals(rep(.5,3), tolerance=0.1))

    for (i in 1:3) {
        expect_that(coxed::bca(as.numeric(res[i,]), conf.level = 0.95),
                    equals(c(0,1)))
        expect_that(coxed::bca(as.numeric(res2[i,]), conf.level = 0.95),
                    equals(c(0,1)))
    }

    #without windows the distributions is asymmetric and 95% BCa CI is rather than [0.25,0.75] for 
    # percentile CIs
    res3 <- get.pi.permute(x, test, 4,0, 500)
    res4 <- get.pi.typed.permute(x, 1, 2, 4, 0, 500)
    expect_that(coxed::bca(as.numeric(res3[1,]), conf.level = 0.95), equals(c(1/3,1)))
    expect_that(coxed::bca(as.numeric(res4[1,]), conf.level = 0.95), equals(c(1/3,1)))
})


test_that ("fails nicely if x and y column names are not provided", {
    x<-cbind(rep(c(1,2),500), a=runif(1000,0,100), b=runif(1000,0,100))

    test <- function(a,b) {
        if (a[1] != 2) return(3)
        if (b[1] == 3) return(1)
        return(2)
    }

    expect_that(get.pi.permute(x,test,seq(10,50,10), seq(0,40,10),100),
                throws_error("unique x and y columns must be defined"))

})

