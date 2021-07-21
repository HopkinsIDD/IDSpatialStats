test_that("get.tau.permute returns appropriate values for test case 1 (equilateral triangle)" ,{

    x <- rbind(c(1,0,0), c(1,1,0),c(2,.5,sqrt(.75)))
    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }


    ###REPRESENTATIVE
    #should return 1 for every permutation
    res <- get.tau.permute(x, test, 1.5, 0, 500)[,-(1:2)]
    res2 <- get.tau.typed.permute(x, 1, 2, 1.5, 0, 500)[,-(1:2)]


    expect_that(as.numeric(res), equals(rep(1,500)))
    expect_that(as.numeric(res2), equals(rep(1,500)))

    ###INDEPENDENT
    #should return 1 for every permutation
    res <- get.tau.permute(x, test, 1.5, 0, 500,
                           comparison.type="independent")[,-(1:2)]
    res2 <- get.tau.typed.permute(x, 1, 2, 1.5, 0, 500,
                                  comparison.type="independent")[,-(1:2)]


    expect_that(as.numeric(res), equals(rep(1,500)))
    expect_that(as.numeric(res2), equals(rep(1,500)))

})


test_that("get.tau.permute returns appropriate values for test case 2 (points on a line)" ,{
    x<-rbind(c(1,0,0), c(2,1,0), c(2,-1,0), c(3,2,0),
             c(2,-2,0), c(3,3,0),c(3,-3,0))

    colnames(x) <-c("type","x","y")

    test <- function(a,b) {
        if (a[1] != 1) return(3)
        if (b[1] == 2) return(1)
        return(2)
    }

    ####REPRESENTATIVE
    #the mean of the null distribution should be 1
    #the 95% CI equals 0,2 with windows
    res <- get.tau.permute(x, test, c(1.5,2.5,3.5), c(0,1.5,2.5), 500)[,-(1:2)]
    res2 <- get.tau.typed.permute(x, 1, 2, c(1.5,2.5,3.5), c(0,1.5,2.5), 500)[,-(1:2)]

    expect_that(rowMeans(res, na.rm=T), equals(rep(1,3), tolerance=0.1))
    expect_that(rowMeans(res2, na.rm=T), equals(rep(1,3), tolerance=0.1))

    for (i in 1:3) {
        expect_that(as.numeric(quantile(res[i,], probs=c(.025,.975))),
                    equals(c(0,2)))
        expect_that(as.numeric(quantile(res2[i,], probs=c(.025,.975))),
                    equals(c(0,2)))
    }

    ####INDEPENDENT
    #the mean of the null distribution should be 1
    #the 95% CI equals 0,Inf with windows
    res <- get.tau.permute(x, test, c(1.5,2.5,3.5), c(0,1.5,2.5), 500,
                           comparison.type="independent")[,-(1:2)]
    res2 <- get.tau.typed.permute(x, 1, 2, c(1.5,2.5,3.5), c(0,1.5,2.5), 500,
                                  comparison.type="independent")[,-(1:2)]


    for (i in 1:3) {
        expect_that(as.numeric(quantile(res[i,], probs=c(.025,.5,.975))),
                    equals(c(0,1,Inf)))
        expect_that(as.numeric(quantile(res2[i,], probs=c(.025,.5,.975))),
                    equals(c(0,1,Inf)))
    }

})



test_that ("fails nicely if x and y column names are not provided", {
    x<-cbind(rep(c(1,2),500), a=runif(1000,0,100), b=runif(1000,0,100))

    test <- function(a,b) {
        if (a[1] != 2) return(3)
        if (b[1] == 3) return(1)
        return(2)
    }

    expect_that(get.tau.permute(x,test,seq(10,50,10), seq(0,40,10),100),
                throws_error("unique x and y columns must be defined"))

})

