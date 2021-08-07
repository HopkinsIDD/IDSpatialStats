context("crossK and crossPCF wrapper functions")

test_that("Data checks performed", {
     
     msg <- "epi.data must be a numeric matrix"
     
     test.data <- matrix(rep(NA, 9), ncol=3)
     
     expect_that(get.cross.K(epi.data=test.data, type=3, hom=3, het=NULL, 
                             r=NULL, correction='border'), 
                 throws_error(msg))
     
     expect_that(get.cross.K(epi.data=as.data.frame(test.data), type=3, hom=3, het=NULL, 
                             r=NULL, correction='border'), 
                 throws_error(msg))
     
     expect_that(get.cross.PCF(epi.data=test.data, type=3, hom=3, het=NULL, 
                               r=NULL, correction='border'), 
                 throws_error(msg))
     
     expect_that(get.cross.PCF(epi.data=as.data.frame(test.data), type=3, hom=3, het=NULL, 
                               r=NULL, correction='border'), 
                 throws_error(msg))
     
     msg <- "all homotypic and heterotypic case types are not found in case type data"
     
     test.data <- matrix(rep(1, 9), ncol=3)
          
     expect_that(get.cross.K(epi.data=test.data, type=3, hom=c(1,3), het=NULL, 
                             r=NULL, correction='border'), 
                 throws_error(msg))
     
     expect_that(get.cross.PCF(epi.data=test.data, type=3, hom=c(1,3), het=NULL, 
                               r=NULL, correction='border'), 
                 throws_error(msg))
     
     msg <- 'type column defined is out of bounds'
     
     n <- 4
     test.data <- matrix(rnorm(n*3), ncol=3)
     test.data[,3] <- c(rep(1, n/2), rep(2, n/2))
     
     expect_that(get.cross.K(epi.data=test.data, type=10, hom=1, het=NULL, r=NULL),
                 throws_error(msg))
     
     expect_that(get.cross.PCF(epi.data=test.data, type=10, hom=1, het=NULL, r=NULL),
                 throws_error(msg))
     

})

test_that("Gives correct output for data", {
     
     n <- 4
     test.data <- matrix(rnorm(n*3), ncol=3)
     test.data[,3] <- c(rep(1, n/2), rep(2, n/2))
     
     expect_that(ncol(get.cross.K(epi.data=test.data, type=3, hom=1, het=2, r=NULL, correction=c('border', 'isotropic'))),
                 equals(4))
     
     expect_that(ncol(get.cross.K(epi.data=test.data, type=3, hom=1, het=2, r=NULL, correction='border')),
                 equals(3))
     
     expect_that(nrow(get.cross.K(epi.data=test.data, type=3, hom=1, het=2, r=seq(0, 0.5, length.out=10), correction='border')),
                 equals(10))
     
})