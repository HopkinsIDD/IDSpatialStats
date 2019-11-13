context("theta weights")

test_that("Correct array for theta values is returned", {
     
     case.times <- c(1,2,2,3,3)
     gen <- c(0, 2/3, 1/3, 0, 0)
     t.density <- gen/sum(gen)
     gen.time <- 2
     wt <- est.wt.matrix(case.times=case.times, gen.t.dist=t.density)
     ngen <- round((max(case.times) - min(case.times)) / gen.time) + 1 # Number of generations
     
     # Get matrix of theta values calculated for each case pair
     a <- get.transdist.theta(wal.teun.mat=wt,
                              cases=case.times,
                              gen.t.mean=gen.time,
                              max.sep=ngen*2,
                              ret.theta.mat=T) 
     
     # Check that theta matrix is symetrical with appropriate dimensions and contains integers
     expect_true(is.matrix(a) & ncol(a) == length(case.times) & nrow(a) == length(case.times))
     expect_true(is.integer(a))
     expect_true(sum(is.na(diag(a))) == length(case.times))
     
})

test_that("Correct array for theta weights is returned", {
     
     case.times <- c(1,2,2,3,3)
     gen <- c(0, 2/3, 1/3, 0, 0)
     t.density <- gen/sum(gen)
     gen.time <- 2
     wt <- est.wt.matrix(case.times=case.times, gen.t.dist=t.density)
     ngen <- round((max(case.times) - min(case.times)) / gen.time) + 1 # Number of generations
     
     # Get matrix of theta values calculated for each case pair
     a <- get.transdist.theta(wal.teun.mat=wt,
                              cases=case.times,
                              gen.t.mean=gen.time,
                              max.sep=ngen*2) 
     
     # Check that theta weights are given in array with appropriate dimensions
     expect_true(is.array(a))
     expect_true(ncol(a) == max(case.times) & nrow(a) == max(case.times))
     expect_true(dim(a)[3] == ngen*2)
     
})

test_that("Testing border conditions: all cases in first time step", {
     
     case.times <- rep(1, 5)
     gen <- c(0, 2/3, 1/3, 0, 0)
     t.density <- gen/sum(gen)
     gen.time <- 2
     wt <- est.wt.matrix(case.times=case.times, gen.t.dist=t.density)
     ngen <- round((max(case.times) - min(case.times)) / gen.time) + 1 # Number of generations
     
     # Get matrix of theta values calculated for each case pair
     a <- get.transdist.theta(wal.teun.mat=wt,
                              cases=case.times,
                              gen.t.mean=gen.time,
                              max.sep=ngen) # maximum seperation = number generations
     
     # Check that array has dimensions c(1,1,1) and value = NA
     expect_true(is.array(a))
     tmp <- length(unique((case.times)))
     expect_true(ncol(a) == tmp & nrow(a) == tmp)
     expect_true(dim(a)[3] == ngen)
     expect_true(is.na(a))
     
})

test_that("Testing border conditions: all cases in last time step", { 
     
     # Expect same if all cases in last time step
     case.times <- rep(5, 5)
     gen <- c(0, 2/3, 1/3, 0, 0)
     t.density <- gen/sum(gen)
     gen.time <- 2
     wt <- est.wt.matrix(case.times=case.times, gen.t.dist=t.density)
     ngen <- round((max(case.times) - min(case.times)) / gen.time) + 1 # Number of generations
     
     # Get matrix of theta values calculated for each case pair
     a <- get.transdist.theta(wal.teun.mat=wt,
                              cases=case.times,
                              gen.t.mean=gen.time,
                              max.sep=ngen) # maximum seperation = number generations
     
     # Check that array has dimensions c(1,1,1) and value = NA
     expect_true(is.array(a))
     tmp <- length(unique((case.times)))
     expect_true(ncol(a) == tmp & nrow(a) == tmp)
     expect_true(dim(a)[3] == ngen)
     expect_true(is.na(a))
     
})