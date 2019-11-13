context("sim.epidemic")

test_that("Plausible parameter values produce simulations: R", {
     
     for(i in seq(1, 2, 0.25)) {
          sim <- sim.epidemic(R=i,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a)),
                              tot.generations=10,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              min.cases=2,
                              max.try=1000)
          
          expect_true(is.data.frame(sim) & is.numeric(sim[,1]) & ncol(sim) == 3) # Output is a three column numerical matrix
          expect_gte(nrow(sim), 2) # Number of cases in output > min.cases 
     }
})

test_that("Plausible parameter values produce simulations: tot.generations", {
     
     for(i in 5:10) {
          sim <- sim.epidemic(R=2,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a)),
                              tot.generations=i,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              min.cases=2,
                              max.try=1000)
          
          expect_true(is.data.frame(sim) & is.numeric(sim[,1]) & ncol(sim) == 3) # Output is a three column numerical matrix
          expect_gte(nrow(sim), 2) # Number of cases in output > min.cases 
     }
})

test_that("Plausible parameter values produce simulations: min.cases", {
     
     for(i in c(2, 10, 50)) {
          sim <- sim.epidemic(R=2,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a)),
                              tot.generations=10,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              min.cases=i,
                              max.try=1000)
          
          expect_true(is.data.frame(sim) & is.numeric(sim[,1]) & ncol(sim) == 3) # Output is a three column numerical matrix
          expect_gte(nrow(sim), 2) # Number of cases in output > min.cases 
     }
})

test_that("Plausible parameter values produce simulations: max.try", {
     
     for(i in c(10, 100, 1000, 10000)) {
          sim <- sim.epidemic(R=2,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a)),
                              tot.generations=10,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              min.cases=2,
                              max.try=i)
          
          expect_true(is.data.frame(sim) & is.numeric(sim[,1]) & ncol(sim) == 3) # Output is a three column numerical matrix
          expect_gte(nrow(sim), 2) # Number of cases in output > min.cases 
     }
})

test_that("Works with various trasmission kernel functions", {
     
     ls <- list(alist(n=1, a=1, b=1, rexp(n, a)), # Exponential kernel
                alist(n=1, a=1, b=1, runif(n, a, b)), # Uniform kernel
                alist(n=1, a=1, b=1, rgamma(n, a, b)), # Gamma kernel
                alist(n=1, a=1, b=1, rnorm(n, a, b)), # Gaussian kernel
                alist(n=1, a=1, b=1, rlnorm(n, a, b))) # Log-normal kernel
     
     for(i in 1:length(ls)) {
          sim <- sim.epidemic(R=2,
                              trans.kern.func=ls[[i]],
                              tot.generations=10,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              min.cases=2,
                              max.try=1000)
          
          expect_true(is.data.frame(sim) & is.numeric(sim[,1]) & ncol(sim) == 3) # Output is a three column numerical matrix
          expect_gte(nrow(sim), 2) # Number of cases in output > min.cases 
     }
})

test_that("Implausible simulations throw errors", {
     
     # Nonsense values
     msg <- "Chosen parameters did not produce epidemic."
     
     expect_that(sim.epidemic(R=0,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a))), 
                 throws_error(msg))
     
     msg <- "The specified parameters did not acheive an epidemic within the maximum number of tries."
     
     expect_that(sim.epidemic(R=1,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a)), 
                              max.try=0), 
                 throws_error(msg))
     
     # R too low
     expect_that(sim.epidemic(R=0.1,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a)),
                              tot.generations=10,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              min.cases=10000000,
                              max.try=100), 
                 throws_error(msg))
     
     # too few generations
     expect_that(sim.epidemic(R=2,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a)),
                              tot.generations=1,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              min.cases=10000,
                              max.try=1000), 
                 throws_error(msg))
     
     # max.try too low
     expect_that(sim.epidemic(R=2,
                              trans.kern.func=alist(n=1, a=100, rexp(n, a)),
                              tot.generations=10,
                              gen.t.mean=14, 			
                              gen.t.sd=3,
                              min.cases=10000000,
                              max.try=1), 
                 throws_error(msg))
})