context("sim_epi")

test_that("Plausible parameter values produce simulations: R", {
     
     for(i in seq(1, 2, 0.25)) {
          sim <- sim_epi(R=i,
                         kern_func=alist(n=1, a=100, rexp(n, a)),
                         tot_gen=10,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         min_cases=2,
                         max_try=1000)
          
          expect_true(is.data.frame(sim) & is.numeric(sim$x) & is.numeric(sim$y) & is.numeric(sim$t) & ncol(sim) == 3) # Output is a dataframe with three numerical columns
          expect_gte(length(sim), 2) # Number of cases in output > min_cases 
     }
})

test_that("Plausible parameter values produce simulations: tot_gen", {
     
     for(i in 5:10) {
          sim <- sim_epi(R=2,
                         kern_func=alist(n=1, a=100, rexp(n, a)),
                         tot_gen=i,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         min_cases=2,
                         max_try=1000)
          
          expect_true(is.data.frame(sim) & is.numeric(sim$x) & is.numeric(sim$y) & is.numeric(sim$t) & ncol(sim) == 3) # Output is a dataframe with three numerical columns
          expect_gte(nrow(sim), 2) # Number of cases in output > min_cases 
     }
})

test_that("Plausible parameter values produce simulations: min_cases", {
     
     for(i in c(2, 10, 50)) {
          sim <- sim_epi(R=2,
                         kern_func=alist(n=1, a=100, rexp(n, a)),
                         tot_gen=10,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         min_cases=i,
                         max_try=1000)
          
          expect_true(is.data.frame(sim) & is.numeric(sim$x) & is.numeric(sim$y) & is.numeric(sim$t) & ncol(sim) == 3) # Output is a dataframe with three numerical columns
          expect_gte(length(sim), 2) # Number of cases in output > min_cases 
     }
})

test_that("Plausible parameter values produce simulations: max_try", {
     
     for(i in c(10, 100, 1000, 10000)) {
          sim <- sim_epi(R=2,
                         kern_func=alist(n=1, a=100, rexp(n, a)),
                         tot_gen=10,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         min_cases=2,
                         max_try=i)
          
          expect_true(is.data.frame(sim) & is.numeric(sim$x) & is.numeric(sim$y) & is.numeric(sim$t) & ncol(sim) == 3) # Output is a dataframe with three numerical columns
          expect_gte(length(sim), 2) # Number of cases in output > min_cases 
     }
})

test_that("Works with various trasmission kernel functions", {
     
     ls <- list(alist(n=1, a=1, b=1, rexp(n, a)), # Exponential kernel
                alist(n=1, a=1, b=1, runif(n, a, b)), # Uniform kernel
                alist(n=1, a=1, b=1, rgamma(n, a, b)), # Gamma kernel
                alist(n=1, a=1, b=1, rnorm(n, a, b)), # Gaussian kernel
                alist(n=1, a=1, b=1, rlnorm(n, a, b))) # Log-normal kernel
     
     for(i in 1:length(ls)) {
          sim <- sim_epi(R=2,
                         kern_func=ls[[i]],
                         tot_gen=10,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         min_cases=2,
                         max_try=1000)
          
          expect_true(is.data.frame(sim) & is.numeric(sim$x) & is.numeric(sim$y) & is.numeric(sim$t) & ncol(sim) == 3) # Output is a dataframe with three numerical columns
          expect_gte(length(sim), 2) # Number of cases in output > min_cases 
     }
})

test_that("Implausible simulations throw errors", {
     
     # Nonsense values
     msg <- "Chosen parameters did not produce epidemic."
     
     expect_that(sim_epi(R=0,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         kern_func=alist(n=1, a=100, rexp(n, a))), 
                 throws_error(msg))
     
     msg <- "The specified parameters did not acheive an epidemic within the maximum number of tries."
     
     expect_that(sim_epi(R=1,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         kern_func=alist(n=1, a=100, rexp(n, a)), 
                         max_try=0), 
                 throws_error(msg))
     
     # R too low
     expect_that(sim_epi(R=0.1,
                         kern_func=alist(n=1, a=100, rexp(n, a)),
                         tot_gen=10,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         min_cases=10000000,
                         max_try=100), 
                 throws_error(msg))
     
     # too few generations
     expect_that(sim_epi(R=2,
                         kern_func=alist(n=1, a=100, rexp(n, a)),
                         tot_gen=1,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         min_cases=10000,
                         max_try=1000), 
                 throws_error(msg))
     
     # max_try too low
     expect_that(sim_epi(R=2,
                         kern_func=alist(n=1, a=100, rexp(n, a)),
                         tot_gen=10,
                         gen_t_mean=14, 			
                         gen_t_sd=3,
                         min_cases=10000000,
                         max_try=1), 
                 throws_error(msg))
})