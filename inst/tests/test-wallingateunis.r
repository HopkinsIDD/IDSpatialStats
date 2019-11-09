context("Wallinga-Teunis")

test_that("Basic case time WT weights are calculated correctly", {
     
     # Toy example from paper
     # Assume 5 cases occur over 3 days (Fig 2A of Salje et al 2016)
     case_times <- c(1,2,2,3,3) 
     times <- c(0, seq(min(case_times), max(case_times) + 1, 1))       # sequence of case times from zero to max case time
     epi <- hist(case_times, breaks=times, plot=F, right=T)$counts     # count of cases in full time sequence from zero to max case time
     unique.times <- unique(case_times)	 	                          # Unique time points
     ntimes <- length(unique.times) 			                     # Number of unique time points
     
     # Assume the generation time is known, where 2/3 of sequential infections occur one day apart and 1/3 occur two days apart
     gen <- c(0, 2/3, 1/3, 0, 0)
     t.density <- gen/sum(gen)
     
     # Basic weights
     wt.weights <- wt_weights(case_times=case_times, gen_t_dist=t.density)
     
     # expected weights
     expected.wt.weights <- matrix(c(0, 1, 0.2, 0,
                                     0, 0, 0.8, 0,
                                     0, 0, 0, 0,
                                     0, 0, 0, 0), 
                                   nrow=ntimes+1, 
                                   ncol=ntimes+1, 
                                   byrow=T)
     
     # Output is numerical matrix with expected dimesion
     check.mat <- is.matrix(wt.weights) & is.numeric(wt.weights) & ncol(wt.weights) == ntimes+1 & nrow(wt.weights) == ntimes+1
     expect_true(check.mat)
     
     # Calculated values are identical to known values
     expect_identical(wt.weights, expected.wt.weights) 
})

test_that("Infector-Infectee WT matrix is calculated correctly", {
     
     case_times <- c(1,2,2,3,3) 
     times <- c(0, seq(min(case_times), max(case_times) + 1, 1))       # sequence of case times from zero to max case time
     epi <- hist(case_times, breaks=times, plot=F, right=T)$counts     # count of cases in full time sequence from zero to max case time
     unique.times <- unique(case_times)	 	                          # Unique time points
     ntimes <- length(unique.times) 			                     # Number of unique time points
     
     gen <- c(0, 2/3, 1/3, 0, 0)
     t.density <- gen/sum(gen)
     
     # Basic weights
     wt.weights <- wt_weights(case_times=case_times, gen_t_dist=t.density)
     
     # Infector-Tnfectee WT matrix
     wt.vals <- wt(case_times=case_times, gen_t_dist=t.density, basic_wt_weights=wt.weights)
     
     # expected values
     expected.wt.vals <- matrix(c(0, 1, 1, 0.2, 0.2,
                                  0, 0, 0, 0.4, 0.4,
                                  0, 0, 0, 0.4, 0.4,
                                  0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0), 
                                nrow=length(case_times), 
                                ncol=length(case_times), 
                                byrow=T)
     
     # Output is numerical matrix with expected dimesion
     check.mat <- is.matrix(wt.vals) & is.numeric(wt.vals) & ncol(wt.vals) == length(case_times) & nrow(wt.vals) == length(case_times)
     expect_true(check.mat)
     
     # Calculated values are identical to known values
     expect_identical(wt.vals, expected.wt.vals) 
})

test_that("Testing border conditions: all cases in first time step", {
     
     case_times <- c(1,1,1,1,1) 
     times <- c(0, seq(min(case_times), max(case_times) + 1, 1))       # sequence of case times from zero to max case time
     epi <- hist(case_times, breaks=times, plot=F, right=T)$counts     # count of cases in full time sequence from zero to max case time
     unique.times <- unique(case_times)	 	                          # Unique time points
     ntimes <- length(unique.times) 			                     # Number of unique time points
     
     gen <- c(0, 2/3, 1/3, 0, 0) # Does not matter what gen time is in this example because there is no instantaneous infection
     t.density <- gen/sum(gen)
     
     # Basic weights
     wt.weights <- wt_weights(case_times=case_times, gen_t_dist=t.density)
     
     # expected basic WT weights
     expected.wt.weights <- matrix(c(0, 0,
                                     0, 0), 
                                   nrow=ntimes+1, 
                                   ncol=ntimes+1, 
                                   byrow=T)
     
     # Output of weight matrix is numerical with expected dimesion
     check.mat <- is.matrix(wt.weights) & is.numeric(wt.weights) & ncol(wt.weights) == ntimes+1 & nrow(wt.weights) == ntimes+1
     expect_true(check.mat)
     
     # Calculated values are identical to known values
     expect_identical(wt.weights, expected.wt.weights)
     
     # Infector-Infectee WT matrix
     wt.vals <- wt(case_times=case_times, 
                              gen_t_dist=t.density, 
                              basic_wt_weights=wt.weights)
     
     expected.wt.vals <- matrix(rep(0, length(case_times)^2),
                                nrow=length(case_times),
                                ncol=length(case_times),
                                byrow=T)
     
     # Calculated values should be all zero
     expect_identical(wt.vals, expected.wt.vals)
})

test_that("Testing border conditions: all cases in last time step", {
     
     case_times <- c(4,4,4,4,4) 
     times <- c(0, seq(min(case_times), max(case_times) + 1, 1))       # sequence of case times from zero to max case time
     epi <- hist(case_times, breaks=times, plot=F, right=T)$counts     # count of cases in full time sequence from zero to max case time
     unique.times <- unique(case_times)	 	                          # Unique time points
     ntimes <- length(unique.times) 			                     # Number of unique time points
     
     gen <- c(0, 2/3, 1/3, 0, 0) # Does not matter what gen time is in this example because there is no instantaneous infection
     t.density <- gen/sum(gen)
     
     # Basic weights
     wt.weights <- wt_weights(case_times=case_times, gen_t_dist=t.density)
     

     
     # expected basic WT weights
     expected.wt.weights <- matrix(c(0, 0,
                                     0, 0), 
                                   nrow=ntimes+1, 
                                   ncol=ntimes+1, 
                                   byrow=T)
     
     # Output of weight matrix is numerical with expected dimesion
     check.mat <- is.matrix(wt.weights) & is.numeric(wt.weights) & ncol(wt.weights) == ntimes+1 & nrow(wt.weights) == ntimes+1
     expect_true(check.mat)
     
     # Calculated values are identical to known values
     expect_identical(wt.weights, expected.wt.weights)
     
     # Infector-Infectee WT matrix
     wt.vals <- wt(case_times=case_times, 
                              gen_t_dist=t.density, 
                              basic_wt_weights=wt.weights)
     
     # expected values of infector-infectee matrix
     ind <- length(case_times)
     expected.wt.vals <- matrix(rep(0, ind^2), 
                                nrow=ind, 
                                ncol=ind, 
                                byrow=T)
     
     # Output is numerical matrix with expected dimesion
     check.mat <- is.matrix(wt.vals) & is.numeric(wt.vals) & ncol(wt.vals) == length(case_times) & nrow(wt.vals) == length(case_times)
     expect_true(check.mat)
     
     # Calculated values are identical to known values
     expect_identical(wt.vals, expected.wt.vals) 
})


