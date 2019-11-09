context("theta weights")

test_that("Correct array for theta values is returned", {
     
     case_times <- c(1,2,2,3,3)
     gen <- c(0, 2/3, 1/3, 0, 0)
     t_density <- gen/sum(gen)
     gen_time <- 2
     wt_mat <- wt(case_times=case_times, gen_t_dist=t_density)
     n_gen <- round((max(case_times) - min(case_times)) / gen_time) + 1 # Number of generations
     
     # Get matrix of theta values calculated for each case pair
     a <- transdist_theta(wt_matrix=wt_mat,
                          cases=case_times,
                          gen_t_mean=gen_time,
                          max_sep=n_gen*2,
                          theta_matrix=T) 
     
     # Check that theta matrix is symetrical with appropriate dimensions and contains integers
     expect_true(is.matrix(a) & ncol(a) == length(case_times) & nrow(a) == length(case_times))
     expect_true(is.integer(a))
     expect_true(sum(is.na(diag(a))) == length(case_times))
     
})

test_that("Correct array for theta weights is returned", {
     
     case_times <- c(1,2,2,3,3)
     gen <- c(0, 2/3, 1/3, 0, 0)
     t_density <- gen/sum(gen)
     gen_time <- 2
     wt_mat <- wt(case_times=case_times, gen_t_dist=t_density)
     n_gen <- round((max(case_times) - min(case_times)) / gen_time) + 1 # Number of generations
     
     # Get matrix of theta values calculated for each case pair
     a <- transdist_theta(wt_matrix=wt_mat,
                          cases=case_times,
                          gen_t_mean=gen_time,
                          max_sep=n_gen*2) 
     
     # Check that theta weights are given in array with appropriate dimensions
     expect_true(is.array(a))
     expect_true(ncol(a) == max(case_times) & nrow(a) == max(case_times))
     expect_true(dim(a)[3] == n_gen*2)
     
})

test_that("Testing border conditions: all cases in first time step", {
     
     case_times <- rep(1, 5)
     gen <- c(0, 2/3, 1/3, 0, 0)
     t_density <- gen/sum(gen)
     gen_time <- 2
     wt_mat <- wt(case_times=case_times, gen_t_dist=t_density)
     n_gen <- round((max(case_times) - min(case_times)) / gen_time) + 1 # Number of generations
     
     # Get matrix of theta values calculated for each case pair
     a <- transdist_theta(wt_matrix=wt_mat,
                          cases=case_times,
                          gen_t_mean=gen_time,
                          max_sep=n_gen) # maximum seperation = number generations
     
     # Check that array has dimensions c(1,1,1) and value = NA
     expect_true(is.array(a))
     tmp <- length(unique((case_times)))
     expect_true(ncol(a) == tmp & nrow(a) == tmp)
     expect_true(dim(a)[3] == n_gen)
     expect_true(is.na(a))
     
})

test_that("Testing border conditions: all cases in last time step", { 
     
     # Expect same if all cases in last time step
     case_times <- rep(5, 5)
     gen <- c(0, 2/3, 1/3, 0, 0)
     t_density <- gen/sum(gen)
     gen_time <- 2
     wt_mat <- wt(case_times=case_times, gen_t_dist=t_density)
     n_gen <- round((max(case_times) - min(case_times)) / gen_time) + 1 # Number of generations
     
     # Get matrix of theta values calculated for each case pair
     a <- transdist_theta(wt_matrix=wt_mat,
                          cases=case_times,
                          gen_t_mean=gen_time,
                          max_sep=n_gen) # maximum seperation = number generations
     
     # Check that array has dimensions c(1,1,1) and value = NA
     expect_true(is.array(a))
     tmp <- length(unique((case_times)))
     expect_true(ncol(a) == tmp & nrow(a) == tmp)
     expect_true(dim(a)[3] == n_gen)
     expect_true(is.na(a))
     
})





