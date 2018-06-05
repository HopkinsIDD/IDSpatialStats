##' Simulation of an epidemic in space and time
##'
##' A function which simulates the spatial spread of infections through time given the reproductive number (\code{R}), 
##' a function describing the spatial transmission kernel (\code{trans.kern.func}), and the mean and standard deviation 
##' of the generation time distribution (\code{gen.t.mean} and \code{gen.t.sd}) for the infecting pathogen. The function returns 
##' the location (\code{x}, \code{y}) and time (\code{t}) for each case of infection in the simulation.
##'
##' @param R a scalar or a vector of length \code{tot.generations} providing the reproductive number for the epidemic. 
##' If scalar, the R value is constant. If a vector, the R value varies according to each generation in the vector.
##' @param gen.t.mean mean of generation time
##' @param gen.t.sd standard deviation of the generation time (assumed to be normally distributed)
##' @param trans.kern.func a function for the transmission kernel that takes \code{n} as an arguement. 
##' Function and associated parameters must be given in a list object.
##' @param tot.generations the total number of generations in the epidemic, where the index case (x,y,t = [0,0,0]) is considered generation zero (default = 10)
##' @param min.cases the minimum number of cases in the epidemic (default = 0)
##' @param max.try maximum number of tries to acheive the minimum number of cases (default = 1000)
##' 
##' @return a numerical matrix with three columns giving the coordinates \code{x} and \code{y}, and time \code{t} of simulated cases
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##'
##' @example R/examples/sim_epidemic.R
##'

sim.epidemic <- function(
     R, 					                 # Reproductive number (scalar or vector)
     gen.t.mean, 			             # Mean generation time
     gen.t.sd, 				             # Standard deviation of generation time (assumes normally distributed)
     trans.kern.func,              # Function for transmission kernel that takes n as argument
     tot.generations=10,           # Number of generations (considers index case x,y,t = [0,0,0] as generation zero)
     min.cases=0,                  # Minumum number of cases in epidemic
     max.try=1000                  # Maximum number of tries to acheive minimum number of cases
){
  
  if(length(R) > 1 && length(R) != tot.generations) stop('R must be a scalar or a vector with length equal to tot.generations')  
  if(length(R) > 1 && length(R) == tot.generations) message('Simulating epidemic with variable R value') 
  if(length(R) == 1) {
    message('Simulating epidemic with constant R value')
    R <- rep(R, tot.generations)
  }
    
     dist.func <- as.function(trans.kern.func)
     
     stay <- TRUE
     n.try <- 0
     while(stay){ # ensure simulation returns a user-specified minimum number of cases or stop at max number of tries
          
          output <- vector("list", tot.generations)
          output[[1]] <- cbind(0, 0, 0) # x, y, and t of index case
          
          for (i in 2:(tot.generations + 1)){
               n.per.gen <- rpois(nrow(output[[i-1]]), R[i-1]) # number of new infections caused by each individual in previous generation
               n <- sum(n.per.gen) # total number of new infections in current generation
               old.x <- rep(output[[i-1]][,1], times=n.per.gen) # Get correpsonding x and y of parent points for each new case
               old.y <- rep(output[[i-1]][,2], times=n.per.gen)
               
               dist <- dist.func(n=n) # Get distances from parent for new cases	
               angle <- runif(n, 0, 2*pi) # Get direction from parent for new cases
               x <- dist*cos(angle) + old.x # Make x and y coordinates for new cases
               y <- dist*sin(angle) + old.y
               t <- rep(output[[i-1]][,3], times=n.per.gen) + rnorm(n, gen.t.mean, gen.t.sd) # Assign times based on mean and sd of gen time
               
               output[[i]] <- cbind(x, y, t)
          }
          
          out <- as.data.frame(do.call("rbind", output))
          
          n.try <- n.try + 1 # Number of times simulation has tried to produce an epidemic
          stay <- nrow(out) <= min.cases & n.try <= max.try # If minimum cases and maximum tries have not been reached, stay in loop
          if(n.try >= max.try) stop("The specified parameters did not acheive an epidemic within the maximum number of tries.")
     }
     
     max.time <- gen.t.mean * (tot.generations - 1) # End of epidemic
     out <- out[which(out[,3] <= max.time),]
     
     if(nrow(out) < min.cases | nrow(out) == 1) stop("Chosen parameters did not produce epidemic.")
     
     out[,3] <- floor(out[,3]) # Round case times to integer of time step
     out <- as.matrix(out)
     row.names(out) <- NULL
     return(out)
}

##' Plot output of simulated epidemic
##'
##' A simple visualization function which plots the location of the index case and the spatial distribution of subsequent cases, 
##' and the epidemic curve showing the case count over time.
##'
##' @param sim a three-column matrix object produced by the \code{sim.epidemic} function
##' 
##' @return A two-panel plotted object
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 

sim.plot <- function(sim) {
  par(mfrow=c(1,2))
  plot(sim[,1], sim[,2], xlab="x", ylab="y")
  points(sim[1,1], sim[1,2], col='red', pch=3, cex=2)
  hist(sim[,3], breaks=max(sim[,3]), xlab="Time", ylab="Case count", col='lightblue', main="")
}

##' Calculate the Infector-Infectee Wallinga-Teunis matrix
##'
##' A function which takes the time of each case occurrence, the generation time distribution of the infecting pathogen, 
##' and the matrix of basic Wallinga-Teunis weights and estimates the probability that an infectee occurring time step j (columns) 
##' was infected by a case occurring at time i (rows).
##'
##' @param case.times a vector giving the occurrence time for each case
##' @param gen.t.dist a vector giving the generation time distribution for the infecting pathogen
##' @param basic.wt.weights a matrix giving the basic normalized Wallinga-Teunis weights for each time step (output from the \code{est.wt.matrix.weights} function). 
##' If this argument is \code{NULL} (the default), the basic Wallinga-Teunis matrix will be calculated automatically.
##' 
##' @return a numerical matrix with the number of columns and rows equal to the number of cases in the epidemic
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##'
##' @references 
##' Salje H, Cummings DAT and Lessler J (2016). “Estimating infectious disease transmission distances using the overall distribution of cases.” Epidemics, 17, pp. 10–18. ISSN 1755-4365, doi: \href{https://www.sciencedirect.com/science/article/pii/S1755436516300317}{10.1016/j.epidem.2016.10.001}.
##' 
##' @family est.wt
##'
##' @example R/examples/est_wt_matrix.R
##'

est.wt.matrix <- function(
     case.times,                   # a vector of case times (the time step in which each case occurs)
     gen.t.dist,                   # the generation time distribution
     basic.wt.weights=NULL         # A matrix produced by the 'est.wt.matrix.weights' function
){
     
     times <- c(0, seq(min(case.times), max(case.times) + 1, 1))
     incidence <- hist(case.times, breaks=times, plot=FALSE, right=TRUE)$counts

     # Calculate the Wallinga-Teunis matrix giving the probability of transmission from an infector at time step i to infectee at time step j
     if(is.null(basic.wt.weights)){
          basic.wt.weights <- est.wt.matrix.weights(case.times=case.times, gen.t.dist=gen.t.dist)
     }
     
     keep <- which(incidence > 0) # keep only time steps which have at least one case
     mat <- as.matrix(basic.wt.weights[keep, keep])
     
     a <- sweep(mat, 1, incidence[keep], "/") # Divide by number of cases (we then multiply up by this)
     b <- apply(a, 2, function(x) rep(x, incidence[keep])) # Repeat probabilities by number of cases
     
     wal.teun.mat <- t(apply(b, 1, function(x) rep(x, incidence[keep])))
     
     return(wal.teun.mat)
     
}

##' Estimate matrix of basic Wallinga-Teunis weights
##'
##' A function called by \code{est.wt.matrix}, which calculates the basic weights in the Wallinga-Teunis matrix given 
##' the time of each case occurrence and the generation time distribution of the pathogen. Code adapted from the R0 package.
##'
##' @param case.times a vector giving the occurrence time for each case
##' @param gen.t.dist a vector giving the generation time distribution for the infecting pathogen
##' 
##' @return a numerical matrix with the number of columns and rows equal to the number of time steps in the epidemic
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references
##' Boelle P and Obadia T (2015). R0: Estimation of R0 and Real-Time Reproduction Number from Epidemics. R package version 1.2-6, \href{https://CRAN.R-project.org/package=R0}{https://CRAN.R-project.org/package=R0}.
##' 
##' Salje H, Cummings DAT and Lessler J (2016). “Estimating infectious disease transmission distances using the overall distribution of cases.” Epidemics, 17, pp. 10–18. ISSN 1755-4365, doi: \href{https://www.sciencedirect.com/science/article/pii/S1755436516300317}{10.1016/j.epidem.2016.10.001}.
##' 
##' @family est.wt
##'
##' @example R/examples/est_wt_matrix_weights.R
##'

est.wt.matrix.weights <- function(
     case.times,                # a vector of case times (the time step in which each case occurs)
     gen.t.dist                 # the generation time distribution
){
     
     times <- c(0, seq(min(case.times), max(case.times) + 1, 1))
     incidence <- as.numeric(hist(case.times, breaks=times, plot=FALSE, right=TRUE)$counts)
     t.max <- length(incidence) # maximum time step where a case occurs
     
     # if number of time steps exceeds the length of the generation time distribution, pad  the distribution with zeros
     gt.pad <- gen.t.dist 
     
     if (length(gt.pad) < t.max) { 
          gt.pad <- c(gt.pad, rep(0, t.max - length(gt.pad)))
     }
     
     # Calculate weight in Wallinga-Teunis matrix
     p <- matrix(0, ncol=t.max, nrow=t.max)
     
     for (s in 2:t.max) {
          if ((incidence[s] > 0)) {
               weights <- (incidence[1:s] - c(rep(0, s - 1), 1)) * gt.pad[s:1] # calculate relative weight of each time step
               weights <- weights/sum(weights) # normalize weights by sum
               p[1:s, s] <- weights
          }
          else {
               p[1:s, s] <- 0
          }
     }
     
     return(p)
}

##' Get weights of transmission distance theta
##'
##' This function estimates the weights of each theta (number of transmission events separating cases at two time points). A randomized transmission tree is drawn and the number of transmission events 
##' separating cases at two time points is calculated based on probabilies found in the Wallinga-Teunis matrix.
##'
##' @param wal.teun.mat a Wallinga-Teunis matrix produced by the \code{est.wt.matrix} function
##' @param cases a vector of case times for each case
##' @param gen.t.mean the mean generation time of the infecting pathogen
##' @param max.sep maximum number of transmission events allowed between two cases
##' @param ret.theta.mat logical value which returns the matrix of estimated theta values (default = FALSE) 
##' 
##' @return a three-dimensional array containing normalized theta weights. Columns and rows represent unique case times. The third dimension is the number of transmission events between two cases.
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references 
##' Salje H, Cummings DAT and Lessler J (2016). “Estimating infectious disease transmission distances using the overall distribution of cases.” Epidemics, 17, pp. 10–18. ISSN 1755-4365, doi: \href{https://www.sciencedirect.com/science/article/pii/S1755436516300317}{10.1016/j.epidem.2016.10.001}.
##' 
##' @family transdist
##'
##' @example R/examples/get_transdist_theta.R
##'
##'

# 'get.theta' is already taken
get.transdist.theta <-function(wal.teun.mat,
                               cases,
                               gen.t.mean,
                               max.sep,
                               ret.theta.mat=FALSE
){
  
  n <- length(cases) # total number of cases
  unique.times <- unique(cases) # the time steps in which cases occur
  ntimes <- length(unique.times) # total number of time steps in which cases occur
  
  # Create random sample of infectee cases 
  samp.inds <- c(1, sample(2:n, n - 1, replace=TRUE)) # ensure first time step included
  samp.times <- cases[samp.inds] # time steps of sampled cases
  
  # Link Infectees to an Infector based on Infector-Infectee Wallinga-Teunis matrix
  func <- function(x){
    if(sum(x[samp.inds]) == 0) return(NA)
    a <- sample(n, 1, prob=x[samp.inds]/sum(x[samp.inds]))
    return(a)
  }
  
  tree <- apply(wal.teun.mat[,samp.inds], 2, "func") # get columns of sampled cases only
  nodes <- cbind(tree, 1:n)
  nodes <- nodes[!is.na(nodes[,1]),] # infectee cases in right column, probable infector cases in left (cases not case times)
  
  # distance between infector and infectee in terms of generation time
  sep <- (samp.times[nodes[,2]] - samp.times[nodes[,1]]) / gen.t.mean
  #sep <- round(samp.times[nodes[,2]] - samp.times[nodes[,1]]) / gen.t.mean
  sep[which(sep == 0)] <- 1 # Don't have infections at time 0
  
  # Graph connecting infectors and infectees
  df <- data.frame(nodes[,2], nodes[,1], sep=sep) 
  g <- graph.data.frame(df, directed=FALSE) # expressed as case individuals NOT case times
  theta.mat <- shortest.paths(g, weights=E(g)$sep) # raw number of transmission events (theta) separating two cases
  
  # Clean matrix containing theta estimates
  a <- order(as.numeric(rownames(theta.mat))) # reorder by cases
  suppressWarnings(theta.mat <- matrix(as.integer(floor(theta.mat[a,a])), n, n))
  diag(theta.mat) <- NA
  
  # Return theta matrix for unit testing
  if(ret.theta.mat==TRUE) {return(theta.mat)}
  
  # Calculate weights for each theta
  weight.mat <- array(NaN, c(ntimes, ntimes, max.sep))
  for (i in 1:ntimes){
    
    a <- which(samp.times == unique.times[i])
    
    for (j in 1:i){
      b <- which(samp.times == unique.times[j])
      c <- theta.mat[a, b] # Get the sampled theta values which occur in time steps i to j
      
      # Count number of transmission events that are separated by theta events
      d <- as.vector(table(cut(c, breaks=c(0:max.sep, max.sep+1e10))))
      d <- d[-length(d)] # drop large bin added to the end (to catch outliers)
      weight.mat[i,j,] <- d/sum(d) # Normalize to get weights of each possible theta
    }
  }
  return(weight.mat)
}

##' Estimate transmission distance theta values by replication 
##'
##' This function estimates the weight of each theta value by performing a user defined number of replications with the \code{get.transdist.theta} function. The weights
##' of each theta are calculated as the number of simulations in which a case at time \code{t1} and \code{t2} are separated by theta transmission events.
##'
##' @param case.times a vector giving the occurrence time for each case
##' @param gen.t.mean the mean generation time of the infecting pathogen
##' @param t.density a vector giving the generation time density of the infecting pathogen
##' @param t1 time step to begin simulation
##' @param n.rep number of replications in the simulation (default = 100)
##' 
##' @return a three-dimensional array containing the mean normalized theta weights estimated across all replications
##' 
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references 
##' Salje H, Cummings DAT and Lessler J (2016). “Estimating infectious disease transmission distances using the overall distribution of cases.” Epidemics, 17, pp. 10–18. ISSN 1755-4365, doi: \href{https://www.sciencedirect.com/science/article/pii/S1755436516300317}{10.1016/j.epidem.2016.10.001}.
##' 
##' @family transdist
##'
##' @example R/examples/est_transdist_theta_weights.R
##'

est.transdist.theta.weights <- function(case.times,
                                        gen.t.mean,
                                        t.density,
                                        t1,
                                        n.rep=100
){
  
  first.case.detected <- if (t1 %in% case.times)(1) else(0)
  
  if(first.case.detected == 0){case.times <- c(t1, case.times)}
  
  if (sum(case.times == min(case.times)) > 1){
    case.times <- c(min(case.times), case.times[-which(case.times == min(case.times))])
  }
  
  if(min(case.times) <= 0){case.times <- case.times + (-min(case.times) + 1)}     # No negative times
  
  ngen <- round((max(case.times) - min(case.times)) / gen.t.mean) + 1             # Number of generations
  unique.times <- unique(case.times)	 	                                          # Unique time points
  ntimes <- length(unique.times) 		                                              # Number of unique time points
  
  wal.teun <- est.wt.matrix(case.times=case.times, gen.t.dist=t.density)
  
  wt.arr <- array(NaN, c(ntimes, ntimes, ngen*2, n.rep))
  for (i in 1:n.rep) {
    
    wt.arr[,,,i] <- get.transdist.theta(wal.teun.mat=wal.teun, 
                                        cases=case.times, 
                                        max.sep=ngen*2, 
                                        gen.t.mean=gen.t.mean)
  }
  
  wt <- apply(wt.arr, c(1,2,3), mean, na.rm=TRUE)
  
  if(first.case.detected == FALSE){wt <- wt[-1,-1,]}
  
  return(wt)
}

##' Estimate transmission distance
##'
##' this function estimates the mean transmission distance of an epidemic when given the locations and times of symptomatic cases and the mean and standard deviation of the generation time of the infecting pathogen
##'
##' @param epi.data a three-column matrix giving the coordinates (\code{x} and \code{y}) and time of infection (\code{t} for all cases in an epidemic (columns must be in \code{x}, \code{y}, \code{t} order)
##' @param gen.t.mean mean generation time of the infecting pathogen
##' @param gen.t.sd standard deviation of generation time of the infecting pathogen
##' @param t1 time step to begin estimation of transmission distance
##' @param max.sep maximum number of time steps allowed between two cases (passed to the \code{get.transdist.theta} function)
##' @param max.dist maximum spatial distance between two cases considered in calculation
##' @param n.transtree.reps number of time to simulate transmission trees when estimating the weights of theta (passed to the \code{est.transdist.theta.weights} function, default = 10). Warning: higher values of this parameter cause significant increases in computation time.
##' @param theta.weights use external matrix of theta weights. If NULL (default) the matrix of theta weights is automatically estimated by calling the \code{est.transdist.theta.weights} function
##' @param silent silence progress messages (default = F)
##' 
##' @return a list containing the estimated mean distance of the transmission kernel (\code{mu.est}) and its standard deviation (\code{sigma.est}). Bounded estimates (\code{bound.mu.est} and \code{bound.sigma.est}) are also given for when the assumption of equal mean and standard deviation is violated.
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references
##' Salje H, Cummings DAT and Lessler J (2016). “Estimating infectious disease transmission distances using the overall distribution of cases.” Epidemics, 17, pp. 10–18. ISSN 1755-4365, doi: \href{https://www.sciencedirect.com/science/article/pii/S1755436516300317}{10.1016/j.epidem.2016.10.001}.
##' 
##' @family est.wt
##' @family transdist
##'
##' @example R/examples/est_transdist.R
##'

est.transdist <- function(
  epi.data,				                     # Three column matrix: coordinates and time of case (must be in x, y, t, order)
  gen.t.mean,				                   # Mean of generation time
  gen.t.sd, 			                     # SD of generation time
  t1,			                             # Start of epidemic
  max.sep, 		                         # Maximum time between cases considered in calculation
  max.dist, 	                         # Maximum distance considered in calculation
  n.transtree.reps=100, 			         # How many times to simulate transmission trees
  theta.weights=NULL,  			           # External matrix of theta weights
  silent=FALSE                             # silence progress messages
){
  
  # Data check
  if(is.matrix(epi.data) == FALSE) stop('Epidemic data must be an integer or numeric matrix')
  if(is.numeric(epi.data) == FALSE & is.integer(epi.data) == FALSE) stop('Epidemic data must be an integer or numeric matrix')
  
  miss <- sum(complete.cases(epi.data) != TRUE)
  if(miss > 0) {
    epi.data <- epi.data[complete.cases(epi.data),]
    warning(paste(miss, 'rows were removed from data matrix due to missing data', sep=" "))
  }

  epi.data <- epi.data[order(epi.data[,3]),] 	# Reorder so in order of time
  case.times <- round(epi.data[,3])			      # Just the case times
  unique.times <- unique(case.times)	 	      # Unique time points
  ntimes <- length(unique.times) 			        # Number of unique time points
  
  ## Generation time distribution
  max.t <- round(max(unique.times) - t1) - 1
  n.step <- round(max.t/gen.t.mean)
  gen <- rep(0, max.t*2)
  
  for (i in 1:n.step){gen <- gen + dnorm(1:(max.t*2), gen.t.mean*i, gen.t.sd*i)}
  gen[1] <- 0 							# No instantaneous infections
  t.density <- gen/sum(gen)
  
  ## If weights are not provided, calculate weights
  if(is.null(theta.weights)){
    
    if(silent == FALSE) print("Estimating matrix of theta weights...", quote=FALSE)
    
    theta.weights <- est.transdist.theta.weights(case.times=case.times, 
                                                 n.rep=n.transtree.reps, 
                                                 gen.t.mean=gen.t.mean, 
                                                 t1=t1, 
                                                 t.density=t.density)
  }
  
  thetas <- 1:dim(theta.weights)[3] # range of thetas
  
  window <- bounding.box.xy(epi.data[,1:2])
  
  A <- B <- wt.mat.sigma <- wt.mat <- array(NaN, c(ntimes, ntimes))
  
  if(silent == FALSE) print(paste("Estimating transmission distance for", ntimes, "unique time points...", sep=" "), quote=FALSE)
  
  for (j in 1:ntimes){
    for (k in 1:j){
      
      if(abs(unique.times[j] - unique.times[k]) > max.sep)(next)
      
      wt.mat[j,k] <- sum(theta.weights[j,k,thetas]*sqrt(2*pi*thetas)/2, na.rm=TRUE)
      wt.mat.sigma[j,k] <- 2*(sum(theta.weights[j,k,thetas]*thetas, na.rm=TRUE) - pi/4*(sum(theta.weights[j,k,thetas]*sqrt(thetas), na.rm=TRUE))^2*(2 - sum(theta.weights[j,k,thetas], na.rm=TRUE)))
      wt.sqrt <- sum(theta.weights[j,k,thetas]*sqrt(thetas))
      
      B[j,k] <- sum(theta.weights[j,k,thetas]*((sqrt(thetas) - wt.sqrt)^2 + thetas*(4/pi-1)), na.rm=TRUE)
      A[j,k] <- wt.sqrt^2
    }}
  
  wt.mat[which(wt.mat == 0)] <- NA
  wt.mat.sigma[which(wt.mat.sigma == 0)] <- NA
  
  obs.sigma <- counts <- mat <- array(NaN, c(ntimes, ntimes))
  
  for (j in 1:ntimes){
    
    for (k in 1:j){
      
      if(abs(unique.times[j] - unique.times[k]) > max.sep)(next)
      a <- which(case.times == unique.times[j])
      b <- which(case.times == unique.times[k])
      
      if(length(a) == 0 | length(b) == 0)(next)
      ppp1 <- as.ppp(epi.data[a,1:2, drop=FALSE], window, check=FALSE)
      ppp2 <- as.ppp(epi.data[b,1:2, drop=FALSE], window, check=FALSE)
      c <- crossdist(ppp1, ppp2)
      
      if(j == k){diag(c) <- NA}
      c <- c[which(c < max.dist & c > 0)]
      
      if(length(c) == 0)(next)
      mat[j,k] <- mean(c, na.rm=TRUE)
      obs.sigma[j,k] <- sd(c, na.rm=TRUE)
      counts[j,k] <- sum(unique(c) > 0, na.rm=TRUE)
    }}
  
  mat[which(mat == 0)] <- NA
  
  mu.est <- sum((counts/sum(counts, na.rm=TRUE))*mat/wt.mat, na.rm=TRUE)
  sigma.est <- sum((counts/sum(counts, na.rm=TRUE))*obs.sigma/sqrt(wt.mat.sigma), na.rm=TRUE)
  bound.mu.est <- sqrt(2)*mu.est
  bound.sigma.est <- sqrt(2)*sigma.est
  
  if(silent == FALSE) print("Complete.", quote=FALSE)
  
  outlist <- list(mu.est=mu.est, 
                  sigma.est=sigma.est, 
                  bound.mu.est=bound.mu.est,
                  bound.sigma.est=bound.sigma.est)
  
  return(outlist)
}

##' Bootstrap mean transmission distance values 
##'
##' Runs \code{est.trandsdist} on multiple bootstraps of the data and calculates confidence intervals for the mean transmission distance.
##'
##' @param epi.data a three-column matrix giving the coordinates (\code{x} and \code{y}) and time of infection (\code{t} for all cases in an epidemic (columns must be in \code{x}, \code{y}, \code{t} order)
##' @param gen.t.mean mean generation time of the infecting pathogen
##' @param gen.t.sd standard deviation of generation time of the infecting pathogen
##' @param t1 time step to begin estimation of transmission distance
##' @param max.sep maximum number of time steps allowed between two cases (passed to the \code{get.transdist.theta} function)
##' @param max.dist maximum spatial distance between two cases considered in calculation
##' @param n.transtree.reps number of time to simulate transmission trees when estimating the weights of theta (passed to the \code{est.transdist.theta.weights} function, default = 10). Warning: higher values of this parameter cause significant increases in computation time.
##' @param theta.weights use external matrix of theta weights. If NULL (default) the matrix of theta weights is automatically estimated by calling the \code{est.transdist.theta.weights} function
##' @param boot.iter the number of bootstrapped iterations to perform
##' @param ci.low low end of the confidence interval (default = 0.025)
##' @param ci.high high end of the confidence interval (default = 0.975)
##' @param parallel run bootstraps in parallel (default = FALSE)
##' @param n.cores number of cores to use when \code{parallel} = TRUE (default = NULL, which uses half the available cores)
##' 
##' @return a list object containing the point estimate for mean transmission distance and low and high bootstrapped confidence intervals
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references
##' Salje H, Cummings DAT and Lessler J (2016). “Estimating infectious disease transmission distances using the overall distribution of cases.” Epidemics, 17, pp. 10–18. ISSN 1755-4365, doi: \href{https://www.sciencedirect.com/science/article/pii/S1755436516300317}{10.1016/j.epidem.2016.10.001}.
##' 
##' @family transdist
##'
##' @example R/examples/est_transdist_bootstrap_ci.R
##'

est.transdist.bootstrap.ci <- function(
  epi.data,				                     # Three column matrix: coordinates and time of case (must be in x, y, t, order)
  gen.t.mean,				                   # Mean of generation time
  gen.t.sd, 			                     # SD of generation time
  t1,			                             # Start of epidemic
  max.sep, 		                         # Maximum time between cases considered in calculation
  max.dist, 	                         # Maximum distance considered in calculation
  n.transtree.reps=100, 			         # How many times to simulate transmission trees
  theta.weights=NULL,  			           # External matrix of theta weights
  boot.iter,
  ci.low=0.025,
  ci.high=0.975,
  parallel=FALSE,
  n.cores=NULL
){
  
  # Point estimate
  p <- est.transdist(epi.data=epi.data,
                     gen.t.mean=gen.t.mean,
                     gen.t.sd=gen.t.sd,
                     t1=t1,
                     max.sep=max.sep,
                     max.dist=max.dist,
                     n.transtree.reps=n.transtree.reps,
                     theta.weights=theta.weights,
                     silent=TRUE)
  
  n <- nrow(epi.data)
  
  if(parallel == FALSE) {
    
    i <- NULL # to avoid no binding error for i
    bs <- foreach::foreach(i=1:boot.iter, .combine='c', .multicombine=TRUE) %do% {
      
      samp <- sample(1:n, n, replace=TRUE)
      
      a <- est.transdist(epi.data=epi.data[samp,],
                         gen.t.mean=gen.t.mean,
                         gen.t.sd=gen.t.sd,
                         t1=t1,
                         max.sep=max.sep,
                         max.dist=max.dist,
                         n.transtree.reps=n.transtree.reps,
                         theta.weights=theta.weights,
                         silent=TRUE)
      a$mu.est
    }
  }
  
  if(parallel == TRUE) {
    
    if(is.null(n.cores)) clust <- parallel::makeCluster(parallel::detectCores()/2)
    if(!is.null(n.cores)) clust <- parallel::makeCluster(n.cores)
    
    doParallel::registerDoParallel(clust) # register as foreach back end
    parallel::clusterExport(clust, c("est.transdist", ls(environment())), envir=environment())
    
    i <- NULL # to avoid no binding error for i
    bs <- foreach::foreach(i=1:boot.iter, .combine='c', .multicombine=TRUE) %dopar% {
      
      samp <- sample(1:n, n, replace=TRUE)
      
      a <- est.transdist(epi.data=epi.data[samp,],
                         gen.t.mean=gen.t.mean,
                         gen.t.sd=gen.t.sd,
                         t1=t1,
                         max.sep=max.sep,
                         max.dist=max.dist,
                         n.transtree.reps=n.transtree.reps,
                         theta.weights=theta.weights,
                         silent=TRUE)
      
      a$mu.est
    }
    parallel::stopCluster(clust)
  }
  
  ci <- quantile(bs, probs=c(ci.low, ci.high))
  
  return(list(mu.est=p$mu.est,
              mu.ci.low=ci[1],
              mu.ci.high=ci[2]))
}

##' Change in mean transmission distance over time
##'
##' Estimates the change in mean transmission distance over the duration of the epidemic by running \code{est.trandsdist} on all cases 
##' occuring up to each time point.
##'
##' @param epi.data a three-column matrix giving the coordinates (\code{x} and \code{y}) and time of infection (\code{t} for all cases in an epidemic (columns must be in \code{x}, \code{y}, \code{t} order)
##' @param gen.t.mean mean generation time of the infecting pathogen
##' @param gen.t.sd standard deviation of generation time of the infecting pathogen
##' @param t1 time step to begin estimation of transmission distance
##' @param max.sep maximum number of time steps allowed between two cases (passed to the \code{get.transdist.theta} function)
##' @param max.dist maximum spatial distance between two cases considered in calculation
##' @param n.transtree.reps number of time to simulate transmission trees when estimating the weights of theta (passed to the \code{est.transdist.theta.weights} function, default = 10). Higher values of this parameter cause significant increases in computation time.
##' @param theta.weights use external matrix of theta weights. If NULL (default) the matrix of theta weights is automatically estimated by calling the \code{est.transdist.theta.weights} function
##' @param parallel run time steps in parallel (default = FALSE)
##' @param n.cores number of cores to use when \code{parallel} = TRUE (default = NULL, which uses half the available cores)
##' 
##' @return a vector containing the point estimate for mean transmission distance for each unique time step of the epidemic. 
##' NAs are returned for time steps which contain fewer than three cases
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references
##' Salje H, Cummings DAT and Lessler J (2016). “Estimating infectious disease transmission distances using the overall distribution of cases.” Epidemics, 17, pp. 10–18. ISSN 1755-4365, doi: \href{https://www.sciencedirect.com/science/article/pii/S1755436516300317}{10.1016/j.epidem.2016.10.001}.
##' 
##' @family transdist
##'
##' @example R/examples/est_transdist_temporal.R
##'

est.transdist.temporal <- function(
  epi.data,				                     # Three column matrix: coordinates and time of case (must be in x, y, t, order)
  gen.t.mean,				                   # Mean of generation time
  gen.t.sd, 			                     # SD of generation time
  t1,			                             # Start of epidemic
  max.sep, 		                         # Maximum time between cases considered in calculation
  max.dist, 	                         # Maximum distance considered in calculation
  n.transtree.reps=10, 			           # How many times to simulate transmission trees
  theta.weights=NULL,  			           # External matrix of theta weights
  parallel=FALSE,
  n.cores=NULL
){
  
  # Data check
  if(is.matrix(epi.data) == FALSE) stop('Epidemic data must be an integer or numeric matrix')
  if(is.numeric(epi.data) == FALSE & is.integer(epi.data) == FALSE) stop('Epidemic data must be an integer or numeric matrix')
  
  miss <- sum(complete.cases(epi.data) != TRUE)
  if(miss > 0) {
    epi.data <- epi.data[complete.cases(epi.data),]
    warning(paste(miss, 'rows were removed from data matrix due to missing data', sep=" "))
  }
  
  if(parallel == FALSE) {
    
    unique.times <- unique(epi.data[,3])
    unique.times <- unique.times[order(unique.times)]
    
    i <- NULL # to avoid no binding error for i
    out <- foreach::foreach(i=seq_along(unique.times), .combine='rbind') %do% {
      
      d <- epi.data[epi.data[,3] <= unique.times[i],]
      n <- nrow(matrix(d, ncol=ncol(epi.data)))
      
      pt.est <- try(est.transdist(epi.data=d,
                                  gen.t.mean=gen.t.mean,
                                  gen.t.sd=gen.t.sd,
                                  t1=t1,
                                  max.sep=max.sep,
                                  max.dist=max.dist,
                                  n.transtree.reps=n.transtree.reps,
                                  silent=TRUE),
                    silent=TRUE)
      if(class(pt.est) == "try-error") {
        pt.est <- NA } else {
          pt.est <- pt.est$mu.est
        }
      pt.est
    }
  }
  
  if(parallel == TRUE) {
    
    if(is.null(n.cores)) clust <- parallel::makeCluster(parallel::detectCores()/2)
    if(!is.null(n.cores)) clust <- parallel::makeCluster(n.cores)
    
    doParallel::registerDoParallel(clust) # register as foreach back end
    parallel::clusterExport(clust, c("est.transdist", ls(environment())), envir=environment())
    
    unique.times <- unique(epi.data[,3])
    unique.times <- unique.times[order(unique.times)]
    
    i <- NULL # to avoid no binding error for i
    out <- foreach::foreach(i=seq_along(unique.times), .combine='rbind') %dopar% {
      
      d <- epi.data[epi.data[,3] <= unique.times[i],]
      n <- nrow(matrix(d, ncol=ncol(epi.data)))
      
      pt.est <- try(est.transdist(epi.data=d,
                                  gen.t.mean=gen.t.mean,
                                  gen.t.sd=gen.t.sd,
                                  t1=t1,
                                  max.sep=max.sep,
                                  max.dist=max.dist,
                                  n.transtree.reps=n.transtree.reps,
                                  silent=TRUE),
                    silent=TRUE)
      if(class(pt.est) == "try-error") {
        pt.est <- NA 
      } else {
        pt.est <- pt.est$mu.est
      }
      pt.est
    }
    parallel::stopCluster(clust)
  }
  return(as.vector(out))
}

##' Bootstrapped confidence intervals for the change in mean transmission distance over time
##'
##' Estimates bootstrapped confidence intervals for the mean transmission distance over the duration of the epidemic by running \code{est.trandsdist} on all cases 
##' occuring up to each time point. 
##'
##' @param epi.data a three-column matrix giving the coordinates (\code{x} and \code{y}) and time of infection (\code{t} for all cases in an epidemic (columns must be in \code{x}, \code{y}, \code{t} order)
##' @param gen.t.mean mean generation time of the infecting pathogen
##' @param gen.t.sd standard deviation of generation time of the infecting pathogen
##' @param t1 time step to begin estimation of transmission distance
##' @param max.sep maximum number of time steps allowed between two cases (passed to the \code{get.transdist.theta} function)
##' @param max.dist maximum spatial distance between two cases considered in calculation
##' @param n.transtree.reps number of time to simulate transmission trees when estimating the weights of theta (passed to the \code{est.transdist.theta.weights} function, default = 10). Warning: higher values of this parameter cause significant increases in computation time.
##' @param theta.weights use external matrix of theta weights. If NULL (default) the matrix of theta weights is automatically estimated by calling the \code{est.transdist.theta.weights} function
##' @param boot.iter the number of bootstrapped iterations to perform
##' @param ci.low low end of the confidence interval (default = 0.025)
##' @param ci.high high end of the confidence interval (default = 0.975)
##' @param parallel run bootstraps in parallel (default = FALSE)
##' @param n.cores number of cores to use when \code{parallel} = TRUE (default = NULL, which uses half the available cores)
##' 
##' @return a three-column matrix containing the point estimate for mean transmission distance and low and high bootstrapped confidence intervals
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references
##' Salje H, Cummings DAT and Lessler J (2016). “Estimating infectious disease transmission distances using the overall distribution of cases.” Epidemics, 17, pp. 10–18. ISSN 1755-4365, doi: \href{https://www.sciencedirect.com/science/article/pii/S1755436516300317}{10.1016/j.epidem.2016.10.001}.
##' 
##' @family transdist
##'
##' @example R/examples/est_transdist_temporal_bootstrap_ci.R
##'


est.transdist.temporal.bootstrap.ci <- function(
  epi.data,				                     # Three column matrix: coordinates and time of case (must be in x, y, t, order)
  gen.t.mean,				                   # Mean of generation time
  gen.t.sd, 			                     # SD of generation time
  t1,			                             # Start of epidemic
  max.sep, 		                         # Maximum time between cases considered in calculation
  max.dist, 	                         # Maximum distance considered in calculation
  n.transtree.reps=100, 			         # How many times to simulate transmission trees
  theta.weights=NULL,  			           # External matrix of theta weights
  boot.iter,
  ci.low=0.025,
  ci.high=0.975,
  parallel=FALSE,
  n.cores=NULL
){
  
  # Data check
  if(is.matrix(epi.data) == FALSE) stop('Epidemic data must be an integer or numeric matrix')
  if(is.numeric(epi.data) == FALSE & is.integer(epi.data) == FALSE) stop('Epidemic data must be an integer or numeric matrix')
  
  miss <- sum(complete.cases(epi.data) != TRUE)
  if(miss > 0) {
    epi.data <- epi.data[complete.cases(epi.data),]
    warning(paste(miss, 'rows were removed from data matrix due to missing data', sep=" "))
  }
  
  if(parallel == FALSE) {
    
    unique.times <- unique(epi.data[,3])
    unique.times <- unique.times[order(unique.times)]
    
    i <- NULL # to avoid no binding error for i
    out <- foreach::foreach(i=seq_along(unique.times), .combine='rbind') %do% {
      
      d <- epi.data[epi.data[,3] <= unique.times[i],]
      n <- nrow(matrix(d, ncol=ncol(epi.data)))
      
      pt.est <- try(est.transdist(epi.data=d,
                                  gen.t.mean=gen.t.mean,
                                  gen.t.sd=gen.t.sd,
                                  t1=t1,
                                  max.sep=max.sep,
                                  max.dist=max.dist,
                                  n.transtree.reps=n.transtree.reps,
                                  silent=TRUE),
                    silent=TRUE)
      if(class(pt.est) == "try-error") {
        pt.est <- NA } else {
          pt.est <- pt.est$mu.est
        }
      
      bs <- rep(NA, boot.iter)
      for(j in 1:boot.iter) {
        
        samp <- sample(1:n, n, replace=TRUE)
        
        a <- try(est.transdist(epi.data=d[samp,],
                               gen.t.mean=gen.t.mean,
                               gen.t.sd=gen.t.sd,
                               t1=t1,
                               max.sep=max.sep,
                               max.dist=max.dist,
                               n.transtree.reps=n.transtree.reps,
                               silent=TRUE), 
                 silent=TRUE)
        if(class(a) == "try-error") {
          bs[j] <- NA} else {
            bs[j] <- a$mu.est
          }
      }
      
      suppressWarnings(bs.probs <- quantile(as.numeric(bs), probs=c(ci.low, ci.high), na.rm=TRUE))
      x <- c(pt.est, bs.probs[1], bs.probs[2])
    }
  }
  
  if(parallel == TRUE) {
    
    if(is.null(n.cores)) clust <- parallel::makeCluster(parallel::detectCores()/2)
    if(!is.null(n.cores)) clust <- parallel::makeCluster(n.cores)
    
    doParallel::registerDoParallel(clust) # register as foreach back end
    parallel::clusterExport(clust, c("est.transdist", ls(environment())), envir=environment())
    
    unique.times <- unique(epi.data[,3])
    unique.times <- unique.times[order(unique.times)]
    
    i <- NULL # to avoid no binding error for i
    out <- foreach::foreach(i=seq_along(unique.times), .combine='rbind') %dopar% {
      
      d <- epi.data[epi.data[,3] <= unique.times[i],]
      n <- nrow(matrix(d, ncol=ncol(epi.data)))
      
      pt.est <- try(est.transdist(epi.data=d,
                                  gen.t.mean=gen.t.mean,
                                  gen.t.sd=gen.t.sd,
                                  t1=t1,
                                  max.sep=max.sep,
                                  max.dist=max.dist,
                                  n.transtree.reps=n.transtree.reps,
                                  silent=TRUE),
                    silent=TRUE)
      if(class(pt.est) == "try-error") {
        pt.est <- NA 
        } else {
          pt.est <- pt.est$mu.est
        }
      
      bs <- rep(NA, boot.iter)
      for(j in 1:boot.iter) {
        
        samp <- sample(1:n, n, replace=TRUE)
        
        a <- try(est.transdist(epi.data=d[samp,],
                               gen.t.mean=gen.t.mean,
                               gen.t.sd=gen.t.sd,
                               t1=t1,
                               max.sep=max.sep,
                               max.dist=max.dist,
                               n.transtree.reps=n.transtree.reps,
                               silent=TRUE), 
                 silent=TRUE)
        if(class(a) == "try-error") {
          bs[j] <- NA
          } else {
            bs[j] <- a$mu.est
          }
      }
      
      suppressWarnings(bs.probs <- quantile(as.numeric(bs), probs=c(ci.low, ci.high), na.rm=TRUE))
      x <- c(pt.est, bs.probs[1], bs.probs[2])
    }
    parallel::stopCluster(clust)
  }
  
  row.names(out) <- NULL
  colnames(out) <- c('mu.est', 'mu.ci.low', 'mu.ci.high')
  return(out)
}

NULL
