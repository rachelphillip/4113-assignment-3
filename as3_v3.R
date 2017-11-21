# I confirm that the attached work is my own, except where clearly indicated in the text.

###############################################
#CONFIDENCE INTERVALS &
#COVERAGE FNS
##############################################

b.ests.np <- function(B = 99, data, 
                      h = sd(data)/3, method) {
  #Purpose: Creates the bootstrap estimates for a dataset by taking B samples of
  #         size n from the dataset and taking the mean of each resample to create our samples
  #Inputs:
  # B - a scalar - number bootstrap resamples to be taken
  # n -  a scalar - size of resamples
  # data - vector of data you wish to create bootstraps from
  # h - scalar - degree of smoothing
  # method - named vector of methods, indicating what method of bootstrap confidence
  #          interval you wish to use. Must be one of "percentile", "bca", "t" and "smooth"
  #Outputs:
  # boot.est - vector of length B containing all the bootstrap estimates of interest
  #            from each sample
  
  if(!is.numeric(data)){
    stop("data must be numeric")
  }
  
  x.star <- matrix(sample(x = data,  length(data)*B, replace = T), ncol = B)
  
  if (method == "smooth"){
    
    x.star <- x.star + rnorm(prod(dim(x.star)))
    
  }
  
  if (method == "t"){
    
    B.ests <- apply(x.star, 2, b.t, data = data)
    
  } else{
    
    B.ests <- apply(x.star, 2, mean)
    
  }
  
  return(B.ests)
  
}


#---------------------------------------------------------------------------------

b.t <- function(x, data){
  #Purpose: function to be used in a apply call in b.ests.np to create the bootstrap-t
  #         estimates
  #Inputs: 
  # x - vector - bootstrap sample used to created estimate
  # data - vector - dataset from which we are sampling and using to create bootstrap
  #        estimates
  #Output:
  # scalar - the bootstrap-t estimate for the bootstrap sample from the data
  
  return((mean(x)-mean(data))/sd(x)) 
  
}

#----------------------------------------------------------------------------------

r.add <- function(x, h){
  #Purpose: adds noise to a scalar value x using random deviates from the standard
  #         normal distribution. This function is to be used in the smooth bootstrap
  #         in a apply call in b.ests.np
  #Inputs:
  # x - scalar that has noise added to it
  # h - scalar that indicates the degree of smoothing
  #Output:
  # scalar - original value of x with noise added
  
  
  return(x + rnorm(1, 0, sd = h))
}


#----------------------------------------------------------------------------------

#Using len's functions:

get.zhat0<-function(est, boot.est){
  #Purpose: Return the bias correction factor, zhat0, in BCa bootstrap CI method
  #Inputs:
  # est - estimated quantity of interest from data
  # boot.est - vector of bootstrap estimates of quantity of interest
  #Outputs:
  # zhat - scalar - bias correction factor
  
  if (!is.numeric(est)){
    stop("est must be numeric")
  }
  if (length(est) > 1){
    stop("est must be a scalar")
  }
  if (!is.numeric(boot.est)){
    stop("boot.est must be numeric")
  }
  
  
  prop.less<-sum(boot.est<est)/length(boot.est)
  zhat<-qnorm(prop.less)
  
  return(zhat)
}



#------------------------------------------------------------------------------------------

get.ahat <- function(data){
  #Purpose: Return the acceleration factor, ahat, in BCa bootstrap CI method
  #Inputs:
  # data - vector of data
  # est - estimated quantity of interest from data
  # fun - function that can be used to produce est from data via fun(data)
  # dist - character indicating the distribution that the data follows
  #        so that the correct est is created to be used in fun
  #Implementation note:
  # 1. The routine calls fun(data,...) and expects it to return a scalar equal to est
  # 2. Requires a data vector of length at least 2
  
  #Check data vector length
  n <- length(data)
  if (n < 2) stop("data vector must be at least length 2\n")
  
  #Get jacknife estimates of quantity of interest
  jack.est <- numeric(n)
  
  for (i in 1:n){
    
    jack.data <- data[-i]
    jack.est[i] <- mean(x = jack.data)
    
  }
  
  #Compute ahat
  mean.jack.est <- mean(jack.est)
  ahat.numerator <- sum((mean.jack.est - jack.est)^3)
  ahat.denominator <- 6*(sum((mean.jack.est - jack.est)^2))^1.5
  ahat <- ahat.numerator / ahat.denominator
  
  return(ahat)
}


#---------------------------------------------------------------------------------

alpha.perc <- function(alpha){
  #Purpose: gives the correct percentiles to use in the quantile function
  #         for the percentile method
  #Inputs:
  # alpha - scalar - gives the size of the interval so alpha = 0.025 gives 
  #         a 95% CI ie. a (1-2*alpha)*100% CI
  #Outputs:
  # vector of size 2 containing alpha and 1-alpha
  
  alpha. <- c(alpha, (1-alpha))
  
  return((alpha.)) 
}

#----------------------------------------------------------------------------------

alpha.bca <- function(alpha = 0.025, data, est, boot.est){
  #Purpose: gives the correct percentiles to use in the quantile fn
  #         for the BCa method
  #Inputs:
  # alpha - scalar - gives the size of the interval
  # data - vector of dataset you are using to create CIs
  # est - estimated quanity of interest from data
  # boot.est - vector of bootstrap estimates
  #Outputs:
  # vector of size 2 containing the BCa percentiles
  
  if (!is.numeric(est)){
    stop("est must be numeric")
  }
  if (length(est) > 1){
    stop("est must be a scalar")
  }
  if (!is.numeric(boot.est)){
    stop("boot.est must be numeric")
  }
  
  ahat <- get.ahat(data)
  zhat <- get.zhat0(est = est, boot.est = boot.est)
  
  alpha1 <- pnorm(zhat + (zhat + qnorm(alpha)) / (1 - ahat*(zhat + qnorm(alpha))))
  alpha2 <- pnorm(zhat + (zhat + qnorm(1 - alpha)) / (1 - ahat*(zhat + qnorm(1 - alpha))))
  alpha. <- c(alpha1, alpha2)
  
  
  return(alpha.)
}

#---------------------------------------------------------------------------------

CI <- function(alpha, boot.est){
  #Purpose: Creates CIs for the BCa, percentile and smooth methods by using the 
  #         quantile function using the appropriate alphas for each method.
  #Inputs:
  # alpha - the percentiles for the method to be used in the quantile function to create the CI
  # boot.est - list of 2 lists of bootstrap estimates for each level of B and n
  #Output:
  # CI.bca - output of the quantile function, vector of size 2 with named entries
  #          that is the percentile used
  
  
  CI.bca <- quantile(boot.est, probs = alpha)
  
  return(CI.bca)
}

#----------------------------------------------------------------------------------

CI.t <- function(alpha, est, data, boot.est){
  #Purpose: Generates the confidence interval for the bootstrap-t method
  #Inputs:
  # alpha - vector of the percentiles for the percentile method to be used in 
  #         the quantile function to create the CI
  # est - scalar - the estimated mean from the data
  # data - vector of the dataset sampled from to create the bootstrap estimates
  # boot.est - vector of bootstrap estimates for each sample
  #Output:
  # CI.t - vector of upper and lower limits of the confidence interval for bootstrap-t
  
  
  quantiles <- CI(1-alpha, boot.est)
  
  CI.t <- est - sd(data)*quantiles
  
  return(CI.t)
}

#----------------------------------------------------------------------------------

bootstrap.ci <- function(B = 99,
                         data, alpha = 0.025, method, dist = "normal", 
                         h = sd(data)/3){
  #Purpose: wrapper function that creates bootstrap estimates for each level of B and n 
  #         and calculates percentiles for each method and then the corresponding CI.
  #Inputs:
  #Inputs:
  # B - a scalar - number of bootstraps resamples to be taken
  # data - vector of dataset you wish to create bootstraps from
  # alpha - scalar - indicatess the size of the interval so alpha = 0.025 gives 
  #         a 95% CI ie. a (1-2*alpha)*100% CI for the percentile method
  # dist - character giving distribution of simulated data, must be one of "normal",
  #        "poisson" or "gamma"
  # h - scalar - degree of smoothing
  # method - named vector of methods, indicating what method of bootstrap CI
  #          you wish to use. Must be one of "percentile", "bca", "smooth" or "t"
  #Outputs:
  # list of:
  # CI - vector of size 2 containing the upper and lower limits of the CI
  
  if (!is.numeric(data)){
    stop("data must be numeric")
  }
  if (length(data) < 2){
    stop("data must have length greater than 2")
  }
  if (h < 0){
    stop("h must be positive")
  }
  if (length(h) > 1){
    stop("h must be a scalar")
  }
  if(!is.numeric(h)){
    stop("h must be numeric")
  }
  
  #generate bootstrap estimates
  boot.est <- b.ests.np(B, data, method = method, h = h)
  est <- mean(data)
  
  #getting right arguments for alpha fn
  args <- switch(method, 
                 percentile = list(alpha),
                 smooth = list(alpha),
                 t = list(alpha),
                 bca = list(alpha = alpha, data = data, est = est,
                            boot.est = boot.est))
  
  #getting right percentiles for the method
  alpha.fn <- switch(method,
                     percentile = match.fun(alpha.perc),
                     smooth = match.fun(alpha.perc),
                     t = match.fun(alpha.perc),
                     bca = match.fun(alpha.bca))
  
  alpha <- do.call(alpha.fn, args) # get the percentiles
  
  # getting arguments for CI fn
  args.CI <- switch(method,
                    percentile = list(alpha = alpha, 
                                      boot.est = boot.est),
                    smooth = list(alpha = alpha, boot.est = boot.est),
                    t = list(alpha = alpha, 
                             boot.est = boot.est, data = data, est = est),
                    bca = list(alpha = alpha, 
                               boot.est = boot.est))
  # getting CI method
  CI.method <- switch(method, 
                      percentile = match.fun(CI),
                      smooth = match.fun(CI),
                      bca = match.fun(CI),
                      t = match.fun(CI.t))
  
  CI <- do.call(CI.method, args = args.CI) # get CI
  
  return(CI = CI)
}

#---------------------------------------------------------------------

data.gen <- function(dist, mean, sd, lambda, shape, rate, n){
  #Purpose: Generates the data to be used as the resample for which a CI is created.
  #         This is the data argument used in b.ests.np and from which est is calculated
  #Inputs:
  # dist - character giving distribution of simulated data, must be one of "normal",
  #        "poisson" or "gamma"
  # mean - scalar - mean of the normal distribution that you want to generate samples from
  # sd - scalar - standard deviation of the normal dist for the sample
  # lambda - scalar - rate parameter for the poisson dist for the sample
  # shape - scalar - the shape parameter or alpha for the gamma dist for the sample
  # rate - scalar - the rate parameter or beta for the gamma dist for the sample
  # n - scalar - the number of random deviates to be sampled from the distribution
  #     ie. size of the bootstrap sample
  
  if (n%%1!=0 | n < 1){
    stop("n must be a positive non-zero integer")
  }
  if (!is.numeric(n)){
    stop("n must be numeric")
  }
  if (length(n) > 1){
    stop("n must be a scalar")
  }
  
  #getting correct arguments to be used in the random deviate fn
  args.dist <- switch(dist,
                      normal = list(n, mean, sd),
                      poisson = list(n, lambda),
                      gamma = list(n, shape, rate))
  
  #getting right distribution fn
  dist.fn <- switch(dist,
                    normal = match.fun(rnorm),
                    poisson = match.fun(rpois),
                    gamma = match.fun(rgamma))
  
  #sample created
  data.fn <- do.call(dist.fn, args = args.dist)
  
  return(data.fn)
}

#--------------------------------------------------------------------------------------

input.checks <- function(sims, B.v, B,
                         n.v, n, alpha, method, 
                         dist, mean, sd,
                         lambda, shape, rate){
  #Purpose: checks inputs into do.sim.np function are of the correct type and form.
  #Input:
  # sims - scalar - number of simulations to be performed
  # B.v - vector of the levels of B - number of bootstrap samples - to be used to create CIs
  # B - scalar - the B to kept constant as n varies in n.v
  # n.v - vector of the levels of n - size of resamples - to be used in simulation
  # n - scalar - the size of resamples to be kept constant as B changes in B.v
  # alpha - scalar - gives the size of the interval so alpha = 0.025 gives 
  #         a 95% CI ie. a (1-2*alpha)*100% CI for the percentile method
  # method - character giving the method to be used to create the CI. Must be one of "percentile",
  #          "bca" and "t"
  # dist - character that determines the distribution that the data are to be simulated from.
  #        One of "normal", "poisson" and "gamma"
  # h - scalar - degree of smoothing
  # mean - scalar - the mean of the normal distribution that the samples are simulated from
  # sd - scalar - the standard deviation of the normal distribution that the samples
  #               are simulated from
  # lambda - scalar - the rate parameter of the poisson distribution that the samples are generated from
  # shape - scalar - shape parameter (alpha) of the gamma dist the samples are simualted from
  # rate - scalar - rate parameter (beta) of the gamma dist the samples are generated from
  #Output:
  # if all the checks pass then nothing happens and the code carries on to run. If any fail
  # then an error message is produced.
  
  if (!is.numeric(sims) | !is.numeric(B.v) | !is.numeric(B) | 
      !is.numeric(n.v) | !is.numeric(n) | !is.numeric(alpha) |
      !is.numeric(mean) | !is.numeric(sd) | !is.numeric(lambda) |
      !is.numeric(shape) | !is.numeric(rate)){
    stop("input must be numeric")
  }
  
  if (sims%%1!=0 | B%%1!=0 | n%%1!=0){
    stop("input must be an integer")
  }
  
  if (sims < 1 | B < 1 | n < 1){
    stop("input must be  postive non-zero integer")
  }
  
  if (sd <= 0 | lambda <= 0 | shape <= 0 | rate <= 0){
    stop("input must be positive and non zero")
  }
  
  if (alpha <= 0 | alpha > 1){
    stop("alpha must be between 0 and 1")
  }
  
  if (!(all(B.v > 0)) | !(all(n.v > 0))){
    stop("all elements in vector must be positive and non zero")
  }
  
  if (!(all(B.v%%1==0)) | !(all(n.v%%1==0))){
    stop("all elements in vector must be integers")
  }
  
  if (length(B) > 1 | length(n) > 1 | length(mean) > 1 | 
      length(sd) > 1 | length(lambda) > 1 | length(alpha) > 1 | length(shape) > 1 |
      length(rate) > 1){
    stop("input must be a scalar")
  }
  
  if (!method %in% c("percentile", "bca", "t", "smooth")){
    stop("method is not valid")
  }
  
  if (!dist %in% c("normal", "poisson", "gamma")){
    stop("distribution is not valid")
  }
  
  if (!is.character(method) | !is.character(dist)){
    stop("input must be character")
  }
  
}

#--------------------------------------------------------------------------------------

do.sim.np <- function(sims = 100, B.v = c(10, 50, 100), B = 100,
                      n.v = c(10, 50, 100), n = 100, 
                      alpha = 0.025 , method = "percentile", 
                      dist = "normal", h = sd(data)/3, mean = 0 , sd = 1,
                      lambda = 10, shape = 1, rate = 1, check = T){
  #Purpose: Generates confidence intervals for each simulation and each level of B.v 
  #         and n.v Creates sims CIs.
  #         Data is generated from the distribution specified and this is our sample.
  #         Calls bootstrap.ci to create the CI for that sample for that B and n, using the method
  #         specified. The lower and upper limits of this CI are then stored in a matrix
  #         which is then stored in a list.
  #Inputs:
  # sims - scalar - number of simulations to be performed
  # B.v - vector of the levels of B - number of bootstrap samples - to be used to create CIs
  # B - scalar - the B to kept constant as n varies in n.v
  # n.v - vector of the levels of n - size of resamples - to be used in simulation
  # n - scalar - the size of resamples to be kept constant as B changes in B.v
  # alpha - scalar - gives the size of the interval so alpha = 0.025 gives 
  #         a 95% CI ie. a (1-2*alpha)*100% CI for the percentile method
  # method - character giving the method to be used to create the CI. Must be one of "percentile",
  #          "bca" and "t"
  # dist - character that determines the distribution that the data are to be simulated from.
  #        One of "normal", "poisson" and "gamma"
  # smooth - boolean T/F that determines whether a smooth bootstrap is used
  # h - scalar - degree of smoothing
  # mean - scalar - the mean of the normal distribution that the samples are simulated from
  # sd - scalar - the standard deviation of the normal distribution that the samples
  #               are simulated from
  # lambda - scalar - the rate parameter of the poisson distribution that the samples are generated from
  # shape - scalar - shape parameter (alpha) of the gamma dist the samples are simualted from
  # rate - scalar - rate parameter (beta) of the gamma dist the samples are generated from
  # check - boolean T/F indicating whether input checks should take place. 
  #Outputs:
  # list of 6 of :
  # dist - character indicating the distribution of the samples. Returning this to use in plotting
  #        functions later
  # method - character giving the method used to create bootstrap simulations. Used in plotting 
  # B.levels - vector of each level of B
  # n.levels - vector of each level of n
  # CI.B - list for each level of B where each element is a matrix. Rows are CI for each simulation. Columns are
  #        lower and upper limits of CI for that level of B.
  #CI.B - list for each level of n where each element is a matrix. Rows are CI for each simulation. Columns are
  #        lower and upper limits of CI for that level of n.
  
  if (!is.logical(check)){
    stop("check must be TRUE or FALSE")
  }
  
  if (check == TRUE){
    
    input.checks(sims = sims, B.v = B.v, B = B, n.v = n.v, n = n, alpha = alpha, 
                 method = method, dist = dist,
                 mean =  mean, sd = sd, lambda = lambda, 
                 shape = shape, rate = rate)
    
  }
  
  sim.CIs.B <- matrix(nrow = sims, ncol = 2) #matrix to store CIs for each level of B
  sim.CIs.n <- matrix(nrow = sims, ncol = 2) #matrix to store CIs for each level of n
  CI.B <- list()
  CI.n <- list()
  
  for (i in 1:length(B.v)){
    for (simulation in 1:sims){ # each row is CI for that simulation
      
      data <- data.gen(dist = dist, mean, sd, lambda, shape, rate, n)
      
      CI <- bootstrap.ci(B = B.v[i], data = data, 
                         alpha = alpha, method = method, 
                         dist = dist, h = h)
      
      sim.CIs.B[simulation,1] <- CI[1] # first column is lower limit of CIs
      sim.CIs.B[simulation,2] <- CI[2] # 2nd column is upper limit of CIs
      
    }
    
    CI.B[[i]] <- sim.CIs.B # all simulations for that level of B are stored in a matrix
    # and so each element in list is matrix for each level of B
    
  }
  
  for (j in 1:length(n.v)){
    for (simulation in 1:sims){
      
      data <- data.gen(dist = dist, mean, sd, lambda, shape, rate, n.v[j])
      
      CI <- bootstrap.ci(B = B, data = data, alpha = alpha,
                         method = method, dist = dist)
      
      sim.CIs.n[simulation,1] <- CI[1]
      sim.CIs.n[simulation,2] <- CI[2]
      
    }
    
    CI.n[[j]] <- sim.CIs.n # same as above but for each level of n
    
  }
  
  return(list(dist = dist, method = method, 
              B.levels = B.v, n.levels = n.v, CI.B = CI.B, CI.n = CI.n))
}


#-----------------------------------------------------------------------------------------

coverage <- function(true_mean, simulation){
  #Purpose: Calculates the average coverage, the proportion of times that the simulated
  #         confidence interval includes the true mean for each simulated dataset.
  #Input: 
  # true_mean - a scalar that is the true mean for the simulated dataset
  # simulation - an output from do.sim.np function that is a list of 4 elements,
  #              B.levels - vector of each level of B - number of bootstrap resamples
  #              n. levels - vector of each level of n - size of resample
  #              CI.B - list of a list for each level of B containing a matrix with #sims rows 
  #                      and 2 columns with the first element being the lower bound
  #                      and the 2nd element in the row being the upper bound of the CI
  #                      for each bootstrap sample
  #              CI.n - same as for CI.B but for each level of n instead
  #Ouput:
  # list of 6 elements:
  # dist - character - the distribution of the simulated data. I'm returning this so I can plot by
  #        distribution for each method type.
  # method - character of the method used to create CI
  # B.levels - vector of each level of B 
  # n.levels - vector of each level of n
  # B.cov - vector, length = number of B levels, containing the average coverage 
  #         for each level of B
  # n.cov - vector, length = number of n levels, containing the average coverage
  #         for each level of n
  
  sims <- length(simulation$CI.B[[1]][,1])
  dist <- simulation$dist
  method <- simulation$method
  B.levels <- simulation$B.levels
  n.levels <- simulation$n.levels
  no.B.levels <- length(simulation$CI.B) 
  no.n.levels <- length(simulation$CI.n)
  B.results <- rep(NA, sims)
  n.results <- rep(NA, sims)
  B.cov <- rep(NA, no.B.levels)
  n.cov <- rep(NA, no.n.levels)
  
  for (j in 1:no.B.levels){
    for (i in 1:sims){
      
      # is true_mean within CI?
      B.results[i] <- with(simulation, 
                           CI.B[[j]][i,1] <= true_mean & CI.B[[j]][i,2] >= true_mean)
      
    }
    
    #proportion of times true_mean is in CI  
    B.cov[j] <- length(B.results[B.results == TRUE])/length(B.results)
    
  }
  
  for (y in 1:no.n.levels){
    for (i in 1:sims){
      
      n.results[i] <- with(simulation, 
                           CI.n[[y]][i,1] <= true_mean & CI.n[[y]][i,2] >= true_mean)
      
    }
    
    n.cov[y] <- length(n.results[n.results == TRUE])/length(n.results)
    
  }
  
  
  return(list(dist = dist, method = method,
              B.levels = B.levels, n.levels = n.levels, 
              B.cov = B.cov, n.cov = n.cov))
}

#---------------------------------------------------------------------------------------------------

##################################################################
#PLOT FNS
##################################################################

format.data<- function(cov_objects){
  #Purpose: puts the coverage data into a format that can be used to create ggplots.
  #         Creates a data frame from a list of coverage outputs.
  #Input:
  # cov_objects - list of outputs from the coverage function
  #Output:
  # df - data frame with columns of B - levels of B, n - levels of n, coverage.B -
  #      the coverage of the simulation for the corresponding level of B, coverage.n -
  #      same as for coverage.B but for each corresponding level of n, method - the method
  #      of the coverage simulation object, dist - distribution of the data used in the coverage object
  
  
  B <- cov_objects[[1]]$B.levels
  n <- cov_objects[[1]]$n.levels
  N <- length(cov_objects)
  cov.B.L <- rep(NA, N)
  cov.n.L <- rep(NA, N)
  
  
  for (j in 1:N){
    
    if(B!=n){
      stop("There must be the same number of B and n levels")
    }
    
    cov.B.L[j] <- length(cov_objects[[j]]$B.cov)
    cov.n.L[j] <- length(cov_objects[[j]]$n.cov)
    
  }
  
  coverage.B <- rep(NA, sum(cov.B.L))
  coverage.n <- rep(NA, sum(cov.n.L))
  method <- rep(NA, sum(cov.n.L))
  dist <- rep(NA, sum(cov.n.L))
  
  for (i in 1:N){
    
    B.len <- length(cov_objects[[i]]$B.cov)
    n.len <- length(cov_objects[[i]]$n.cov)
    coverage.B[(B.len*(i-1)+1):(B.len*i)] <- cov_objects[[i]]$B.cov 
    coverage.n[(n.len*(i-1)+1):(n.len*i)] <- cov_objects[[i]]$n.cov 
    method[(B.len*(i-1)+1):(B.len*i)] <- cov_objects[[i]]$method
    dist[(B.len*(i-1)+1):(B.len*i)] <-cov_objects[[i]]$dist
    
  }
  
  df <- data.frame(B, coverage.B, n, coverage.n, method, dist)
  return(df)
  
}

#-----------------------------------------------------------------------------------------

#this doesn't work :/
plot.fn2 <- function(df, x, y = coverage.B, method.plot = T, dist.plot = F){
  #Purpose: creates plot that show
  # a) how coverage changes as either B or n changes, for multiple distributions using the same
  #    method or
  # b) how coverage changes as either B or n changes, for multiple methods using the same
  #    distribution
  #Inputs:
  # df - data frame, the output from the data.format function
  # x - the variable to be plotted along the x-axis, must be B or n
  # y - variable to be plotted along the y-axis must be coverage.B or coverage.n
  # method.plot - logical T/F indicating whether you want a plot to compare methods for one dist
  # dist.plot - logical T/F indicating whether you want a plot to compare dists for one method
  #Output:
  # ggplot of a) or b)
  
  if (method.plot == TRUE && dist.plot == TRUE){
    stop("both method.plot and dist.plot cannot be TRUE")
  }
  if (method.plot == FALSE && dist.plot == FALSE){
    return(NULL)
  }
  
  if (method.plot == TRUE && dist.plot == FALSE){
    
    p <- ggplot(data = df, aes(x = x, y = y)) +
      geom_point() +
      geom_line(aes(color = method)) +
      coord_cartesian(ylim = c(0.7,1))
    
  }
  
  if (dist.plot == TRUE && method.plot == FALSE){
    
    p <- ggplot(data = df, aes(x = x, y = y, dist = dist, group = dist)) +
      geom_point() +
      geom_line() +
      coord_cartesian(ylim = c(0.7,1))
    
  }
  
  
  p
}


