# MT 4113: Computing in Statistics, Assignment 1
# Student ID: 160023114

# I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n, mean = 0, sd = 1) {
  
  #   Purpose: 
  #     This function returns a vector of n pseudo-random values from a normal distribution.
  #     The numbers are generated with the Marsaglia and Bray's method.
  #
  #   Input:
  #     n - integer >= 0. number of desired random deviates
  #     mean - scalar. sets the mean of the normal distribution
  #     sd - scalar. sets the standard deviation of the normal distribution
  #
  #   Output:
  #     a vector of n pseudo-random values sampled from the distribution N(mean, sd^2)
  #
  # 
  #   Reference:
  #     Maindonald, John. 1984. Statistical Computation. John Wiley and Sons
  
  # check validity of arguments
  if(!is.numeric(c(n, mean, sd))) stop("invalid arguments")
  if(any(is.infinite(c(n, mean, sd)))) stop("invalid arguments")
  if(length(c(n, mean, sd))!=3) stop("invalid arguments")
  if(n%%1!=0 || n<0) stop("invalid arguments")
  if(sd<=0) stop("invalid arguments")
  
  # initiate vector to later save the generated numbers
  normal.deviates <- rep(0,n)
  # generate (n+1)%/%2 pairs of normally-distributed deviates
  # (n/2 pairs for n even, n/2+0.5 pairs for n odd)
  for (i in 1:((n+1)%/%2)) {
    # initiate w, so that while loop is entered
    w <- 2
    # create u1 and u2 that meet the criteria w <=1
    while (w>1) {
      # generate u1 and u2 from uniform(0,1) distribution
      u <- runif(2, min = 0, max = 1)
      # convert to u1, u2 ~ uniform(-1,1) and update w
      u <- 2*u-1
      w <- sum(u^2)
    }
    # calculate v, then x1, x2 ~ N(0,1) with the help of v, u1 and u2
    v <- sqrt((-2*log(w))/(w))
    x1 <- u[1]*v
    x2 <- u[2]*v
    # change to mean "mean" and standard deviation "sd"
    # and save in vector normal.deviates
    normal.deviates[2*i-1] <- x1*sd + mean
    normal.deviates[2*i] <- x2*sd + mean
  }
  # if n is odd, or n = 0, vector is too long. Correct the length.
  length(normal.deviates) <- n
  return(normal.deviates)
}

general.rnorm <- function(n, mean = 0, sd = 1, method = 1) {
  
  #   Purpose:
  #     This function returns a vector of n pseudo-random values from a normal distribution.
  #   
  #   Input:
  #     n - integer >= 0. number of desired random deviates
  #     mean - scalar. mean of the normal distribution
  #     sd - scalar. standard deviation of the normal distribution
  #     method - integer 1, 2 or 3. select method 1, 2, or 3 of how to generate the numbers
  # 
  #     method = 1: Marsaglia and Bray's method
  #     method = 2: Box and Mueller algorithm
  #     method = 3: central-limit theorem algorithm
  #
  #   Output:
  #     a vector of n pseudo-random values sampled from the distribution N(mean, sd^2)
  # 
  #   References:
  #     Jones, O., R. Maillardet, and A. Robinson. 2009. Scientific Programming and Simulation Using R. CRC Press.
  #     Maindonald, John. 1984. Statistical Computation. John Wiley and Sons
  #     Mood, A.M., F.A. Graybill, and D.C. Boes. 1974. Introduction to the Theory of Statistics. Third Edition. McGraw Hill.
  
  # check validity of arguments
  if(!is.numeric(c(n, mean, sd, method))) stop("invalid arguments")
  if(any(is.infinite(c(n, mean, sd, method)))) stop("invalid arguments")
  if(length(c(n, mean, sd, method))!=4) stop("invalid arguments")
  if(n%%1!=0 || n<0) stop("invalid arguments")
  if(sd<=0) stop("invalid arguments")
  if(!method %in% 1:3) stop("invalid arguments")
  
  
  # initiate vector to later save the generated numbers
  normal.deviates <- rep(0,n)
  
  if(method == 1){
    normal.deviates  <- my.rnorm(n, mean, sd)
  } else if (method == 2){
    # generate (n+1)%/%2 pairs of normally-distributed deviates
    # (n/2 pairs for n even, n/2+0.5 pairs for n odd) 
    for(i in 1:((n+1)%/%2)) {
      # generate u1 and u2 from uniform(0,1) distribution
      u1 <- runif(1, min = 0, max = 1)
      u2 <- runif(1, min = 0, max = 1)
      # calculate x1, x2 ~ N(0,1) from u1 and u2
      x1 <- sin(2*pi*u1)*sqrt(-2*log(u2))
      x2 <- cos(2*pi*u1)*sqrt(-2*log(u2))
      # change to mean "mean" and standard deviation "sd"
      # and save in vector normal.deviates
      normal.deviates[2*i-1] <- x1*sd + mean
      normal.deviates[2*i] <- x2*sd + mean
    }
    # if n is odd, or n = 0, vector is too long. Correct the length.
    length(normal.deviates) <- n
  } else if (method == 3){
    for(i in 1:n){
      u <- runif(16, min = 0, max = 1)
      x <-(sum(u)-8)*sqrt(12/16)
      normal.deviates[i] <- x*sd + mean
    }
    length(normal.deviates) <- n
  }
  return(normal.deviates)
}

###############################################################################################
#   Testing functions
###############################################################################################
#
#   All tests are implemented for the function general.rnorm, since this includes my.rnorm as a special case.
#   Tests are named test.***
#
#   Available tests:
#     test.simple - test the class and length
#     test.graphical - print a histogram and a normal QQ plot
#     test.shapiro - test for normality
#     test.mean - test the mean
#     test.sd - test the standard deviation

generate.input <- function(m, mean.range = c(-1000,1000), sd.range = c(0,1000)) {
  #
  #   Purpose:
  #     Create a dataframe with m rows with columns for n, mean, sd and method.
  #     This dataframe can then be used in the testing functions.
  #
  #   Input:
  #     m - integer > 0. number of desired sets of input (n, mean, sd, and method)
  #     mean.range - vector containing 2 numerics with mean.range[1] <= mean.range[2].
  #       range from which the means will be randomly chosen from.
  #     sd.range - vector containing 2 numerics with sd.range[1] <= sd.range[2] and both >= 0.
  #       range from which the standard deviations will be randomly chosen from.
  #
  # generate the required values from discrete/continous uniform distributions
  ns <- sample(3:5000, m)
  means <- runif(m, min = mean.range[1], max = mean.range[2])
  sds <- runif(m, min = sd.range[1], max = sd.range[2])
  methods <- sample(1:3, size = m, replace = TRUE)
  
  # combine the vectors in a dataframe and name the columns
  df <- data.frame(ns, means, sds, methods)
  names(df) <- c("n", "mean", "sd", "method")
  
  return(df)
}


test.simple <- function(m = 100) {
  #
  #   Purpose:
  #     Checks that the function general.rnorm returns a vector of the right length of class numeric.
  #     Returns TRUE or FALSE.
  #
  #   Input:
  #     m - integer > 0. Number of times to run the test.
  #
  
  # generate input values for the general.rnorm function
  my.input <- generate.input(m)
  
  # calculate the random deviates with the function general.rnorm
  my.deviates <- apply(my.input, 1, function(x) general.rnorm(x[1], x[2], x[3], x[4]))
  
  # check that all vectors are numeric and of the right length
  is.num <- all(sapply(my.deviates, is.numeric))
  right.length <- all(sapply(my.deviates, length) == my.input$n)
  
  return(is.num&&right.length)
}


test.graphical <- function(n = 1000, mean = 0, sd = 1, method = 1) {
  #
  #   Purpose:
  #     Create a histogram and a normal QQ plot for deviates generated by general.rnorm
  #
  #   Input:
  #     n, mean, sd and method as in the function general.rnorm
  #     
  
  my.deviates <- general.rnorm(n, mean, sd, method)
  
  par(mfrow = c(1,2))
  hist(my.deviates)
  qqnorm(my.deviates)
  qqline(my.deviates)
  
  par(mfrow = c(1,1))
  
  return()
}

test.shapiro <- function(m = 100) {
  #
  #   Purpose:
  #     Peforms the Shapiro-Wilk test of normality on a list of m sets of deviates generated by general.rnorm
  #     Prints a histogram of the p-values and returns the ratio of how many of the p-values are below 0.05.
  #     This ratio should be around 0.05, if the function general.rnorm really does produce normal deviates.
  #
  #   Input:
  #     m - integer > 0. Number of times to run the test.
  
  # generate input values for the general.rnorm function
  my.input <- generate.input(m)
  
  # calculate the random deviates with the function general.rnorm
  my.deviates <- apply(my.input, 1, function(x) general.rnorm(x[1], x[2], x[3], x[4]))
  
  # apply shapiro.test to all vectors in the list and save only the p-value
  p.values <- sapply(my.deviates, function(x) shapiro.test(x)$p.value)
  
  hist(p.values)
  p.lessthan0.05 <- sum(p.values <= 0.05)/m
  
  return(p.lessthan0.05)
}



test.mean <- function(m = 100) {
  #
  #   Purpose:
  #     Performs a t-test on the generated numbers to test that the generated numbers have mean "mean".
  #     Prints a histogram of the p-values and returns the ratio of how many of the p-values are below 0.05.
  #     This ratio should be around 0.05, if the function general.rnorm really does produce deviates with mean "mean".
  #   Input:
  #     m - integer > 0. Number of times to run the test.
  #
  
  # generate input values for the general.rnorm function
  my.input <- generate.input(m)
  
  # generate sets of random deviates with gerneral.rnorm and perform a t-test on every set of them
  p.values <- apply(my.input, 1, function(x) (t.test(general.rnorm(x[1], x[2], x[3], x[4]), mu = x[2]))$p.value)
  
  hist(p.values)
  p.lessthan0.05 <- sum(p.values <= 0.05)/m
  
  return(p.lessthan0.05)
}


test.sd <- function(m = 100) {
  #
  #   Purpose:
  #     Performs an F-test to compare the variances of the numbers generated by general.rnorm and the R function rnorm.
  #     
  #     Prints a histogram of the p-values and returns the ratio of how many of the p-values are below 0.05.
  #     This ratio should be around 0.05, if the function general.rnorm really does produce deviates with the same variance as rnorm (given the same sd as argument).
  #   Input:
  #     m - integer > 0. Number of times to run the test.
  #
  
  # generate input values for the general.rnorm function
  my.input <- generate.input(m)
  
  # generate sets of random deviates with general.rnorm and rnorm and perform an F-test on every set of them
  p.values <- apply(my.input, 1, function(x) (var.test(general.rnorm(x[1], x[2], x[3], x[4]), rnorm(x[1], x[2], x[3])))$p.value)
  
  hist(p.values)
  p.lessthan0.05 <- sum(p.values <= 0.05)/m
  
  return(p.lessthan0.05)
}
