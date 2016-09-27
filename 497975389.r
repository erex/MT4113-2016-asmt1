# I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n, mean = 0, sd = 1) { # setting arguments and defaults
#   Purpose:   
# Returns a vector of pseudo-random values from a normal distribution using the Marsaglia and Bray algorithm
# 
#   Input:
# n - the number of random values desired
# mean - mean of values to return, default = 0
# sd - standard deviation of values to return, default = 1
#
#   Output:
# A vector of n random variables from normal distribution N(mean, sd^2)
#
#   Reference:
# Maindonald, John. 1984. Statistical Computation. John Wiley and Sons.
  
  if (n < 1 | is.numeric(n) == FALSE) { # error trapping
    stop("invalid arguments")
  } 
  if (sd < 0 | is.numeric(sd) == FALSE | is.numeric(mean) == FALSE) {
    stop("invalid arguments")
  }
  results = NULL
  while (length(results) < n) {
    w = 2 # initialising w at a value large enough to start the while loop
    while (w > 1) {
      u <- runif(2,min = 0, max = 1) # generate random uniform values
      u <- 2*u - 1
      w = u[1]^2 + u[2]^2
    }
    v = sqrt(-2*log(w)/w)
    x <- u*v
    results <- c(results,x)   # store all random normal deviates, do while length(x) < n 
  }
  if (n %% 2 != 0) {
    results <- results[1:n] # for odd n, creating a n length deviates vector from a vector length of even length, n+1
  }
  results <- results*sd + mean # transforming N(0,1) deviates to N(mean, sd^2) deviates
  return(results)
} 

box.mueller <- function(n, mean = 0, sd = 1) {
#   Purpose:
# Returns a vector of pseudo-random values from a normal distribution using the Box and meaneller algorithm
#
#   Input:
# n - the number of random values desired
# mean - mean of values to return, default = 0
# sd - standard deviation of values to return, default = 1
#
#   Output:
# A vector of n random variables from normal distribution N(mean, sd^2)
#
#   Reference:
# Jones, O., R. Maillardet, and A. Robinson. 2009. Scientific Programming and Simeanlation Using R. CRC Press.
  
  if (n < 1 | is.numeric(n) == FALSE) { # error trapping
    stop("invalid arguments")
  } 
  if (sd < 0 | is.numeric(sd) == FALSE | is.numeric(mean) == FALSE) {
    stop("invalid arguments")
  }
  results = NULL
  while (length(results) < n) { # performing the Box and meaneller algorithm
    u <- runif(2, min = 0, max = 1)
    x <- NULL
    x[1] <- sin(2*pi*u[1])*sqrt(-2*log(u[2]))
    x[2] <- cos(2*pi*u[1])*sqrt(-2*log(u[2]))
    results <- c(results,x)
  }
  if (n %% 2 != 0) {
    results <- results[1:n] #for odd n, creating a n length deviates vector for vector length n+1
  }
  results <- results*sd + mean #transforming N(0,1) deviates to N(mean, sd^2) deviates
  return(results)
}

central.limit <- function(n, mean = 0, sd = 1) {
#   Purpose:
# Returns a vector of pseudo-random values from a normal distribution using the central-limit theorem algorithm
#
#   Input:
# n - the number of random values desired
# mean - mean of values to return, default = 0
# sd - standard deviation of values to return, default = 1
#
#   Output:
# A vector of n random variables from normal distribution N(mean, sd^2)
#  
#   Reference:
# Mood, A.M., F.A. Graybill, and D.C. Boes. 1974. Introduction to the Theory of Statistics. Third Edition. McGraw Hill.

  if (n < 1 | is.numeric(n) == FALSE) { # error trapping
    stop("invalid arguments")
  } 
  if (sd < 0 | is.numeric(sd) == FALSE | is.numeric(mean) == FALSE) {
    stop("invalid arguments")
  }
  x = NULL
  for (i in 1:n) { # performing central limit theorem algorithm
    x[i] <- (sum(runif(16,0,1))-8)*sqrt(12/16)
  }
  results <- x*sd + mean #transforming to desired N(mean, sd^2) distribution
  return(results)
}

general.rnorm <- function(n, mean = 0, sd = 1, method = 1) {
#   Purpose:
# Returns a vector of pseudo-random values from a normal distribution using a chosen algorithm from three
# 
#   Input:
# n - the number of random values desired
# mean - mean of values to return, default = 0
# sd - standard deviation of values to return, default = 1
# method - which algorithm method use to generate the values(Marsaglia and Bray = 1, Box and meaneller = 2, central-limit theorem = 3), default = 1
# 
#   Output:
# A vector of n random variables from normal distribution N(mean, sd^2)

  if (method == 1) { # determine which method to use
    results <- my.rnorm(n, mean, sd) 
  } else if (method == 2) {
    results <- box.mueller(n, mean, sd)
  } else if (method == 3){
    results <- central.limit(n, mean, sd)
  } else {  
    stop("invalid arguments")  # error trapping
  }
  return(results)
}

# testing functions

pass.test <- function(n = 10, mean = 0, sd = 1, method = 1) {
  #   Purpose:   
  # Checks whether function general.rnorm produces correct number of value and that values are numbers.
  # 
  #   Input:
  # n - the number of random values desired, default = 10
  # mean - mean of values to return, default = 0
  # sd - standard deviation of values to return, default = 1
  # method - which algorithm method to use for generation of the random values, default = 1
  #
  #   Output:
  # A statement of TRUE or FALSE, TRUE indicating that the test passed successfully.

  x <- general.rnorm(n, mean, sd, method)
  pass.test <- (length(x) == n & is.numeric(x)) 
  return(pass.test)
}

ks.D.pvalue <- function(n, mean = 0, sd = 1, method = 1, iterations = 1000) {
  #   Purpose:   
  # Stores the KS test statistic and p-values for the number of iterations specified, for H0 = the random values follow a Normal distribution
  # 
  #   Input:
  # n - the number of random values desired
  # mean - mean of values to return, default = 0
  # sd - standard deviation of values to return, default = 1
  # method - which algorithm method to use for generation of the random values, default = 1
  # iterations - the number of random samples to be regenerated, default = 1000
  #
  #   Output:
  # A list of 2, one vector called D contained the stored ks test statistics for each iteration and the other the p-values
  D <- rep(0, iterations) # initialising test statistic and p-value vectors to be filled in for-loop
  pval <- rep(0, iterations)
  for (i in 1:iterations) { # for loop to generate given number of interations and store their KS test statistics and p-value
    x <- general.rnorm(n, mean, sd, method)
    D[i] <- as.numeric(ks.test(x, "pnorm", mean, sd)[[1]])
    pval[i] <- as.numeric(ks.test(x,"pnorm", mean , sd)[[2]])
  }
  return(list(D = D, p.value = pval))
}

ks.histograms <- function(n, mean = 0, sd = 1, method = 1, iterations = 1000) {
  #   Purpose:   
  # Uses the ks.D.pvalue function and produces histograms for both the D and pvalue vectors
  # 
  #   Input:
  # n - the number of random values desired
  # mean - mean of values to return, default = 0
  # sd - standard deviation of values to return, default = 1
  # method - which algorithm method to use for generation of the random values
  # iterations - the number of random samples to be regenerted
  #
  #   Output:
  # Two histograms showing the distribution of the simeanlated sample test statistics D and p-value
  test <- ks.D.pvalue(n)
  par(mfrow=c(1,2))
  D <- hist(test$D, main="Histogram of test stats")
  p <- hist(test$p.value, main = "Histogram of p-values")
  return(list(D, p))
}

ks.confint <- function(n, mean = 0, sd = 1, method = 1, iterations = 1000) {
  #   Purpose:   
  # Returns 5% and 95% levels for the p-values vector, to reflect the type A error of the ks test.
  # If the value is near the quantile value, then shows accuracy of test sample.
  # (eg. if 5% quantile is near 0.05, shows that 5% of the time when normally distributed, would reject the null that the random values follow a Normal distribution)
  # If the deviates are normally distributed, then as iterations and n tend to infinity, the value will tend to the quantile level.
  # 
  #   Input:
  # n - the number of random values desired
  # mean - mean of values to return, default = 0
  # sd - standard deviation of values to return, default = 1
  # method - which algorithm method to use for generation of the random values
  # iterations - the number of random samples to be regenerted
  #
  #   Output:
  # Quantile values for 5% and 95% quantiles
  test <- ks.D.pvalue(n)
  return(quantile(test$p.value, probs = c(0.05, 0.95)))
}
