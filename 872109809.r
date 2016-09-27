# I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n, mean = 0, sd = 1) {
#   Purpose: 
#       Get random values from a normal distribution using the Marsaglia and
#       Bray algorithm.
#
#   Input:
#       n - number of values to return
#       mean - mean of values to return (default 0)
#       sd - standard deviation of values to return (default 1)
#
#   Output:
#       Vector of pseudo-random values from a normal distribution.
  if (!is.numeric(n) || !is.numeric(mean) || !is.numeric(sd) || n < 0 || n%%1!=0 || sd <= 0 || length(n) > 1 || length(mean) > 1 || length(sd) > 1){
#   the first three ensure that n, mean and sd are numeric
#   the next two ensure that n is an greater than zero and an integer
#   the sixth ensure that the s.d. is greater than zero (since it can't be negative and it is only 0 if all the data point are equal to the mean)
#   the last three ensure that n, mean and sd are all scalars
    stop("invalid arguments") # error trap on inputs
  }
  if (n == 0){
    return(numeric(0)) # since rnorm(0) returns numeric(0)
  } 
  values <- NULL
  for (i in 1:((n+1)/2)) {
    u <- runif(2) # runif() creates uniformly distributed variables
    u.<- 2*u-1
    w <- (u.[1])^2+(u.[2])^2
    while (w > 1) { # rejection step
      u <- runif(2)
      u. <- 2*u-1
      w <- (u.[1])^2+(u.[2])^2
    }
    v <- sqrt(((-2)*log(w))/w)
    x <- u.*v
    values <- c(values, x)
  }
  if (length(values) > n) { # get as many values as requested by n, despite having pairs
    values <- values[1:n]
  }
  values <- values*sd+mean
  return(values)
}

general.rnorm <- function(n, mean = 0, sd = 1, method = 1) {
#   Purpose: 
#       Get random values from a normal distribution by choosing which algorithm to use.
#       Choose between the Marsaglia and Bray algorithm, the Box and Mueller algorithm
#       or the Central Limit Theorem algorithm.
#
#   Input:
#       n - number of values to return
#       mean - mean of values to return (default 0)
#       sd - standard deviation of values to return (default 1)
#       method - an integer value corresponding to a specific algorithm:
#                1 corresponds to Marsaglia and Bray algorithm (default)
#                2 corresponds to Box and Mueller algorithm
#                3 corresponds to Central Limit Theorem algorithm
#  
#   Output:
#       vector of pseudo-random values from a normal distribution
  if (!is.numeric(n) || !is.numeric(mean) || !is.numeric(sd) || n < 0 || n%%1!=0 || sd <= 0 || length(n) > 1 || length(mean) > 1 || length(sd) > 1){
    stop("invalid arguments") # error trap on inputs
  }
  if (n == 0){
    return(numeric(0))
  }  
  if (method == 1){
    my.rnorm(n,mean,sd)
  } else if (method == 2){
      values <- NULL
      for (i in 1:((n+1)/2)) {
        u <- runif(2)
        x1 <- sin(2*pi*u[1])*sqrt((-2)*log(u[2]))
        x2 <- cos(2*pi*u[1])*sqrt((-2)*log(u[2]))
        values<-c(values, x1, x2)
      }
      if (length(values) > n) {
        values <- values[1:n]
      }
      values <- values*sd+mean
      return(values)
  } else if (method == 3){
      values <- NULL
      for (i in 1:n) {
        u <- runif(16)
        x <- (sum(u)-8)*sqrt(12/16)
        values <- c(values, x)
      }
      values <- values*sd+mean
      return(values)
  } else
      stop("invalid arguments")
}

test1 <- function(x) {
#   Purpose: 
#       Test whether the values produced by my.rnorm or general.rnorm follow a normal
#       distribution using the Shapiro-Wilks test.
#       H0 = data normally distributed
#       We test this at the 0.05 significance level.
#
#   Input:
#       x - data wishing to find out whether they are normally distributed
#  
#   Output:
#       String stating whether we reject or do not reject H0.
#       When testing my.rnorm & general.rnorm we would expect "Do not reject H0".
#
#   References:  
#       http://www.dummies.com/programming/r/how-to-test-data-normality-in-a-formal-way-in-r/
  sw <- shapiro.test(x) 
  pval <- sw$p.value # shapiro.test() returns a list object, of which we only need the p-value
  if (pval < 0.05) {
    print("Reject H0")
  }
  else {
    print("Do not reject H0")
  }
}

test2 <- function(x) {
#   Purpose: 
#       Test whether the values produced by my.rnorm or general.rnorm follow a normal
#       distribution using a histogram plot. We have to make our own inferences from
#       the histogram.
#
#   Input:
#       x - data we are wishing to find out whether they are normally distributed
#  
#   Output:
#       Histogram of data with a line on top using the mean and sd of the data.
#       If data is normally distributed, we expect to get a histogram that is
#       approximately bell-shaped and a bell-shaped curve agreeing with the histogram.
  h <- hist(x)
  xfit <- seq(min(x), max(x), length=100)
  yfit <- dnorm(xfit, mean=mean(x), sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2]*length(x))
  lines(xfit, yfit, col="turquoise4", lwd=2)
}

test3 <- function(x) {
#   Purpose: 
#       Test whether the values produced by my.rnorm or general.rnorm follow a normal
#       distribution using a Q-Q plot. We have to make our own inferences from the plot.
#
#   Input:
#       x - data we are wishing to find out whether they are normally distributed
#  
#   Output:
#       Q-Q Plot of data with a line on top. If data is normally distributed, we expect
#       to get a straight line and not many significant outliers.
  qqnorm(x)
  qqline(x)
}

test4 <- function(n, mean, sd, method) {
#   Purpose: 
#       Create a confidence interval for the mean used as an imput to the test (and 
#       hence in general.rnorm) and check whether the actual mean of the values produced
#       by the function lies within that confidence interval.
#
#   Input:
#       input is the same as for general.rnorm
#  
#   Output:
#       String stating whether the mean is in the confidence interval calculated or not.
#       We would expect the mean to lie within the confidence interval approximately
#       95/100 times since a 95% confidence interval is used.
  x <- general.rnorm(n, mean, sd, method)
  y <- mean(x)
  ci.l <- mean - qnorm(0.975)*sd/sqrt(n) # calculating lower
  ci.u <- mean + qnorm(0.975)*sd/sqrt(n) # and upper bounds of the confidence interval
  if (y > ci.l & y < ci.u){ # condition that y lies in the confidence interval
    return("mean in confidence interval")
  }
  else {
    return("mean not in confidence interval")
  }
}