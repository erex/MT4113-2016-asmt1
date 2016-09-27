######################## ASSIGNMENT 1 ########################
#I confirm that the attached is my own work, except where clearly indicated in the text
#NOTE: all the algorithms used in this assignment are referred to from 'Assignment 1'
#from the moodle website under the heading 'Projects and Assignments'
#

#FUNCTIONS

my.rnorm <- function(n = NULL, mean = 0, sd = 1) {
    
# Purpose:  Generate a vector of n pseudo-random values with
#           mean and standard deviation required using
#           Marsaglia and Bray's Algorithm (Maindonald 1984)
#
# Input:    n     <- number of pseudo-random values
#           mean  <- mean of the values desired (default = 0)
#           sd    <- standard deviation (default = 1)
#
# Output:   Vector of n pseudo-random values with mean and
#           standard deviation from the input
#
  
  if (!is.numeric(n) || !is.numeric(mean) || !is.numeric(sd)) {
    stop("invalid arguments")
  }
  #returns error if any argument is not numeric
  
  if (length(n) != 1 || length(mean) != 1 || length(sd) != 1) {
    stop("invalid arguments")
  }
  #returns error if any argument is not a scalar
  
  if (n < 0 || is.null(n) || sd < 0) {
    stop("invalid arguments")
  }
  #returns error n is negative or NULL / standard deviation is negative
  
  if (n == 0) {
    return(numeric(0))
  }
  #returns same value as rnorm(0)
  
  xf <- c()
  for (i in seq(1:ceiling(n/2))) {
    w <- 2
    while (w > 1) {
      u <- runif(n = 2, min = 0, max = 1)
      u <- 2 * u - 1
      w <- u[1]^2 + u[2]^2
    }
    v <- sqrt((-2 * log(w)) / w)
    x <- u * v
    xf <- c(xf,x)
  }
  
  if (n %% 2 != 0) {
    xf <- xf[-n]
  }
  #removes last value in return vector if n is odd
  
  xf <- xf * sd + mean
  #tranform to values to sd and mean from the input 
  
  return(xf)
}


general.rnorm <- function(n = NULL, mean = 0, sd = 1, method = 1) {
  
# Purpose:  generate a vector of n pseudo-random values with
#           mean and standard deviation required using
#           one of three algorithms
#
# Input:    n       <- number of pseudo-random values  
#           mean    <- mean of the values desired (default = 0)
#           sd      <- standard deviation (default = 1)
#           method  <- number of algortihm wanted to generate the values
#                       1 <- Marsaglia and Bray's Method (Maindonald 1984)
#                       2 <- Box-Mueller algorithm. Jones et al. (2009)
#                       3 <- Central Limit Theorem. Mood et al. (1974)
#
# Output:   vector of n pseudo-random values with mean and
#           standard deviation and required algorithm from the input

  if (!is.numeric(n) || !is.numeric(mean) || !is.numeric(sd) || !is.numeric(method)) {
    stop("invalid arguments")
  }
  
  if (length(n) != 1 || length(mean) != 1 || length(sd) != 1 || length(method) != 1) {
    stop("invalid arguments")
  }
  
  if (method < 1 || method > 3 || method %% 1 != 0) {
    stop("invalid arguments")
  }
  #returns error if method is not 1, 2 or 3
  
  if (n < 0 || is.null(n) || sd < 0) {
    stop("invalid arguments")
  }
  
  if (n == 0) {
    return(numeric(0))
  }
  
  xf <- c()
  if (method == 1) {
    
  #Marsaglia and Bray's Method (Maindonald 1984)
    
    xf <- my.rnorm(n, mean, sd)
  }
  
  if (method == 2) {
    
  #Box-Mueller algorithm. Jones et al. (2009)
   
     for (i in seq(1:ceiling(n/2))) {
      u <- runif(n=2, min = 0, max = 1)
      x1 <- sin(2 * pi * u[1]) * sqrt(-2 * log(u[2]))
      x2 <- cos(2 * pi * u[1]) * sqrt(-2 * log(u[2]))
      x <- c(x1, x2)
      xf <- c(xf, x)
    } 
    if (n %% 2 != 0) {
        xf <- xf[-n]
        #removes last value in return vector if n is odd
    }
  }
  
  if (method == 3) {
    
  #Central Limit Theorem. Mood et al. (1974)
    
    for (i in seq(1:n)) {
      x <- ((sum(runif(n = 16, min = 0, max = 1)) - 8) * sqrt(12 / 16))
      xf <- c(xf, x)
    }
  }
  
  xf <- xf * sd + mean
  #tranform to values to sd and mean from the input
  
  return(xf)
}


#TESTS

#For all tests (except test5) below (same purpose/input/output)
# Purpose:  Test the inputs of the general.rnorm function
#           and check the out is correctly an error
# 
# Input:    N/A
# 
# Output:   Always an error "invalid arguments"
#

test1 <- function() {
  
  a <- runif(n = 1, min = -1000, max = 0)
  general.rnorm(n = a)
  #generate a random negative number and input into n 
  #of general.rnorm, returns error
  
}

test2 <- function() {
  b <- runif(n = 1, min = 1, max = 10)
  c <- runif(n = 1, min = -1000, max = 1000)
  general.rnorm(n = b, sd = c)
  #generate a random negative number and input into sd 
  #of general.rnorm, returns error
}

test3 <- function() {
  general.rnorm()
  #general.rnorm for NULL n, returns error
}

test4 <- function() {
  b <- runif(n = 1, min = 1, max = 10)
  general.rnorm(n = b, method = b)
  #generate a random negative number and input into method 
  #of general.rnorm, returns error
}

test5 <- function() {
  general.rnorm(n = 0) 
  #returns numeric 0 like rnorm(0)
}