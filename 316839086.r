#####################################
# MT4113 - Programming Assignment 1 #
#####################################
# By EMD, for MT4113 at the University of St. Andrews.
# I confirm that the following is my own work, except where clearly indicated in the text.
# (Here, this is only the is.wholenumber function referenced in the comments)

# Overview:
# These functions recreate R's own function rnorm, generating pseudo-random draws
# from a pseudo-Normal distribution, by transforming deviates from a uniform 
# distribution. Three methods are offered.

# Usage Examples:
# x <- my.rnorm(n = 20, mean = 100, sd = 10)
# y <- general.rnorm(n = 20, mean = 100, sd = 10, method = 1)

# Both of the above simulates twenty random draws from a normal distribution of mean 100 
# and standard deviation 10 using the Marsaglia-Bray algorithm.

# Arguments:
# n = number of observations (If length(n) > 1, the length is taken to be the 
#     number required in the same manner as R's own rnorm function number.)
#     ~~~~~ But is this necessary?
# mean = mean(s) of the normal distribution(s). ~~~~~ Can take a vector.
# sd = standard deviation(s) (NOTE - NOT VARIANCE). ~~~~~ Can also take a vector.
# method = 1: Marsaglia and Bray's Method
#          2: Box-Mueller Method
#          3: Central Limit Therem

# Programming notes:
# Where there was a choice between accomplishing something succintly (say, on one line) and
# making things clear to a reader, clarity was prioritised. 


# This first function uses only Marsaglia and Bray's Method. 
my.rnorm <- function(n, mean = 0, sd = 1)
{
  # VALIDATES ARGUMENT FOR N
  # Checks that n exists
  if(missing(n)) stop("invalid arguments")
  # Checks that n is a numeric vector of length one (as per assignment specs, no need to accept vectors)
  if(!is.numeric(n) || length(n) != 1) stop("invalid arguments")
  # Checks that n is a whole number.
    ## NOTE that this modifies the "is.wholenumber" custom function/workaround described
    ## at the bottom of R's helpfile for the is.integer function.
  if(abs(n - round(n)) >= .Machine$double.eps^0.5) stop("invalid arguments")
  # Checks that n is >= 1
  if(n < 1) stop("invalid arguments")
  
  # VALIDATES ARGUMENT FOR MEAN
  # Checks that mean is a numeric vector of length one (again, as per specs)
  if(!is.numeric(mean) || length(mean) != 1) stop("invalid arguments")
  
  # VALIDATES ARGUMENT FOR STANDARD DEVIATION
  # Checks that the standard deviation is a single, positive number
  if(!is.numeric(sd) || length(sd) != 1) stop("invalid arguments")
  if(sd <= 0) stop("invalid arguments")
  
  # Initialises a vector to store the results
  x <- NULL
  
  # For as few pairs as needed (n/2 if n is even, otherwise (n+1)/2 is n is odd)
  for(i in 1:ceiling(n/2))
  {
    # Generates a pair of numbers from the "unit square"
    u <- runif(n = 2, min = -1, max = 1)
    
    # Rejects until the pair indicates a point within the unit circle.
    while(sum(u^2) > 1) u <- runif(n = 2, min = -1, max = 1)
    
    # Transforms the pair of uniform deviates to a pair of standard normal deviates
    w <- sum(u^2)
    v <- sqrt(((-2)*log(w))/w)
    
    # Assigns these standard normal deviates to the results vector
    x[(2*i)-1] <- u[1]*v
    x[2*i] <- u[2]*v
  }
  
  # Transforms the standard normal deviates to have the required mean and standard deviation
  x <- sd*x + mean

  # Trims the extra deviate from the last pair, if an odd number of observations is required.
  length(x) <- n

  # Returns vector of normally distributed deviates
  return(x)
}


# General function with more options for methods
general.rnorm <- function(n, mean = 0, sd = 1, method = 1)
{
  # VALIDATES ARGUMENT FOR N
  # Checks that n exists
  if(missing(n)) stop("invalid arguments")
  # Checks that n is a numeric vector of length one (specs say no need to accept longer vectors)
  if(!is.numeric(n) || length(n) != 1) stop("invalid arguments")
  # Checks that n is a whole number.
    ## NOTE that this modifies the "is.wholenumber" custom function/workaround described
    ## at the bottom of R's helpfile for the is.integer function.
  if(abs(n - round(n)) >= .Machine$double.eps^0.5) stop("invalid arguments")
  # Checks that n is >= 1
  if(n < 1) stop("invalid arguments")
  
  # VALIDATES ARGUMENT FOR MEAN
  # Checks that mean is a numeric vector of length one
  if(!is.numeric(mean) || length(mean) != 1) stop("invalid arguments")
  
  # VALIDATES ARGUMENT FOR STANDARD DEVIATION
  # Checks that the standard deviation is a single, positive number
  if(!is.numeric(sd) || length(sd) != 1) stop("invalid arguments")
  if(sd <= 0) stop("invalid arguments")
  
  # VALIDATES ARGUMENT FOR METHOD
  # Checks that the methos is either 1, 2, or 3, nothing else. 
  if(!any(method == c(1, 2, 3))) stop("invalid arguments")
  
  # Initialises a vector to store the results
  x <- NULL
  
  # Marsaglia and Bray's Method
  if(method == 1)
  {
    # For as few pairs as needed (n/2 if n is even, otherwise (n+1)/2 is n is odd)
    for(i in 1:ceiling(n/2))
    {
      # Generates a pair of numbers from the "unit square"
      u <- runif(n = 2, min = -1, max = 1)
      
      # Rejects until the pair indicates a point within the unit circle.
      while(sum(u^2) > 1) u <- runif(n = 2, min = -1, max = 1)
      
      # Transforms the pair of uniform deviates to a pair of standard normal deviates,
      # according to the algorithm
      w <- sum(u^2)
      v <- sqrt(((-2)*log(w))/w)
      
      # Assigns these standard normal deviates to the results vector
      x[(2*i)-1] <- u[1]*v
      x[2*i] <- u[2]*v
    }
    
    # Trims the extra deviate from the last pair, if an odd number of observations is required.
    length(x) <- n
  }
 
  # Box-Mueller Algorithm
  if(method == 2)
  {
    # For as few pairs as needed (n/2 if n is even, otherwise (n+1)/2 is n is odd)
    for(i in 1:ceiling(n/2))
    {
      # Generates a pair of uniform deviates
      u <- runif(n = 2, min = 0, max = 1)
      
      # Transforms the pair of uniform deviates to a pair of standard normal deviates,
      # according to the algorithm
      x[(2*i)-1] <- sin(2*pi*u[1])*sqrt( (-2)*log(u[2]) )
      x[2*i] <- cos(2*pi*u[1])*sqrt( (-2)*log(u[2]) )
    }
    
    # Trims the extra deviate from the last pair, if an odd number of observations is required.
    length(x) <- n
  }

  # Central Limit Theorem Algorithm
  if(method == 3)
  {
    for(i in 1:n)
    {
      # Generates 16 standard uniform deviates
      u <- runif(n = 16, min = 0, max = 1)
      
      # Transforms the uniform deviates to a standard normal deviate,
      # according to the algorithm
      x[i] <- (sum(u)-8) * sqrt(12/16)
    }
  }
  
  # Transforms the standard normal deviates (from any method) to the required mean and standard deviation
  x <- sd*x + mean
  
  # Returns vector of normally distributed deviates
  return(x)
}

# TESTING FUNCTION
# Testing function to be handed a vector of normal deviates, and the supposed mean, sd, and length (n) for 
# that vector. This only tests the output of the functions, it doesn't check anything about the running
# of the functions themselves (like runtime, memory usage, or ability to deal with crappy arguments)

# I chose not to test bad input here, as this will only be used for testing purposes. 

# If the testing function doesn't throw an error, it produces a histogram of the vector, comparing it to
# a histogram of R's own rnorm output for the same arguments, so that the user can visually check.
test <- function(x, mean, sd, n)
{
  if(!is.numeric(x)) stop("Vector not numeric")
  if(is.null(x) || is.na(x)) stop("Vector empty/invalid")
  if(length(x) != n) stop("Vector has wrong length")
  
  # For a vector of at least some length (chosen kinda arbitrarily), tests properties of vector
  if(n > 9)
  {
    if(abs(mean - mean(x)) > sd) stop("Mean incorrect OR unlikely draw")
    # Thresholds here chosen super arbitrarily, erring on the side of less sensitive
    # If I spent more time here, I'd do something proportional to n.
    if(abs(sd - sd(x)) > (sd)) stop("Standard deviation incorrect OR unlikely draw")
  }
  
  # Makes some pretty histograms, w/o titles, cos we're only testing.
  # Sorry if this mucks up your plot windorw. Run: par(mfrow=c(1,1)) to fix. 
  par(mfrow=c(2,1))
  hist(x)
  hist(rnorm(n = n, mean = mean, sd = sd))
  
  return(TRUE)
}
