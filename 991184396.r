####################################################################################
# DECLARATION
# I confirm that the attached is my own work, except where clearly indicated in 
# the text.  
####################################################################################


####################################################################################
# marsaglia.bray
####################################################################################
# DESCRIPTION
# Function outputs pseudo-normally distributed random values using the Marsaglia
# & Bray algorithm (Maindonald, 1984).
#
# USAGE
# marsaglia.bray()
#
# VALUE
# A vector containing a pair of normally distributed random deviates with
# mean = 0 and standard deviation = 1
####################################################################################
# REFERENCES:
# Maindonald, J. (1984). Statistical Computation. John Wiley & Sons.
####################################################################################
marsaglia.bray <- function(){
  w <- 2                                     # Set rejection criteria (w) to an arbitrary value > 1
  
  while (w > 1) {
    u1 <- (2 * runif(1)) - 1                 # Create two random U(0,1) deviates and 
    u2 <- (2 * runif(1)) - 1                 # transform to unit square [-1,+1]
     
    w <- (u1^2 + u2^2)                       # Calculate rejection criteria, w. Repeat loop until w <= 1.
  }
  
  v <- sqrt((-2*log(w))/w)                   # Generate factor used for conversion from U to N distribution
  
  x1 <- (u1 * v)                             # Multiply both uniform deviate by v to give pseudo-normal 
  x2 <- (u2 * v)                             # distibuted deviates
 
  return(c(x1,x2))                           # Return vector with pair of normal deviates
}



####################################################################################
# box.mueller()
####################################################################################
# DESCRIPTION
# Function outputs pseudo-normally distributed random values using the Box-Mueller
# algorithm (Jones et al., 2009).
#
# USAGE
# box.mueller()
#
# VALUE
# A vector containing a pair of normally distributed random deviates with
# mean = 0 and standard deviation = 1
####################################################################################
# REFERENCES:
# Jones, O., Maillardet, R. and Robinson, A. 2009. Scientific Programming and 
# Simulation Using R. CRC Press.
####################################################################################
box.mueller <- function(){
  u1 <- runif(1)                             # Generate pair of random uniform deviates between 0 and 1
  u2 <- runif(1)
  
  x1 <- sin(2*pi*u1)*sqrt(-2*log(u2))        # Use Box-Mueller formula to convert to pseudo-normally
  x2 <- cos(2*pi*u1)*sqrt(-2*log(u2))        # distributed deviates
  
  return(c(x1,x2))                           # Return vector with pair of normal deviates
}



####################################################################################
# central.limit
####################################################################################
# DESCRIPTION
# Function outputs pseudo-normally distributed random values using the Central Limit
# Theorem algorithm (Mood et al., 1974)
#
# USAGE
# central.limit()
#
# VALUE
# A vector containing a single normally distributed random deviate with
# mean = 0 and standard deviation = 1
####################################################################################
# REFERENCES:
# Mood, A.M., Graybill, F.A. and Boes, D.C. 1974. Introduction to the Theory of 
# Statistics. 3rd Edition. McGraw Hill.
####################################################################################
central.limit <- function(){
    u <- seq(1,16)                           # Create 16-element vector
    
    for (j in u) {
      u[j] <- runif(1)                       # Iterate through vector and fill with U(0,1) random deviates  
    }
    
    x <- (sum(u) - 8) * sqrt(12/16)          # Apply Central Limit Theorem to generate pseudo-normal deviate
    
    return(x)
}



####################################################################################
# my.rnorm
####################################################################################
# DESCRIPTION
# Wrapper for the marsaglia.bray() function which outputs the requested number of 
# normal deviates (using the Marsaglia & Bray algorithm) with the mean and standard 
# deviation specified by the user
#
# USAGE
# my.rnorm(n, mean=0, sd=1)
#
# ARGUMENTS
# n       - the number of random deviates to be output
# mean    - the mean of the values to be returned (default = 0)
# sd      - the standard deviation of the values to be returned (default = 1)
#
# VALUE
# A vector containing n normally distributed random deviates with mean = mean and 
# standard deviation = sd
####################################################################################
# REFERENCES:
# See marsaglia.bray() function
####################################################################################

my.rnorm <- function(n, mean=0, sd=1) {

#ERROR CATCHING CODE  
    if (missing(n) || !is.numeric(n) || length(n) > 1 || (n < 1) || (n >=1 && (n %% 1 != 0))) {
    stop("invalid arguments", call. = FALSE)       # Exit if n is missing, non-numeric, not a positive integer  
  }                                                # or a vector with > 1 element.
  
  if (!is.numeric(mean) || length(mean) > 1) {
    stop("invalid arguments", call. = FALSE)       # Exit if mean is non-numeric or a vector with > 1 element.
  }
  
  if (!is.numeric(sd) || length(sd) > 1 || sd < 0) {
    stop("invalid arguments", call. = FALSE)       # Exit if standard deviation is non-numeric or a vector
  }                                                # with > 1 element


# FUNCTION TASK  
  result <- vector(mode="numeric", length=0)       # Create empty numeric vector to contain result

  rep <- (n-1) %/% 2                               # Calculate number of pairs of deviates we require
  repeat {
    norm.pair <- (marsaglia.bray() * sd) + mean    # Run marsaglia.bray() and adjust to required mean/sd.
    
    #add normal variates
    result <- append(result, norm.pair)            # Append pair of normal deviates to result vector
    
    if (rep == 0) break                            # Exit repeat loop if no further pairs required 
    rep <- rep - 1                                 # Decrement number of remaining loops by 1
  }
  
  if(n %% 2 == 0){                                 # If user requested even number of normal deviates
    return(result)                                 # output result vector as is
  } else {
    return(result[1:n])                            # otherwise, output the first n elements
  }
}


####################################################################################
# DESCRIPTION
# general.rnorm is a generalized version of my.rnorm, allowing the user to 
# choose the algorithm for generating normally distributed random deviates:
#
# USAGE
# general.rnorm(n, mean=0, sd=1, method=1)
#
# ARGUMENTS
# n       - the number of random deviates to be output
# mean    - the mean of the values to be returned (default = 0)
# sd      - the standard deviation of the values to be returned (default = 1)
# method  - 1 (default) - Marsaglia & Bray 
#           2           - Box & Mueller 
#           3           - Central Limit Theorem algorithm
#
# VALUE
# A vector containing n normally distributed random deviates with mean = mean and 
# standard deviation = sd, according to the chosen method.
####################################################################################
# REFERENCES:
# See marsaglia.bray(), box.mueller() and central.limit() functions
####################################################################################
general.rnorm <- function(n, mean=0, sd=1, method=1) {

#ERROR CATCHING CODE  
    if (missing(n) || !is.numeric(n) || length(n) > 1 || (n < 1) || (n >=1 && (n %% 1 != 0))) {
    stop("invalid arguments", call. = FALSE)       # Exit if n is missing, non-numeric, not a positive integer 
  }                                                # or a vector with > 1 element.

  if (!is.numeric(mean) || length(mean) > 1) {
    stop("invalid arguments", call. = FALSE)       # Exit if mean is non-numeric or a vector with > 1 element.
  }

  if (!is.numeric(sd) || length(sd) > 1 || sd < 0) {
    stop("invalid arguments", call. = FALSE)       # Exit if standard deviation is non-numeric or a vector
  }                                                # with > 1 element

  if (!is.numeric(method) || length(method) > 1) {   # Exit if method is non-numeric or a vector with > 1
    stop("invalid arguments", call.FALSE)          # element
}

  
  
# FUNCTION TASK
  result <- vector(mode="numeric", length=0)          # Create empty vector to contain result
   
  if (method == 1 || method == 2) {
    rep <- (n-1) %/% 2
    repeat {
      if(method == 1){                                 # If user selects Marsaglia & Bray method
        norm.pair <- (marsaglia.bray() * sd) + mean 
      } else if (method == 2){                         # If user selects Box & Mueller algorithm
        norm.pair <- (box.mueller() * sd) + mean       
      }
      
      result <- append(result, norm.pair)              # Append pair of normal deviates to result vector
      
      if (rep == 0) break                              # Exit loop if no more repeats required
      rep <- rep - 1                                   # Decrement number of remaining loops by 1
    }
    
    if(n %% 2 == 0){                                   # If user requested even number of normal deviates
      return(result)                                   # output result vector as is
    } else {
      return(result[1:n])                              # otherwise, output the first n elements
    }
  } else if (method == 3){                             # User selects Central Limit Theorem algorithm
      for (i in 1:n)                                   # Loop through code until we have n values
    {
      norm.value <- central.limit()                    
      result <- append(result, norm.value)             # Append normal deviate to result vector
      }
    
    return(result)
    
  } else {
      stop("invalid arguments", call. = FALSE)         # If method is any value but 1, 2 or 3, exit function
  }
}



#TESTING CODE

################################################################
# hist.rnorm.test
################################################################
# Creates histogram of n normally distributed deviates
# with user-specified mean and standard deviation. 
#
# Overplotted with red line showing same distribution calculated
# using standard R function "rnorm"
#
# Use: hist.my.rnorm(n, mean, sd, 1) to run my.rnorm
#      hist.my.rnorm(n, mean, sd, 2) to run general.rnorm (using Marsaglia/Bray)
################################################################
hist.rnorm.test <- function(n, mean=0, sd=1, fn=1){
  if (fn == 1){                                       
    my.test <- my.rnorm(n,mean,sd)                          
  } else if (fn == 2) {                               
    my.test <- general.rnorm(n,mean,sd,1)                   
  } else stop("invalid arguments", call.=FALSE)
  
  r.norm <- rnorm(n,mean,sd)       
  x <- seq(min(r.norm),max(r.norm),length.out = 1000)
  
  mean.my.test <- mean(my.test)
  sd.my.test <- sd(my.test)
  hist(my.test, main=paste("Histogram of ", n, " deviates with mean=",round(mean.my.test,4)," and std dev=",round(sd.my.test,4)), prob=T)
  lines(x,dnorm(x, mean = mean, sd=sd), lwd=2, col=2)
  return(NULL)
}

################################################################
# check.num.outputs
################################################################
# Returns true if my.rnorm outputs the requested number of 
# deviates

# Usage: 
# check.num.outputs(n)
################################################################
check.num.outputs <- function(n) {
  x <- my.rnorm(n)
  if (length(x) == n & is.numeric(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

################################################################
# check.mean
################################################################
# Returns the exact mean of 10000 normally distributed deviates
# generated using my.rnorm with user-specified mean
################################################################
check.mean <- function(mn=0) {
  my.mean <- my.rnorm(10000,mn,1)  
  return(mean(my.mean))
}

################################################################
# check.sd
################################################################
# Returns the exact standard deviation of 10000 normally 
# distributed deviates generated using my.rnorm with user-
# specified standard deviation
################################################################
check.sd <- function(sdev=1){
  my.sd <- my.rnorm(10000,0,sdev)
  return(sd(my.sd))
}
  