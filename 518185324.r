# I confirm that the attached is my own work, except where clearly indicated in the text

# Assignment 1: Random Number Generation
# Functions that use the uniform random number generator in R to generate random normal deviates


# function valid_arguments

valid_arguments <- function(n, mean, sd){
  
  # Purpose: Check whether inputs are suitable values for no. of variates, 
  # mean and standard deviation
  
  # Inputs: n- number of variates, mean - mean, sd - standard deviation
  
  # Output: Boolean - TRUE if arguments all valid, FALSE o/w
  
  if (!is.numeric(n) | !is.numeric(mean) | !is.numeric(sd)){
    return(FALSE)
  } else if (!(length(n)==1) | !(length(mean)==1) | !(length(sd)==1)){
    return(FALSE)
  }else if (!(n>0 & n%%1==0 & sd>0)){
    return(FALSE)
  } else return(TRUE)
}
  

#===================================================================================

# function my.rnorm

my.rnorm <- function(n, mean=0, sd=1){
  
# Purpose: Returns vector of n pseudo-random values from a N(mean,sd) distribution using the 
# Marsaglia and Bray algorithm.  Stops if exceeds 1000 iterations
  
  # Inputs: n -- number of observations (length of vector required)
  #         mean -- mean of distribution
  #         sd -- standard deviation of distribution
  
  # Output: normal_variates -- vector of length n containing pseudo-random values from 
  #                            N(mean,sd) distribution
  
  # Reference:  Maindonald, John. 1984. Statistical Computation. John Wiley and Sons.
  
  if (!valid_arguments(n=n, mean=mean, sd=sd)){ # Checks validity of arguments
    stop("invalid arguments")
  }
  normal_variates <- numeric(length=n)
  for (i in seq(1, n, by=2)){         # loop which cycles through length of normal_variates
    count <- 0                        # generating a pair of N(0,1) variates using the M-B
    repeat {                          # algorithm
      uniform_deviates <- runif(2, 0, 1)
      uniform_deviates <- 2 * uniform_deviates - 1
      w <- (uniform_deviates[1])^2 + (uniform_deviates[2])^2
      count <- count + 1
      if (w<1) break # Breaks loop if w<1 condition for M-B algorithm satisfied
      if (count>1000) stop("1000 iteration limit exceeded")
    }
    v <- sqrt(-2 * log(w)/w)
    if (i == length(normal_variates)) { # Only adds one variate in last position if odd length
      normal_variates[i] <- uniform_deviates[1] * v
    } else {
      normal_variates[c(i,i+1)] <- uniform_deviates * v # adds two generated variates to vector
    }
  }
  normal_variates <- normal_variates * sd + mean # converts variates to N(mean, sd) distribution
  normal_variates
}

#=========================================================================================

# function box_mueller

box_mueller <- function(n, mean=0, sd=1){
  
  # Purpose: Returns vector of n pseudo-random values from a N(mean,sd) distribution using the 
  # Box--Mueller algorithm.
  
  # Inputs: n, mean, sd as in my.rnorm
  
  # Output: vector of N(mean,sd) variates of length n
  
  # Reference: Jones, O., R. Maillardet, and A. Robinson. 2009. Scientific Programming and Simulation Using R. CRC Press.
  
  if (!valid_arguments(n=n, mean=mean, sd=sd)){ # Tests validity of arguments
    stop("invalid arguments")
  }
  normal_variates <- numeric(length=n)
  for (i in seq(1, n, by=2)){ # Performs Box--Mueller algorithm to generate N(0,1) variates
      uniform_deviates <- runif(2, 0, 1)
      if (i<n){
        normal_variates[i] <- sin(2*pi*uniform_deviates[1])*sqrt(-2*log(uniform_deviates[2]))
        normal_variates[i+1] <- cos(2*pi*uniform_deviates[1])*sqrt(-2*log(uniform_deviates[2]))
      } else {
        normal_variates[i] <- sin(2*pi*uniform_deviates[1])*sqrt(-2*log(uniform_deviates[2]))
      }
  }
  normal_variates <- normal_variates * sd + mean # converts to N(mean,sd) variates
  return(normal_variates)
}

# function to return a vector of n pseudo-random values from a N(mean, sd) distribution using the
# Central Limit Theorem algorithm

central_limit_theorem <- function(n, mean=0, sd=1){
  
  # Purpose: Returns vector of n pseudo-random values from a N(mean,sd) distribution using the 
  # Central Limit Theorem.
  
  # Inputs: n, mean, sd as in my.rnorm
  
  # Output: vector of length n containing N(mean, sd) variates
  
  # Reference: Mood, A.M., F.A. Graybill, and D.C. Boes. 1974. Introduction to the Theory of Statistics. Third Edition. McGraw Hill.
  
  
   if (!valid_arguments(n=n, mean=mean, sd=sd)){ # Test argument validity
     stop("invalid arguments")
   }
   normal_variates <- numeric(length=n)
   for (i in seq(1,n)){ # Generates 16 uniform variates and uses
     uniform_deviates <- runif(16, 0, 1)
     normal_variates[i] <- (sum(uniform_deviates) - 8) * sqrt(12/16)
   }
   normal_variates <- normal_variates * sd + mean # Converts to N(mean,sd) variates
   normal_variates
}

#==============================================================================

# function general.rnorm

general.rnorm <- function(n, mean=0, sd=1, method=1){
  
  # Purpose: Returns a vector of n pseudo-random values from a N(mean,sd) distribution
  # using one of the three methods above
  
  # Inputs: n, mean, sd as in my.rnorm
  #         method -- 1 corresponds to Marsaglia and Bray's algorithm (default)
  #                   2 corresponds to Box--Mueller method
  #                   3 corresponds to Central Limit Theorem method
  
  # Output: Vector of n values from N(mean,sd) distribution
  
  
  if (identical(method,1)){
    normal_variates <- my.rnorm(n=n, mean=mean, sd=sd)
  } else if (identical(method,2)){
    normal_variates <- box_mueller(n=n, mean=mean, sd=sd)
  } else if (identical(method,3)){
    normal_variates <- central_limit_theorem(n=n, mean=mean, sd=sd)
  } else {
    stop("invalid arguments")
  }
  return(normal_variates)
}



#==================================================
# TESTING
#==================================================

test_pass <- function(n,mean=0,sd=1,method=1){

  # Function to perform tests of generated vector and return logical
  # False if test failed, True if all passed. Prints message on failure
  # Tests length, numeric and normality
  
  normal_variates <- general.rnorm(n=n, mean=mean, sd=sd, method=method)
  # Length Test
  if (!(length(normal_variates)==n)){
    print("Wrong Length")
    return(FALSE)
  } else if (!(is.numeric(normal_variates))){
    print("Non-numeric")
    return(FALSE)
  } else if (n>3 & n<5000){
    if (shapiro.test(normal_variates)$p.value < 0.05){
      print("Significant value")
      return(FALSE)
    } else return(TRUE)
  } else return(TRUE)
}



visual_normality_check <- function(n, mean=0, sd=1, method=1){
  # Plots 3 graphs to allow visual check of data normality
  par(mfrow=c(1,3))
  variates <- general.rnorm(n, mean, sd, method)
  hist(variates)
  qqnorm(variates)
  qqline(rnorm(10000, mean, sd))
  plot(variates)
}

correct_message_n <- function(method=1){
  
  # tests various types of argument for n to check that error encountered
  
  types <- list("string",TRUE,c(1,1),runif)
  for (n in types){
    print(typeof(n))
    count <- tryCatch(general.rnorm(n,mean=0,sd=1,method=method),
                      error=function(cond){
                        message("Error encountered")
                      }
                      )
  }
}

correct_message_mean <- function(method=1){
  
  # tests various types of argument for sd to check that error encountered
  
  types <- list("string",TRUE,c(1,1),runif)
  for (mean in types){
    print(typeof(mean))
    tryCatch(general.rnorm(n=10,mean,sd=1,method=method),
              error=function(cond){
                    message("Error encountered")
                    }
             )
  }
}

correct_message_sd <- function(method=1){
  
  #  tests various types of argument for sd to check that error encountered
  
  types <- list("string",TRUE,c(1,1),runif)
  for (sd in types){
    print(typeof(sd))
    tryCatch(general.rnorm(n=10,mean=0,sd=sd,method=method),
             error=function(cond){
               message("Error encountered")
             }
    )
  }
}
