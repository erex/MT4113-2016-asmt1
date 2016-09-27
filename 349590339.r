#MT_4113 Assignment_1
#ID: 160023669
#I confirm that the attached is my own work, except where clearly indicated in the text

### Marsaglia and Bray algorithm ###

my.rnorm <- function (n, mean = 0, sd = 1) {
  
  if (is.numeric(n) == TRUE) {                # Ensure n is a numeric argument

    if (round(n) == n &                       # Ensure n is a natural number
        n > 0 &                               # ensure n > 0 
        sd >= 0) {                            # ensure sd >= 0
      results <- numeric(n)                   # Creat a vector with length n to save results
      
      if (n%%2 == 0) {                        # Even Case          
        for (i in 1 : n/2) {                  # In even case, generate n/2 pairs of variates
          w <- 2                              # Any w > 1 will go to
          
          while (w > 1) {                     # rejection step 
            U <- runif(2)                 
            U <- U*2 - 1
            w <- U[1]^2 + U[2]^2            
          }                                   # The values in U are in the unit circle
          
          v <- sqrt(-2*log(w)/w)
          X <- U*v                            # X follows the standard normal distribution
          X <- X*sd + mean                    # X follows the normal distribution with mean and sd
          results[i*2 - 1] <- X[1]            # Give values to results vector
          results[i*2] <- X[2]
        }
        return(results)
        
      } else {                                # Odd Case
        for (i in 1 : ((n+1)/2)) {            # In odd case, generate (n+1)/2 pairs of variates
          w <- 2                              # Any w > 1 will go to
          
          while (w > 1) {                     # rejection step
            U <- runif(2)
            U <- U*2 - 1
            w <- U[1]^2 + U[2]^2
          }                                   # The values in U are in the unit circle
          
          v <- sqrt(-2*log(w)/w)
          X <- U*v                            # X follows the standard normal distribution
          X <- X*sd + mean                    # X follows the normal distribution with mean and sd
          results[i*2 - 1] <- X[1]            # Give values to results vector
          results[i*2] <- X[2] 
        }
      }
      results <- results[1:n]                 # Delete one results value before output
      return(results)
    } else {
      stop("invaild arguments")               # Error message when n is not a natural number
    }
  } else {
    stop("invalid arguments")                 # Error message when n is not a numeric value
  }
} 

### Three-methods generating ###

general.rnorm <- function(n, method = 1, mean = 0, sd = 1) {
  
  if (is.numeric(n) == TRUE) {                # Ensure n is a numeric argument
    
    if (round(n) == n &                       # Ensure n is a natural number
        n > 0 &                               # ensure n > 0
        sd >= 0) {                            # ensure sd >= 0
      results <- numeric(n)                   # Creat a vector with length n to save results
      
      if(method == 1) {                       # Use Marsaglia and Bray algorithm to generate
                                              # pseudo-random normally-distributed deviates
        
        if (n%%2 == 0) {                      # Even case
          for (i in 1 : n/2) {                # In even case, generate n/2 pairs of variates
            w <- 2                            # Any w > 1 will go to
            
            while (w > 1) {                   # rejection step
              U <- runif(2)
              U <- U*2 - 1
              w <- U[1]^2 + U[2]^2            
            }                                 # The values in U are in the unit circle
            
            v <- sqrt(-2*log(w)/w)
            X <- U*v                          # X follows the standard normal distribution
            X <- X*sd + mean                  # X follows the normal distribution with corresponding mean and sd
            results[i*2 - 1] <- X[1]          # Give values to results vector
            results[i*2] <- X[2]
          }
          return(results)                     
          
        } else {                              # Odd case
          for (i in 1 : ((n+1)/2)) {          # In odd case, generate (n+1)/2 pairs of variates
            w <- 2                            # Any w > 1 will go to
            
            while (w > 1) {                   # rejection step
              U <- runif(2)
              U <- U*2 - 1
              w <- U[1]^2 + U[2]^2
            }                                 # The values in U are in the unit circle
            
            v <- sqrt(-2*log(w)/w)
            X <- U*v                          # X follows the standard normal distribution
            X <- X*sd + mean                  # X follows the normal distribution with corresponding mean and sd
            results[i*2 - 1] <- X[1]          # Give values to results vector
            results[i*2] <- X[2]
          }
          results <- results[1:n]             # Delete one results value before output
          return(results)
        }
      } else if(method == 2) {                # Use Box and Mueller algorithm to generate
                                              # pseudo-random normally-distributed deviates
        
        if(n%%2 == 0) {                       # Even case
          for (i in 1 : n/2) {                # In even case, generate n/2 pairs of variates
            U <- runif(2)
            X1 <- sin(2*pi*U[1]) * sqrt(-2*log(U[2]))
            X2 <- cos(2*pi*U[1]) * sqrt(-2*log(U[2]))
            X1 <- X1*sd + mean                # X1 follows the normal distribution with corresponding mean and sd
            X2 <- X2*sd + mean                # X2 follows the normal distribution with corresponding mean and sd
            results[i*2 - 1] <- X1            # Give values to results vector
            results[i*2] <- X2
          }
          return(results)
        } else {                              # Odd case
          for (i in 1 : (n+1)/2) {            # In odd case, generate (n+1)/2 pairs of variates
            U <- runif(2)
            X1 <- sin(2*pi*U[1]) * sqrt(-2*log(U[2]))
            X2 <- cos(2*pi*U[1]) * sqrt(-2*log(U[2]))
            X1 <- X1*sd + mean                # X1 follows the normal distribution with corresponding mean and sd
            X2 <- X2*sd + mean                # X2 follows the normal distribution with corresponding mean and sd
            results[i*2 - 1] <- X1            # Give values to results vector
            results[i*2] <- X2
          }
          results <- results[1:n]             # Delete one results value before output
          return(results)
        }
      } else if(method == 3) {                # Use central-limit theorem algorithm to generate
                                              # pseudo-random normally-distributed deviates
        
        for (i in 1: n) {                     # Generate n variates
          U <- sum(runif(16))
          X <- (U - 8) * sqrt(12/16)
          X <- X*sd + mean                    # X follows the normal distribution corresponding with mean and sd
          results[i] <- X                     # Give values to results vector
        }
        return(results)
      } else {
        stop("invalid arguments")             # Error message when input method is not 1,2 or 3            
      }
    } else {
      stop("invalid arguments")               # Error message when n is not a natural number
    }
  } else {
    stop("invalid arguments")                 # Error message when n is not a numeric value
  }
}

### Test function 1 ###

test.1 <- function(n = 10000, method = 1, mean = 0,sd = 1) {
  
                                            # This test only takes values from general.rnorm
                                            # The default values are set to test the first method i.e. my.rnorm
  
  sample_1 <- general.rnorm(n, method, mean, sd)
  qqnorm(sample_1)                          # Check if Q-Q plot is a straight line
  hist(sample_1, prob = TRUE)               # Plots the histogram of normal sample
  curve(dnorm(x, mean, sd), add = TRUE)     # Plots the corresponding normal distribution curve
                                            # If Q-Q Plot is a straight line and the histogram
                                            # fits normal distribution curve, we could say the results
                                            # from general.rnorm follows normal distribution
}

### Test function 2 ###

test.2 <- function(n = 1000, method = 1, mean = 0, sd = 1) {
  
                                            # This function is using Kolmogorov-Smirnov Tests to check
                                            # if results from general.rnorm follow it's corresponding distribution
                                            # The default values are set to test the first method i.e. my.rnorm
  
  sample.2 <- ks.test(general.rnorm(n, method, mean, sd), "pnorm", mean, sd)
  print(sample.2)                           # By checking p-value, if p-value > 0.05 then there is no evidence 
                                            # to reject the hyppthesis that the results from general.norm
                                            # follow it's corresponding normal distribution
                          
}