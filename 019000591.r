# MT4113 - Assignment 1 
# Random Number Generation

# I confirm that the attached is my own work, except where clearly indicated in the text

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# PART 1: MARSAGLIA & BRAY'S METHOD

  # I have defined a function my.rnorm which will take arguments: 
  # n = the number of values to be returned
  # mean = the mean of the values to be returned, with default mean = 0
  # sd = the standard deviation of the values to be returned, with default sd = 0
  my.rnorm <- function (n, mean = 0, sd = 1) {
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------   
   #This section checks the inputs for each argument of the function and produces error warnings when appropriate
    
    # If n is not a numeric value, is less than 1 or is a vector, then the execution of the function will cease and an error message will be produced.
      if (is.numeric(n) == FALSE || n < 1 || (length(n)==1) == FALSE) {
       stop ('invalid arguments')
     }
    
    # If the standard deviation is less than or equal to 0, not a numeric value or it is a vector, then the execution of the function will cease and an error message will be produced
     if (sd <= 0 || is.numeric(sd) == FALSE || (length(sd)==1) == FALSE) {
       stop ('invalid arguments')
     }
    
    # If the mean is not a numeric value or it is a vector, then the execution of the function will cease and an error message will be produced
     if (is.numeric(mean) == FALSE || (length(mean)==1) == FALSE) {
       stop ('invalid arguments')
     }
    
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------    
    #This section checks whether the value of n is an even or odd number.
    # If n = even, then it will assign m = n/2, so m pairs of values will be produced in the algorithm, and hence we obtain n values
    # If n = odd, then it will assign m = (n+1)/2, so that pairs of values can still be produced. We will end up with one extra observation which we will deal with at the end of the code.
     if (n%%2 == 0) {
      m <- n/2
    } else 
      m <- (n+1)/2
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------   
     #The following code uses the Marsaglia and Bray algorithm to generate pairs of random normally distributed deviates.
     # The for loop, repeats the code m times, so that we have m pairs of x values.
     # The while loop generates values for u1 and u2, which follow a uniform distribution and are transformed to a unit square.
     # Using these values w can be calculated. 
     # This cycle is repeated until the criterion: w is less than or equal to one,  is met.
     # Once met, the loop is exited and v is calculated. 
     Xvalues <- rep(0, n)
     
         for (i in 1:m) {
    
              while (TRUE) {
      
                   u1 <- (2 * runif(1)) - 1
                   u2 <- (2 * runif(1)) - 1
    
                   w <- u1**2 + u2**2
    
                   if (w <= 1) break
              }
    
           v <- sqrt (( -2 * log(w) ) / w)
            
           x1 <- u1 * v
           x2 <- u2 * v
           
           # x1 and x2 can be transformed so that values are returned with the desired mean and sd
           x1 <- sd * x1 + mean
           x2 <- sd * x2 + mean
           
           # These values are then stored in a vector called X values, which is added to during each loop from i= 1 to m 
           Xvalues[i*2-1] <- x1
           Xvalues[i*2] <-x2
           
    }
    
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
     # If n = even, then the code has produced enough n values and we can return these values to the console.
     # If n = odd, then we will have one extra value, hence, we will only return the Xvalues vector for values from 1 to n.
    if (n%%2 == 0) {
       return (Xvalues)
     } else 
       return (Xvalues[1:(n)])
  
  }
 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Part 2: RETURNING PSEUDO-RANDOM NORMALLY-DISTRIBUTED DEVIATES
    # For part 2 I have defined a function general.rnorm which takes arguments:
    # n = number of values to return
    # mean = mean of the values to return, default = 0
    # sd = sd of the values to return, default = 1
    # method = method to be used to generate the deviates, default = 1 which refers to the Marsaglia & Bray's algorithm
      # method = 2 refers to the Box & Mueller algorithm
      # method = 3 refers to the central-limit theorem algorithm
  
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  general.rnorm <- function (n, mean = 0, sd = 1, method = 1) {
   
     # If n: is not a numerical value, is less than 1 or is a vector, then the execution of the function will cease and an error message will be produced.
    if (is.numeric(n) == FALSE || n < 1 || (length(n)==1) == FALSE) {
      stop ('invalid arguments')
    }
    
    # If the standard deviation: is less than or equal to 0, is not a numerical value or it is a vector, then the execution of the function will cease and an error message will be produced.
    if (sd <= 0 || is.numeric(sd) == FALSE || (length(sd)==1) == FALSE) {
      stop ('invalid arguments')
    }
    
    # If the mean: is not a numerical value or it is a vector, then the execution of the function will cease and an error message will be produced.
    if (is.numeric(mean) == FALSE || (length(mean)==1) == FALSE) {
      stop ('invalid arguments')
    }
     
    # If the method: is not a numerical value, is a vector, is not an integer or is not equal to 1, 2 or 3, then the execution of the function will cease and an error message will be produced.
     if (is.numeric(method) == FALSE || (length(method)==1) == FALSE || (floor(method)==method)==FALSE) {
       stop ('invalid arguments')
     }
    
    if ((method==1 || method ==2 || method ==3)== FALSE) {
      stop ('invalid arguments')
    }

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------   
  # If method 1 is called, the function my.rnorm from above is called and executed
        if (method == 1){
            X<-my.rnorm(n, mean, sd)
            return(X)
   
        } else if (method == 2) {
   #For method 2, we check whether n is even or odd, and assign m accordingly so that pairs of deviates can be produced by the algorithm
            if (n%%2 == 0) {
                 m <- n/2
                 } else
                 m <- (n + 1)/2
    # An empty vector is defined in which to store the deviates        
                      Xvalues <- c()
    # Following the algorithm, we generate a pair of u values, u1 and u2, which are then used to generate x1 and x2. These values are then transformed so that they return values with the mean and sd required  
                          for (i in 1:m) {
                                u1 <- runif(1)
                                u2 <- runif(1)
        
                                x1 <- sin(2 * pi * u1) * sqrt (-2 * log(u2))
                                x2 <- cos(2 * pi * u1) * sqrt (-2 * log(u2))
        
                                x1 <- sd * x1 + mean
                                x2 <- sd * x2 + mean
     # The x values are put into the correct position within the Xvalues vector   
                                Xvalues[i*2-1] <- x1
                                Xvalues[i*2] <- x2
        
                            }
     # If n is even, we return all X values and if n is odd, then we return 1 less X value. 
                               if (n%%2 == 0) {
                                  return (Xvalues)
                               } else
                                  return (Xvalues[1:n])
      
        } else
      # For method 3, we define two empty vectors, u and x
             u<- c()
             x<- c()
      # Using a nested loop, we first generate 16 values which are from the uniform distribution. 
             # using the central limit theorem we can find x from these values and then we transform x using the sd and mean from the arguments of the function
             # this loop is run n times to generate n observations of x values, which are then returned in a vector x.
               for (j in 1: n) {
                  for (i in 1: 16) {
                     u[i] <- runif(1)
                   }
      
                   x[j] <- (sum(u) - 8)*sqrt(12/16)
                   x[j] <- sd * x[j] + mean
      
                }
                return (x)
    
  }
  
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# TESTING FUNCTIONS
  
  # Testing that my.rnorm produces the correct number of outputs for when n is odd or even. 
  # This function is based on the function given on the assignment worksheet.
      output.test1 <- function (n) {
      output <- my.rnorm(n)
      pass.test1 <- (length(output)==n & is.numeric(output))
      return (pass.test1)
      }
      
  # Testing that general.rnorm also produces the correct number of outputs for when n is odd or even for each method
      output.test2 <- function (n) {
      output1 <- general.rnorm(n, method = 1)
      output2 <- general.rnorm(n, method = 2)
      output3 <- general.rnorm(n, method = 3)
      pass.test <- (length(output1)==n & length(output2)==n & length(output3)==n & is.numeric(output1) & is.numeric(output2) & is.numeric(output3))
      return (pass.test)
      }
  
  # Testing the null hypothesis that the sample comes from a population with an underlying normal distribution. Using a 10% significance level, if p>0.1, we conclude that we cannot reject the null hypothesis.
      output.test3 <- function (n, mean, sd) {
      output <- my.rnorm(n, mean, sd)
      s.t <- shapiro.test(output)
      pass.test <- (s.t$p>0.1)
      return (pass.test)
      }
    
  # Same test as above, but for the general.rnorm function
      output.test4 <- function (n, mean, sd, method) {
      output <- general.rnorm(n, mean, sd, method)
      s.t <- shapiro.test(output)
      pass.test <- (s.t$p>0.1)
      return (pass.test)
      }
    
  # A visual test, to see whether the data produced in the output is normally distributed, is a histogram plot. 
      output.test5 <- function (n, mean, sd, method) {
        output <- general.rnorm(n, mean, sd, method)
        h <- hist(output, main = "Histogram for the output of data")
        return (h)
      }
      
      output.test6 <- function (n, mean, sd) {
        output <- my.rnorm(n, mean, sd)
        h <- hist(output,  main = "Histogram for the output of data")
        return (h)
      }
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # END OF ASSIGNMENT 1
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  