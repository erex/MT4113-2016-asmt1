my.rnorm <- function(n,mean,sd) {                                 #First function contains the three variables specified in the question
  Xvec <- c()                                                     #Create a vector to hold the values
  if (missing(mean)) {                                            #Set defaults for the function:
    mean <- 0                                                     #Mean = 0 and standard deviation = 1
  }
  if (missing(sd)) {
    sd <- 1
  }
  if (length(n) != 1) {                                           #Modified to ensure the function only accepts scalar values for the variables
    return("Invalid arguments")
  }
  if (length(mean) != 1) {
    return("Invalid arguments")
  }
  if (length(sd) != 1) {
    return("Invalid arguments")
  }
  if (sd < 0) {                                                   #By definition, standard deviation > 0
    return("Invalid arguments")
  }
  if (n %% 2 == 0) {                                              #When we are required to find an even number of values
    for (j in 1:(n/2)) {
      for (i in 1:2) {                                            #Implementing the Marsaglia and Bray Theorem
        U <- c()                                                  #Creating an empty vector to hold the values
        U <- runif(i,0,1)                                         #Finding random deviates of the uniform distribution with min=o and max=1
        U <- 2*U-1                                                #And transforming the values as specified by the algorithm
      }
      w <- (U[1])^2 + (U[2])^2                                    #Defining w
      while (w > 1) {                                             #Using a while loop ensuring that the condition w<=1 is satisfied
        for (i in 1:2) {                                          #Continuously generating new pairs of random values until the condition holds
          U <- runif(i,0,1)
          U <- 2*U-1 
          w <- (U[1])^2 + (U[2])^2
        }
        if (w <= 1) {                                             #Breaking the loop as soon as the condition is satisfied
          break
        }
      }
      v <- ((-2*log(w))/w)^(1/2)                                  #Defining a new value required to compute the result
      X <- U*v                                                    #Transforming the values as required by the algorithm
      X <- X*sd+mean                                              #Giving the user the option to change the mean and standard deviation of the estimates
      Xvec <- c(Xvec,X)                                           #Creating a vector of pairs that are approximately normally distributed
    }
    return(Xvec)                                                  #Returning the vector of required values
  } else {                                                        #Due to odd number of values, we must find an extra pair and remove the final value
    for (j in 1:((n+1)/2)) {                                      #Now repeating the process but for an odd number of values required
      for (i in 1:(2)) {                                          #The following lines have the same purpose as the corresponding lines above
        U <- c()
        U <- runif(i,0,1)
        U <- 2*U-1 
      }
      w <- (U[1])^2 + (U[2])^2                          
      while (w > 1) {                                          
        for (i in 1:2) {                                         
          U <- runif(i,0,1)
          U <- 2*U-1 
          w <- (U[1])^2 + (U[2])^2
        }
        if (w <=1) {
          break
        }
      }
      v <- ((-2*log(w))/w)^(1/2)
      X <- U*v 
      X <- X*sd+mean
      Xvec <- c(Xvec,X)
    }
    newXvec <- Xvec[-(n+1)]                                       #As the algorithm can only create pairs of values, we must remove the last value calculated to have the required odd number of values 
    return(newXvec)                                               #Finally, the function returns a vector with an odd number of values
  }
}



general.rnorm <- function(n,mean,sd,method) {                     #Now creating the second function with variables number of values, mean, standard deviation and method
  Xvec <- c()                                                     #Creating an empty vector that will later hold generated values
  if (missing(mean)) {                                            #Setting defaults: mean = 0, standard deviation = 1 and method = Marsaglia and Bray ie my.rnorm
    mean <- 0
  }
  if (missing(sd)) {
    sd <- 1
  }
  if (missing(method)) {
    Y <- my.rnorm(n,mean,sd)
    return(Y)
  }
  if (length(n) != 1) {                                           #Ensuring the funcion only accepts scalar values
    return("Invalid arguments")
  }
  if (length(mean) != 1) {
    return("Invalid arguments")
  }
  if (length(sd) != 1) {
    return("Invalid arguments")
  }
  if (length(method) != 1) {
    return("Invalid arguments")
  }
  if (sd < 0) {                                                   #Standard deviation must be greater than zero by definition
    return("Invalid arguments")
  }
  if (method == 1) {                                              #If method = 1, we use the Marsaglia and Bray algorithm as defined by my.rnorm
    Y <- my.rnorm(n,mean,sd)
    return(Y)
  } else if (method == 2) {                                       #If method = 2, we use the Box and Mueller algorithm
    if (n %% 2 == 0) {                                            #If we are required to generate an even number of deviates
      for (j in 1:(n/2)) {
        for (i in 1:2) {                                          #Create a loop that forms pairs of random uniformly distributed values with min = 0 and max = 1
          U <- runif(i,0,1)
          X1 <- sin(2*pi*U[1])*((-2*log(U[2]))^(0.5))             #Transform the uniform deviates as defined by the algorithm
          X2 <- cos(2*pi*U[1])*((-2*log(U[2]))^(0.5))
        }
        Xvec <- c(Xvec,X1,X2)                                     #Create a vector containing the newly formed and transformed deviates that are now approximately normally distributed
      }
      return(Xvec)                                                #Return the vector of deviates
    } else {                                                      #Repeat the same process but for an odd number of deviates
      for (j in 1:((n+1)/2)) {                                    #We do this by adding an extra deviate and then removing it at the end
        for (i in 1:2) {                                          #Create uniform deviates and transform them
          U <- runif(i,0,1)
          X1 <- sin(2*pi*U[1])*((-2*log(U[2]))^(0.5))
          X2 <- cos(2*pi*U[1])*((-2*log(U[2]))^(0.5))
        }
        Xvec <- c(Xvec,X1,X2)                                     #Form a vector of transformed deviates
      }
      newXvec <- Xvec[-(n+1)]                                     #Remove the final value from the vector in order to have an odd number of values
      return(newXvec)                                             #Return the modified vector
    }
  } else if (method == 3) {                                       #If method = 3, we use the central-limit theorem algorithm
    for (j in 1:n) {                                              #Create a loop that specifies how many deviates we want to obtain
      for (i in 1:16) {                                           #Generate 16 uniform deviates from U(0,1) for each final value
        U <- runif(i,0,1)
      }
      X <- (sum(U)-8)*((12/16)^(0.5))                             #Sum the vector of uniform deviates and transform as required by the algorithm
      Xvec <- c(Xvec,X)                                           #Create a vector of normally-approximated deviates
    }
    return(Xvec)                                                  #Return this vector
  } else {                                                        #If method does not equal 1, 2 or 3 and is specified, the function returns an "Invalid arguments" error
    return("Invalid arguments")
  }
}



#my.rnorm tests

my.test1 <- function(n,mean,sd) {                                 #This function tests if the length of the returned vector equals the required number of deviates
  x <- my.rnorm(n,mean,sd)                                        #It is based on the function given on the assignment sheet
  if (length(x) != n) {
    return("Error")                                               #If this does not hold an error is returned
  } else {
    return("Pass")
  }
}


my.test2 <- function(n,mean,sd) {                                 #This test function shows a QQplot with a straight diagonal line
  x <- my.rnorm(n,mean,sd)                                        #The result must be checked visually
  qqnorm(x)                                                       #We are hoping to see that the generated values lie on the diagonal line with minimal very few outliers
  qqline(x)                                                       #This means that the data is normally distributed and hence our approximations have been successful
}


my.test3 <- function(n,mean,sd) {                                 #This function plots a histogram of the generated values
  x <- my.rnorm(n,mean,sd)                                        #A histogram that is bell-shaped is indicative of normality 
  hist(x)                                                         #Hence meaning that the algorithm was successfully implemented
}


my.test4 <- function(n,mean,sd) {                                 #This function calculates the proportion of data lying within one standard deviation of the mean
  numVec <- c()                                                   #We need two vectors: numVec holds all the values that lie in the required interval
  rejVec <- c()                                                   #And rejVec contains the values that do not fulfill this condition
  x <- my.rnorm(n,mean,sd)
  upperlim <- mean + 1*sd                                         #Set the upper and lower limits for our desired interval
  lowerlim <- mean - 1*sd
  for (i in 1:n) {                                                #Separate the randomly generated values into numVec and rejVec depending on whether they lie in the interval
    if (x[i] > lowerlim) {
      if (x[i] < upperlim) {
        numVec <- c(numVec,x[i])
      } else {
        rejVec <- c(rejVec,x[i])
      }
    } else {
      rejVec <- c(rejVec,x[i])
    }
  }
  crstat <- (length(numVec))/n                                    #Find the critical statistic ie the proportion of values that lie within one standard deviation of the mean
  crvalue <- 0.73                                                 #68% of data should lie in this interval, but bearing in mind that our values are approximated we increase the margin of error by 5% to 73%
  if (crstat < crvalue) {                                         #If the critical statistic is less than the critical value we set to be 73%
    return("Pass")                                                #The data was successfully approximated to be normal, hence the function returns "Pass"
  } else {                                                        #If this condition was not fulfilled, it returns "Error"
    return("Error")
  }                                       
}


my.test5 <- function(n,mean,sd) {                                 #This function is a hypothesis test for the Shapiro-Wilk test
  x <- my.rnorm(n,mean,sd)                                        #Shapiro-Wilk test is an in-built function in R that runs a normality test returning a p-value
  stvalue <- shapiro.test(x)
  if (stvalue$p.value > 0.05) {                                   #Setting the critical value to be 5% for our test
    return("Pass")                                                #If the p-value is greater than 5%, there is very little evidence to suggest that the data is not normally distributed
  } else {                                                        #Hence it returns "Pass", otherwise it gives "Error"
    return("Error")
  }
}


my.test6 <- function(n,mean,sd) {                                 #This test function confirms that my.rnorm returns numerical values
  x <- my.rnorm(n,mean,sd)                                        #It is based on the function given on the assignment sheet
  if (is.numeric(x)) {                                            #If the results is not numerical, it returns "Error"
    return("Pass")
  } else {
    return("Error")
  }
}


my.test7 <- function(n,mean,sd) {                                 #This test function checks if the mean of the generated values lies within 20% of the given mean
  x <- my.rnorm(n,mean,sd)
  xvalue <- (sum(x))/n                                            #To find the generated mean, sum all values created by the algorithm and divide by the number of deviates
  if (mean == 0) {                                                #If given mean = 0, transform both the generated and given mean by +1 to avoid division by 0
    mean <- mean + 1
    xvalue <- xvalue + 1
  }
  crstat <- xvalue/mean                                           #Our critical statistic is the generated mean divided by the given mean
  if (crstat < 1.1) {                                             #We are hoping it is as close to 1 as possible
    if (crstat > 0.9) {                                           #Set the limits within which the critical statistic must lie to pass the test
      return("Pass")
    } else {
      return("Error")                                             #If it does not hold, the function return "Error"
    }
  } else {
    return("Error")
  }
}



#general.rnorm tests                                              #Performing the equivalent tests for general.rnorm with the added method variable

general.test1 <- function(n,mean,sd,method) {
  x <- general.rnorm(n,mean,sd,method)
  if (length(x) != n) {
    return("Error")
  } else {
    return("Pass")
  }
}


general.test2 <- function(n,mean,sd,method) {
  x <- general.rnorm(n,mean,sd,method)
  qqnorm(x)
  qqline(x)
}


general.test3 <- function(n,mean,sd,method) {
  x <- general.rnorm(n,mean,sd,method)
  hist(x)
}


general.test4 <- function(n,mean,sd,method) {
  numVec <- c()
  rejVec <- c()
  x <- general.rnorm(n,mean,sd,method)
  upperlim <- mean + 1*sd
  lowerlim <- mean - 1*sd
  for (i in 1:n) {
    if (x[i] > lowerlim) {
      if (x[i] < upperlim) {
        numVec <- c(numVec,x[i])
      } else {
        rejVec <- c(rejVec,x[i])
      }
    } else {
      rejVec <- c(rejVec,x[i])
    }
  }
  crstat <- (length(numVec))/n
  crvalue <- 0.73
  if (crstat < crvalue) {
    return("Pass")
  } else {
    return("Error")
  }
}


general.test5 <- function(n,mean,sd,method) {
  x <- general.rnorm(n,mean,sd,method)
  stvalue <- shapiro.test(x)
  if (stvalue$p.value > 0.05) {
    return("Pass")
  } else {
    return("Error")
  }
}


general.test6 <- function(n,mean,sd,method) {                                 
  x <- general.rnorm(n,mean,sd,method)
  if (is.numeric(x)) {                                       
    return("Pass")
  } else {
    return("Error")
  }
}


general.test7 <- function(n,mean,sd,method) {
  x <- general.rnorm(n,mean,sd,method)
  xvalue <- (sum(x))/n
  if (mean == 0) {
    mean <- mean + 1
    xvalue <- xvalue + 1
  }
  crstat <- xvalue/mean
  if (crstat < 1.1) {
    if (crstat > 0.9) {
      return("Pass")
    } else {
      return("Error")
    }
  } else {
    return("Error")
  }
}

