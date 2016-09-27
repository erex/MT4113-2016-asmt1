#I confirm that the attached is my own work, except where clearly indicated in the text.


# defining a function to create the unit square using pairs of randomly generated uniformly distributed numbers
# creating an empty vector of size 2 and filling it with a pair of randomly generated uniformly distributed numbers
# then replacing these values with newly calculated values of the unit square
unit.sqr <- function() {
  A <- rep(0,2)
  for (i in 1:2) {
    A[i] <- runif(1)  
    A[i] <- (2* A[i] ) - 1
  }
  return (A)
}


# Error checking - if n is a double, or less than 0, or is a vector then the function 
# will stop and produce the error message "invalid argument"
# this if statement redefines n as an integer if entered as a decimal. It will always take the integer part of the n
conditions <- function(n) {
  if (!is.double(n) || n < 0 || !length(n) == 1 ) {
    stop("invalid arguments")
  }
  if (!is.integer(n)) {
    n <- as.integer(n)
  }
}


#defining a function to run like the function rnorm.
my.rnorm <- function( n , mean = 0, sd = 1) {
  C <- conditions(n)
  
  #defining variables used within the function
  L1 = n 
  X1 <- rep(0,L1)
  a = -1
  b = 0

  # If L1 is an odd integer this statement re- defines it as an even integer such that the function will run, given the code only works given pairs (even n)
  if (L1%%2 != 0 ) { 
    L1 = n + 1
  }
  
  # begin a while loop such that if the conditions bellow arent met they run untill they are satisfied
  while (L1 != 0) {
    a = a + 2
    b = b + 2
    # calling the function defined above
    A<- unit.sqr()
    W <- (A[1])^2 + (A[2])^2 
    
    # if W calculated above is greater than 1 (outside the unit circle) then run the function unit.sqr, and calculate another W untill W is less than 1 
    while (W > 1) { 
      A <- unit.sqr()
      W <- (A[1])^2 + (A[2])^2 
    }
    V <- sqrt((-2*log(W))/W)

    # work though the loop in pairs to fill a new vector of size L1 with the specified equations    
    for (i in a:b) {  
      if (i%%2 != 0) { 
        X1[i] <- (A[1] * V )
      } else {
        X1[i] <- (A[2] * V )  
      }
      # dependent on the mean and sd entered into the argument the values calculated above might need transformed
    }
    X1[i] <- (X1[i] * sd) + mean 
    # this loop will run untill L1 = 0
    L1 = L1 - 2
  }
  # in the case n is odd, this statment removes the final lisiting of what has been calculated above (n even) to give the user the n they asked for
  if (n%%2 != 0 ) { 
    return (X1[1:n])
  }
  
  return (X1) 
}



# Box Mueller algorithm. Jones et al(2009)

box.muel2  <- function(n, mean = 0, sd = 1) {
  C <- conditions(n)
  L2 = n 
  if (n%%2 != 0 ) {
    L2 = n + 1
  }
  a = -1 
  b = 0 
  B <- rep(0,2)
  X2 <- rep(0,L2)
  while (L2 != 0) { 
    a = a + 2 
    b = b + 2
    for (i in 1:2)  {  
      B[i] <- runif(1) 
    } 
    for (i in a:b) { 
      if (i%%2 != 0){
        
        X2[i] <- sin( 2 * pi * B[1]) * sqrt(-2 * log(B[2])) 
      }
      else { 
        X2[i] <- cos( 2 * pi * B[1]) * sqrt(-2 * log(B[2])) 
      }
    }
    
    X2[i] <- (X2[i] * sd) + mean 
    
    L2 = L2-2
  }
  if (n%%2 != 0 ) { 
    return (X2[1:n])
    }
  
  return (X2)
}


# Central Limit Theorem

#creates a function to produce 16 random numbers from a uniform distribution and store them in an array
#then takes these numbers and adds them all together
part.1<- function() {
  C <- rep(0,16) 
  for (i in 1:16) { 
    C[i] <- runif(1) 
    S = sum(C) 
    }
  return (S)
}

CLT<- function(n, mean = 0, sd = 1 ) { 
  C <- conditions(n)
  L3 = n
  X3 <- rep(0,L3)
  # runs a loop from 1 to L3 and calls on the function defined above, part.1 and uses the result to calculate the CLT
  for (i in 1:L3) {
    X <- part.1()
    X3[i] <-  ( X -8 ) * sqrt(12/16) 
    X3[i] <- (X3[i] * sd) + mean 
  }

  
  return (X3) 
}


# General Normal function

# creates a function with default mean = 0, standard deviation = 1 and method 1 as the default
general.rnorm <- function(n, mean = 0, sd = 1, method = "1" ) {
  C <- conditions(n)
  # creates a condition that if the method entered is less than 1 or greater than 3 it returns the error message
  if (method < 0 || method > 3 ) {
    stop("invalid arguments")
  }
  #sets up an empty vector and fills it with the default method, my.rnorm
  Y <- vector()
  Y <- my.rnorm(n, mean, sd )
  
  # if method 2 is called upon, the box mueller algorithm will be carried out 
  if (method == 2 ) {
    Y <- box.muel2(n, mean, sd)
    # if method 3 is called upon, the Central Limit Theorem will be carried out
  } else { (method == "3")
    Y <- CLT(n, mean , sd)
  }
  
  # the prefered method will be returned at the end of this function 
  return (Y)
}





# Test Functions

#this first function uses a QQ plot to test if the data produced by my.rnorm is of a uniform distribution
# if the distribution lies around blue line it follows a normal distiburtion(rnorm), this should also work for different means and standard deviations
plot.function1<- function(n, mean, sd) {
  x <- my.rnorm(n, mean, sd)
  Q <- qqnorm(x)
  qqline(x, col = 4, lty = 1)
}

# this second function tests that is n is greater than 0 then it will run rnorm and check that 
# the function returns a vector of the size desired ( even or odd ) and that the variables in the vector are numbers
test.function1 <- function(n, mean, sd){
  if (n > 0){
    x1 <- my.rnorm(n, mean, sd)
    pass.test1 <- (length(x1) == n & is.numeric(x1))
  } else {
    stop("n must be greater than zero")
  }
  return (pass.test)
}

#this third function tests that if a decimal is entered into the function that an integer is returned
test.function2 <- function(n, mean, sd){
  x1 <- my.rnorm(n, mean, sd)
  pass.test2 <- (length(is.integer(x1)) & is.numeric(x1))
  }

# plots all the methods in a QQ plot to check a normal distriburion is created by all methods
plot.function2 <- function(n, mean, sd, method) {
  g <- general.rnorm(n, mean, sd, method)
  Q <- qqnorm(g)
  qqline(q, col = 6, lty = 1)
}

test.function3 <- function(n, mean, sd, method){
  if (n > 0){
    g1 <- general.rnorm(n, mean, sd, method)
    pass.test3 <- (length(g1) == n & is.numeric(g1))
  } else {
    stop("n must be greater than zero")
  }
  return (pass.test)
}

#this third function tests that if a decimal is entered into the function that an integer is returned
test.function4 <- function(n, mean, sd, method){
  g1 <- general.rnorm(n, mean, sd, method)
  pass.test4 <- (length(is.integer(g1)) & is.numeric(g1))
}

#this third function tests that if a method other than 1 to 3 is entered an error message apears
test.function5 <- function(n, mean, sd, method = -4){
  g1 <- general.rnorm(n, mean, sd, method)
  pass.test5 <- (Error)
}

