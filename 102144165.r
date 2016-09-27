# I confirm that the attached is my own work, except where clearly indicated in the text. 

my.rnorm <- function(n, mean=0, sd=1){
  
                                                          #
                                                          # Purpose:
                                                          # Generate vector of pseudo-random values from normal 
                                                          # distribution using the Margsalia and Bray algorithm.
                                                          #
                                                          # Input:
                                                          # n - number of values to return (no default)
                                                          # mean - mean of values to return (default 0)
                                                          # sd - standard deviation of values to return (default 1)
                                                          #
                                                          # Output:
                                                          # Obtain n normally distributed values
                                                          #
  
  calculate <- function(sd=sd, mean=mean) {               # 'Calculate' function is created in order to generate
                                                          # a pair of values fulfilling the condition that
                                                          # w = U1^2 + U2^2 <= 1
    
    if (sd >= 0) {                                        # This block of code checks if standard deviation is a 
      U <-  runif(2)                                      # nonnegative number - if it is then the code continues 
      U <- 2 * U - 1                                      # and if not then the error is displayed and the execution
      w <- (U[1])^2+(U[2])^2                              # is stopped. Assuming sd >= 0 two random values are 
                                                          # generated from uniform distribution and they are transformed 
                                                          # into unit square. As both of the values need to be inside 
                                                          # the unit circle (centred on 0, radius 1) the range has to
                                                          # be changed from [0,1] to [-1,1]. Then the condition 
                                                          # U1^2 + U2^2 <= 1 is checked. If it is met then the 
                                                          # function does not go to while loop, however if it is not
                                                          # met then the function goes into while loop.
      
      while (w > 1) {                                     # While loop works until the condition U1^2 + U2^2 <= 1
        U <- runif(2)                                     # meaning that the sum of squared values is less or equal
        U <- 2 * U - 1                                    # to 1. When such pair of values is found then the work
        w <- (U[1])^2+(U[2])^2                            # of loop is finished and the code continues to execute.
      }
      
      v <-((-2)*(log(w)))/w
      v <- sqrt(v)
      X <- sd * c(U * v) + mean                           # c(U * v) gives pair of values which are normally 
                                                          # distributed N(0,1). Simultaneously the vector X
                                                          # is corrected by standard deviation and mean provided 
                                                          # by user and in effect the pair of values N(mean, sd) 
                                                          # is obtained.
      
      
      return(X)                                           # Vector X is returned.
      
    } else {                                              # If standard deviation provided by user is equal less then
                                                          # 0 then error saying that invalid arguments has been entered 
      stop("invalid arguments")                           # is displayed and the execution of the code is stopped.
      
    }
    
    
  }
  
  norm <- NULL                                            # Empty vector 'norm' is initiated in order to be able to
                                                          # write down in it the the results from further calculations. 
  
  if ((n %% 2 == 0) & (n > 0)) {                          # Modulo operation is used in order to determine if n provided
                                                          # by user is an integer and if it is even or odd.
                                                          # Modulo (n/2) = 0 when is even and integer 
                                                          # Modulo (n/2) = 1 when is odd and integer
                                                          # If modulo is not equal to 0 or 1 then user receives error message
                                                          # Example: 
                                                          # n = 6 -> 6/2 = 3 and 0 rest -> modulo = 0 -> n is even number -> program works
                                                          # n = 5 -> 5/2 = 2 and 1 rest -> modulo = 1 -> n is odd number -> program works
                                                          # n = 3.3 -> 3.3/2 = 1 and 1.3 rest -> modulo = 1.3 -> user receives error message
    
    for (i in 1:(n/2))                                    # For each value between 1 and n/2 function 'calculate' is called
      norm <- c(norm, calculate(sd, mean))                # and the results in form of single numbers are written into vector 'norm'.
                                                          # The arguments used in the function (standard deviation and mean)
                                                          # are equal to ones provided by user.
    
  } else if ((n %% 2 == 1) & (n > 0)) {                   # This line determines if n is an odd number. If yes then it continues.
    
    for (i in 1:((n+1)/2))                                # As odd number of values is needed in the end and the algorithm
      norm <- c(norm, calculate(sd, mean))                # works on pairs it is necessary to generate enough number of pairs
    norm <- norm[-length(norm)]                           # which is equal to ((n+1)/2) and then subtract the last value
                                                          # from the vector. The result is written down into 'norm' vector.
    
  } else {                                                # If n provided by user is not an integer then error message
                                                          # is displayed and the program stops further execution.
    
    stop("invalid arguments")
    
  }
  
  return(norm)                                            # In the result vector 'norm' containing n number of values N(mean,sd) is returned.
  
}

# TESTS

test1 <- function() {                                      # TEST 1 - by prof Eric Rexstad
  x <- my.rnorm(n=10)                                      # pass.test takes on the value TRUE if the object 'x' 
  pass.test <- (length(x)==10 & is.numeric(x))             # contains 10 numeric values
  return(pass.test)
}
test1()


test2 <- function() {                                      # TEST 2
  x <- my.rnorm(n=8000)                                    # This test generates histogram in order to check on the graph
  hist(x)                                                  # if the values are normally distributed. The bigger n is the
}                                                          # more accurate the result is going to be and the better the 
test2()                                                    # histogram resembles normal distribution. As overall n could be 
                                                           # also argument of this function.

test3 <- function() {                                      # TEST 3
  length(my.rnorm(100))==100                               # This test checks if the result consists of 100 
}                                                          # values. It gives the answer as logical operation TRUE / FALSE.
test3()


test4 <- function() {                                      # TEST 4
  set.seed(1)                                              # The goal of this test function is to check if the mean and sd 
  x <- my.rnorm(10000, sd=3, mean=5)                       # of n numbers produced by my.rnorm function are approximately 
  m <- mean(x)                                             # the same as the values provided by user.
  s <- sd(x)                                               # set.seed generates the same random number every time so in 
  summary <- c(s,m)                                        # effect if seed will remain the same every computer will generate 
  return(summary)                                          # the same pseudorandom numbers. Because of this it will be possible
}                                                          # to reproduce the exact result obtained by me on any other computer.                              
test4()                                                    


test5 <- function() {                                      # TEST 5                                                    
  N <- 2000                                                # This test function uses the Shapiro-Wilk test of normality.
  results <- numeric(N)                                    # The null hypothesis of this test is that the data is normally distributed
  for (i in 1:N) {                                         # whereas alternative hypothesis says that the data is not normally distributed.
    x <- my.rnorm(500)                                     # The test rejects H0 if p-value is smaller then 0.05.
    results[i] <- shapiro.test(x)$p.value < 0.05           # The simulation of the test is conducted multiple times because there is always small probablity
  }                                                        # that random numbers generated in particular test might not be normally distributed therefore
  result <- sum(results)/N                                 # it is better to check it on big sample. When alpha = 0.05 then the test should reject
  return(result)                                           # H0 in maximum 5% of cases.
}                                                          # In order to achieve accurate results the test is simulated 2000 times, each time
test5()                                                    # for n = 500. At the beginning N takes value 2000 and the numeric vector with length 
                                                           # of 2000 is initiated. Next step is to start the for loop so that 500 values from my.rnorm 
                                                           # function are generated for each value from 1 to N. Then each time Shapiro-Wilk test is performed and it is logically
                                                           # is performed and it is logically checked if p-value is bigger then 0.05. 
                                                           # In effect 2000 binary results are written into vector. If there is 0 on the result vector then
                                                           # it means that the p-value in the test was smaller then 0.05 and H0 has not been rejected therefore in
                                                           # this test data is normally distributed. If 1 appears in the result vector it means that p-value in this
                                                           # particular test was bigger then 0.05 and H0 has been rejected therefore data is not normally distributed.
                                                           # Then the sum of positive answers is divided by number of simulations. 


test6 <- function() {                                      # TEST 6
  x <- my.rnorm(8000)                                      # Quantile - quantile (qq) plot is used to compare an empirical distribution 
  qqnorm(x)                                                # to a theoretical distribution. Therefore as the data obtained from the test
  qqline(x, col = 'green')                                 # should be normally distributed they should form a straight line with angle 
}                                                          # of 45 degrees. If there are any differences between straight line           
test6()                                                    # and the plot of empirical data then it means than it means departures from normality.


test7 <- function() {                                      # TEST 7 
  install.packages("nortest")                              # The Anderson - Darling test is a build-in R test for assesing normality.
  library(nortest)                                         # In order to be able to make this test work it is necessary
  N <- 2000                                                # to install a package according to following instruction:                                            
  results <- numeric(N)                                    # install.packages("nortest")                       
  for (i in 1:N){                                          # library(nortest)                       
    x <- my.rnorm(500)                                     # and then click run. The package should be sucessfully installed.           
    results[i] <- ad.test(x)$p.value < 0.05                # The test rejects H0 if p-value is smaller then 0.05.         
  }                                                        # Logic why the test is conducted multiple times is explained in Shapiro-Wilk test (test5)         
  result <- sum(results)/N                                                         
  return(result)                                                                                     
}                                                          
test7()  


test8 <- function() {                                      # TEST 8
  x <- my.rnorm(8000)                                      # Creating a boxplot is one of the ways of assessing normality.
  boxplot(x)                                               # Data is normally distributed if the lower fence, 
}                                                          # the box and the upper fence all about equally large.
test8()

# GENERAL.RNORM

general.rnorm <- function(n, mean = 0, sd = 1, method = 1) {
  
                                                           #
                                                           # Purpose:
                                                           # Generate vector of n normally distributed values using either
                                                           # Margsalia and Bray algorithm or Box and Mueller algorithm or
                                                           # Central Limit Theorem.
                                                           #
                                                           # Input:
                                                           # n - number of values to return (no default)
                                                           # mean - mean of values to return (default 0)
                                                           # sd - standard deviation of values to return (default 1)
                                                           # method - points algorithm which is used to calculation (default 1)
                                                           #   1 - Margsalia and Bray algorithm
                                                           #   2 - Box and Mueller algorithm
                                                           #   3 - Central Limit Theorem
                                                           #
                                                           # Output:
                                                           # Obtain n normally distributed values
                                                           #
  
  if ((method == 1) & (n > 0)){                            # method = 1 indicates that Margsalia and Bray algorithm will be used.
    
    calculate <- function(sd=sd, mean=mean) {              # 'Calculate' function is created in order to generate
                                                           # a pair of values fulfilling the condition that
                                                           # w = U1^2 + U2^2 <= 1
      
      if (sd >= 0) {                                       # This block of code checks if standard deviation is a 
        U <-  runif(2)                                     # nonnegative number - if it is then the code continues 
        U <- 2 * U - 1                                     # and if not then the error is displayed and the execution
        w <- (U[1])^2+(U[2])^2                             # is stopped. Assuming sd >= 0 two random values are 
                                                           # generated from uniform distribution and they are transformed 
                                                           # into unit square. As both of the values need to be inside 
                                                           # the unit circle (centred on 0, radius 1) the range has to
                                                           # be changed from [0,1] to [-1,1]. Then the condition 
                                                           # U1^2 + U2^2 <= 1 is checked. If it is met then the 
                                                           # function does not go to while loop, however if it is not
                                                           # met then the function goes into while loop.
        
        while (w > 1) {                                    # While loop works until the condition U1^2 + U2^2 <= 1
          U <- runif(2)                                    # meaning that the sum of squared values is less or equal
          U <- 2 * U - 1                                   # to 1. When such pair of values is found then the work
          w <- (U[1])^2+(U[2])^2                           # of loop is finished and the code continues to execute.
        }
        
        v <-((-2)*(log(w)))/w
        v <- sqrt(v)
        X <- sd * c(U * v) + mean                          # c(U * v) gives pair of values which are normally 
                                                           # distributed N(0,1). Simultaneously the vector X
                                                           # is corrected by standard deviation and mean provided 
                                                           # by user and in effect the pair of values N(mean, sd) 
                                                           # is obtained.
        
        
        return(X)                                          # Vector X is returned.
        
      } else {                                             # If standard deviation provided by user is equal less then
                                                           # 0 then error saying that invalid arguments has been entered 
        stop("invalid arguments")                          # is displayed and the execution of the code is stopped.
        
      }
      
    }
    
    norm <- NULL                                           # Empty vector 'norm' is initiated in order to be able to
                                                           # write down in it the the results from further calculations. 
    
    if (n %% 2 == 0) {                                     # Modulo operation is used in order to determine if n provided
                                                           # by user is an integer and if it is even or odd.
                                                           # Modulo (n/2) = 0 when is even and integer 
                                                           # Modulo (n/2) = 1 when is odd and integer
                                                           # If modulo is not equal to 0 or 1 then user receives error message
                                                           # Example: 
                                                           # n = 6 -> 6/2 = 3 and 0 rest -> modulo = 0 -> n is even number -> program works
                                                           # n = 5 -> 5/2 = 2 and 1 rest -> modulo = 1 -> n is odd number -> program works
                                                           # n = 3.3 -> 3.3/2 = 1 and 1.3 rest -> modulo = 1.3 -> user receives error message
      
      for (i in 1:(n/2))                                   # For each value between 1 and n/2 function 'calculate' is called
        norm <- c(norm, calculate(sd, mean))               # and the results in form of single numbers are written into vector 'norm'.
                                                           # The arguments used in the function (standard deviation and mean)
                                                           # are equal to ones provided by user.
      
    } else if (n %% 2 == 1) {                              # This line determines if n is an odd number. If yes then it continues.
      
      for (i in 1:((n+1)/2))                               # As odd number of values is needed in the end and the algorithm
        norm <- c(norm, calculate(sd, mean))               # works on pairs it is necessary to generate enough number of pairs
      norm <- norm[-length(norm)]                          # which is equal to ((n+1)/2) and then delete the last value
                                                           # from the vector. The result is written down into 'norm' vector.
      
    } else {                                               # If n provided by user is not an integer then error message
                                                           # is displayed and the program stops further execution.
      stop("invalid arguments")
      
    }
    
    return(norm)                                           # In the result vector 'norm' containing n number of values N(mean,sd) is returned.
    
  } else if ((method == 2) & (n > 0) & (sd >= 0)) {        # method = 2 indicates that Box and Mueller algorithm will be used.
                                                           # Because of the fact that this algorithm does not use sd or mean
                                                           # there is no condition for the value of those.
    
    calculateBM <- function(){                             # 'calculateBM' function is created in order to generate a pair
                                                           # of values which are uniformly distributed at the very beggining
                                                           # and after following transformations they become normally distributed.
      
      U <- runif(2)                                        # Random pair of numbers is generated from uniform distribution and they
      U1<-U[1]                                             # are written down into vector 'U'. Then the vector which consists from 
      U2<-U[2]                                             # two elements is divided into two separate vectors.
      
      tri <- 2 * pi * U1
      root <- sqrt((-2)*log(U2))
      
      X1 = (sin(tri)) * root                               # After transformations values change their distribution from uniform to normal.
      X2 = (cos(tri)) * root
      
      result <- c(X1, X2)                                  # Vector 'result' is created and it cosists of two normally distributed values.
      
      return(result)                                       # Vector 'result' is returned.
      
    }
    
    norm <- NULL                                           # Empty vector 'norm' is initiated in order to be able to
                                                           # write down in it the the results from further calculations. 
    
    if (n %% 2 == 0) {                                     # Modulo is equal to 0 therefore n provided by user is an integer and even number.
      
      for (i in 1:(n/2))                                   # For each value between 1 and n/2 function 'calculateBM' is called
        norm <- c(norm, calculateBM())                     # and the results in form of single numbers are written into vector 'norm'.
      
    } else if (n %% 2 == 1) {                              # Modulo is equal to 1 therefore n provided by user is an integer and odd number.
      
      for (i in 1:((n+1)/2))                               # As odd number of values is needed in the end and the algorithm
        norm <- c(norm, calculateBM())                     # works on pairs it is necessary to generate enough number of pairs
      norm <- norm[-length(norm)]                          # which is equal to ((n+1)/2) and then delete the last value
                                                           # from the vector. The result is written down into 'norm' vector.
      
    } else {                                               # If n provided by user is not an integer then error message
                                                           # is displayed and the program stops further execution.
      stop("invalid arguments")
      
    }
    
    return(norm)                                           # In the result vector 'norm' containing n number of values is returned.
    
  } else if ((method == 3) & (n > 0) & (sd >= 0)){         # method = 3 indicates that Central Limit Theorem will be used. 
    
    calculateCLM <- function() {                           # 'calculateCLM' function is created in order to generate 16 uniformly distributed
                                                           # values and then transforming them into one value which has normal distribution.
      U <- runif(16)                                       # After repeating such operation n times n normally distributed values should be returned.
      X = ((sum(U)) - 8) * sqrt(12/16)
      
      return(X)                                            # Vector 'X' is returned.
      
    }
    
    norm <- NULL                                           # Empty vector 'norm' is initiated in order to be able to
                                                           # write down in it the the results from further calculations. 
    
    if ((n %% 2 == 0)|(n %% 2 == 1)) {                     # Modulo is equal to 0  or 1 therefore n provided by user is an integer.
      
      for (i in 1:(n))                                     # For each value between 1 and n function 'calculateCLM' is called
        norm <- c(norm, calculateCLM())                    # and the results in form of single numbers are written into vector 'norm'.
      
    } else {                                               # If n provided by user is not an integer then error message
                                                           # is displayed and the program stops further execution.
      stop("invalid arguments")
      
    }
    
    return(norm)                                           # In the result vector 'norm' containing n number of values is returned.
    
  } else {                                                 # If the user pointed any other number than 1 or 2 or 3 then error will
                                                           # be displayed and the execution of the program will be stopped.
    stop("invalid arguments")
    
  }
  
}

# TESTS
# TESTS ARE THE SAME AS THE ONES TESTING MY.RNORM

test1.2 <- function() {                                    # TEST 1 - by prof Eric Rexstad
  x <- general.rnorm(n=10)                                 # pass.test takes on the value TRUE if the object 'x' 
  pass.test <- (length(x)==10 & is.numeric(x))             # contains 10 numeric values
  return(pass.test)
}
test1.2()


test2.2 <- function() {                                    # TEST 2
  x <- general.rnorm(n=8000)                               # This test generates histogram in order to check on the graph
  hist(x)                                                  # if the values are normally distributed. The bigger n is the
}                                                          # more accurate the result is going to be and the better the 
test2.2()                                                  # histogram resembles normal distribution. As overall n could be 
                                                           # also argument of this function.


test3.2 <- function() {                                    # TEST 3
  length(general.rnorm(100))==100                          # This test checks if the result consists of 100 
}                                                          # values. It gives the answer as logical operation TRUE / FALSE.
test3.2()


test4.2 <- function() {                                    # TEST 4
  set.seed(1)                                              # The goal of this test function is to check if the mean and sd 
  x <- general.rnorm(10000, sd=3, mean=5)                  # of n numbers produced by my.rnorm function are approximately 
  m <- mean(x)                                             # the same as the values provided by user.
  s <- sd(x)                                               # set.seed generates the same random number every time so in 
  summary <- c(s,m)                                        # effect if seed will remain the same every computer will generate 
  return(summary)                                          # the same pseudorandom numbers. Because of this it will be possible
}                                                          # to reproduce the exact result obtained by me on any other computer.                              
test4.2()                                                    


test5.2 <- function() {                                    # TEST 5                                                     
  N <- 2000                                                # This test function uses the Shapiro-Wilk test of normality.
  results <- numeric(N)                                    # The null hypothesis of this test is that the data is normally distributed
  for (i in 1:N) {                                         # whereas alternative hypothesis says that the data is not normally distributed.
    x <- general.rnorm(500)                                # The test rejects H0 if p-value is smaller then 0.05.
    results[i] <- shapiro.test(x)$p.value < 0.05           # The simulation of the test is conducted multiple times because there is always small probablity
  }                                                        # that random numbers generated in particular test might not be normally distributed therefore
  result <- sum(results)/N                                 # it is better to check it on big sample. When alpha = 0.05 then the test should reject
  return(result)                                           # H0 in maximum 5% of cases.
}                                                          # In order to achieve accurate results the test is simulated 2000 times, each time
test5.2()                                                  # for n = 500. At the beginning N takes value 2000 and the numeric vector with length 
                                                           # of 2000 is initiated. Next step is to start the for loop so that 500 values from general.rnorm 
                                                           # function are generated for each value from 1 to N. Then each time Shapiro-Wilk test is performed and it is logically
                                                           # is performed and it is logically checked if p-value is bigger then 0.05. 
                                                           # In effect 2000 binary results are written into vector. If there is 0 on the result vector then
                                                           # it means that the p-value in the test was smaller then 0.05 and H0 has not been rejected therefore in
                                                           # this test data is normally distributed. If 1 appears in the result vector it means that p-value in this
                                                           # particular test was bigger then 0.05 and H0 has been rejected therefore data is not normally distributed.
                                                           # Then the sum of positive answers is divided by number of simulations. 


test6.2 <- function() {                                    # TEST 6
  x <- general.rnorm(8000)                                 # Quantile - quantile (qq) plot is used to compare an empirical distribution 
  qqnorm(x)                                                # to a theoretical distribution. Therefore as the data obtained from the test
  qqline(x, col = 'green')                                 # should be normally distributed they should form a straight line with angle 
}                                                          # of 45 degrees. If there are any differences between straight line           
test6.2()                                                  # and the plot of empirical data then it means than it means departures from normality.


test7.2 <- function() {                                      # TEST 7 
  install.packages("nortest")                              # The Anderson - Darling test is a build-in R test for assesing normality.
  library(nortest)                                         # In order to be able to make this test work it is necessary
  N <- 2000                                                # to install a package according to following instruction:                                            
  results <- numeric(N)                                    # install.packages("nortest")                       
  for (i in 1:N){                                          # library(nortest)                       
    x <- general.rnorm(500)                                # and then click run. The package should be sucessfully installed.           
    results[i] <- ad.test(x)$p.value < 0.05                # The test rejects H0 if p-value is smaller then 0.05.         
  }                                                        # Logic why the test is conducted multiple times is explained in Shapiro-Wilk test (test5)         
  result <- sum(results)/N                                                         
  return(result)                                                                                     
}                                                          
test7.2()  


test8.2 <- function() {                                    # TEST 8
  x <- general.rnorm(8000)                                 # Creating a boxplot is one of the ways of assessing normality.
  boxplot(x)                                               # Data is normally distributed if the lower fence, 
}                                                          # the box and the upper fence all about equally large.
test8.2()

# References

# Jones,O.,R.Maillardet,and A.Robinson. 2009. Scientific Programming and Simulation Using R .CRC Press. 
# Maindonald,John. 1984. Statistical Computation. John Wiley and Sons. 
# Mood,A.M.,F.A.Graybill,and D.C.Boes. 1974. Introduction to the Theory of Statistics. Third Edition. McGraw Hill.
# Chang, Winston. 2013. R Graphics Cookbook. O'Reilly.
# Field, A., Miles, J., Field, Z.(2012). Discovering Statistics using R. Sage Publications.
# http://www.dummies.com/programming/r/how-to-chain-ifelse-statements-in-r/
# http://academic.uprm.edu/wrolke/esma3101/normalcheck.htm
# https://www.youtube.com/watch?v=UJmd_Uzsltw
# https://sites.ualberta.ca/~pdc2/252/TtoolsAssumGraphs.pdf
# https://www.youtube.com/watch?v=Erze9pNIX8A
# https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test
# https://www.youtube.com/watch?v=dRAqSsgkCUc
