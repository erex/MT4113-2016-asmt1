# I confirm that the attached is my own work, except where clearly indicated in the text. 
# SID - 130015976
# following styleguide: https://google.github.io/styleguide/Rguide.xml

argument.check = function(n , mu = 0 , om = 1, type = 1){
  # 
  # Purpose:
  #  Checks that the n, mean(mu) and sd(om) are sensible numbers. It checks that the number passed is
  #  scalar, atomic, not null, numerical, sensible value, non-imaginary, with respect to the Marsaglia & Bray (M&B) 
  #  method, Box-Meuller algorithm (BM), Central Limit Theorm (CLT) 
  #  Further, 
  #  for n, check it is a integer and positive and not zero 
  #  for sd(om) it checks the value is between and including 0 and 1.
  #
  # Args:
  #   n: number of times to repeat process for M&B, BM, CLT
  #   mu: default 0, must be a sclar number 
  #   om: default 1, must be a sclar number between 0 and 1
  #
  # Returns:
  #   NULL if the tests are passed. It will stop the program otherwise. 
  
  # Tests which apply to n,mu and om ----
  # Check if vector
  if(length(n) > 1) stop("invalid arguments")
  if(length(mu) > 1) stop("invalid arguments")
  if(length(om) > 1)stop("invalid arguments")
  
  # Check if atomic 
  if(is.atomic(n) != TRUE) stop("invalid arguments")
  if(is.atomic(mu) != TRUE) stop("invalid arguments")
  if(is.atomic(om) != TRUE) stop("invalid arguments")
  
  # Check if null
  if(is.null(n)) stop("invalid arguments")
  if(is.null(mu)) stop("invalid arguments")
  if(is.null(om)) stop("invalid arguments")
  
  # Check if numerical, catches NA, NaN, Inf, letters 
  if(is.finite(n) != TRUE) stop ("invalid arguments")
  if(is.finite(mu) != TRUE) stop ("invalid arguments")
  if(is.finite(om) != TRUE) stop ("invalid arguments")
  
  # Check if number is smaller than the largest integer R can hold for the system 
  #  The numbers will be copied to other variables which are required to be R integers 
  if(n>.Machine$integer.max) stop("invalid argument")
  if(mu>.Machine$integer.max) stop("invalid argument")
  if(om>.Machine$integer.max) stop("invalid argument")
  
  # Check if real (non-imaginary)
  if(Im(n) != 0) stop("invalid arguments")
  if(Im(mu) != 0) stop("invalid arguments")
  if(Im(om) != 0) stop("invalid arguments")
  
  
  # Tests which only apply to n ---- 
  # Check if integer by checking fractional part by taking modulus of 1 (very efficient) 
  if(n%%1 != 0) stop("invalid arguments")
  # Check if positive and not zero 
  if(n <= 0) stop("invalid arguments")
  
  
  # Tests which only apply to om ----
  # Check that sd is not negative
  if(om < 0) stop("invalid arguments")
  
  # Tests which only apply to type ----
  # Check that type is either 1, 2 or 3 
  if(type > 3) stop("invalid arguments")
  if(type < 1) stop("invalid arguments")
  if(type%%1 != 0 ) stop("invalid arguments")
  
  return(NULL)
}

my.rnorm = function (n , mu = 0 , om = 1){
  require(stats)
  # 
  # Purpose:
  #  Generates normally distributed numbers in vector using Marsaglia & Bray method. Options to change 
  #  mean and standard deviation. 
  #  NOTE: if n odd, it will add one to n.   
  # 
  # Explanation: 
  #  Checks that the n, mean(mu) and sd(om) are sensible numbers. Applies the Marsaglia & Bray method. 
  #  Marsaglia and Bray’s method: (Generates a pair of independent, 
  #  normally distributed variates, X1 and X2). This method uses the "rejection" method,
  #  tossing aside variates until a condition is met, before proceeding with the rest of
  #  the algorithm. Begin with a pair of uniform deviates U1 and U2, transform them such 
  #  that they are distributed over the range [-1, +1] rather than [0, 1]. 
  #  The pair of values are now distributed over the unit square:.
  #
  # Args:
  #   n: NUMBER OF SCALARS PRODUCED number of times to repeat process for M&B
  #   mu: MEAN OF DATA, default 0, must be a sclar number 
  #   om: MEAN OF STANDARD DEVIATION, default 1, must be a sclar number between 0 and 1
  #
  # Returns:
  #   x.return - a vector filled with unordered normally distributed numbers. 
  #
  # Reference: 
  #   Maindonald, John. 1984. Statistical Computation. John Wiley and Sons.
  
  
  argument.check(n , mu , om) # Apply check to make sure n,mu,om are sensible
  
  # check if n is odd or even, if odd make even by adding 1
  if (n%%2==0) {
    }  #  do nothing if even print("even") 
  else { 
     n <- n + 1  }  #  add one if odd print("odd")
  
  # Appending to an object in a for loop causes the entire object to be copied on 
  # every iteration, thus we pre-allocate the vector. 
  x.return = c(1, 2)  
  
  # Begin Marsaglia and Bray’s method
  for (i in 1:(n/2)){ 
    
    w <- 2 # set w to 2 to force the while loop to start. Instructed to use while loop in requirements 
    
    while( w > 1){ 
      u <- runif(2, 0, 1) # create u vector containing u1 and u2
      u <- 2 * u - 1   # vectorially do operation
      w <- ( u[1] )^2 + ( u[2] )^2
    }
    
    v <- (-2 * log(w)/w)^(1/2) # log is always negative so this isn't going to ever be imaginary
    
    # fill x.return 
    x.return[i] <- u[1] * v
    x.return[i + (n/2)] <- u[2] * v
    
  }
  x.return= x.return * om + mu # apply mean and standard deviation amendments. 
  # note - this will not effect the general.rnorm function calling this function as it will 
  # always pass mean = 0 and standard deviation of 1. 
  
  return(x.return)
}

general.rnorm <- function(n , mu = 0, om = 1, type = 1) {
  # 
  # Purpose:
  #  Generates normally distributed numbers in a vector using either Marsaglia and Bray’s method
  #  Box-Mueller algorithm.or Central limit theorem. Options to change mean and standard devion
  #  NOTE: if using Marsaglia and Bray Method, n odd, it will add one to n to make even.    
  # 
  # Explanation: 
  #  Checks that the n, mean(mu) and sd(om) are sensible numbers. Applies  either the Marsaglia & 
  #  Bray method (M&B) - type 1 - Box-Mueller algorithm (B&M) - type 2 -  Central limit theorem (CLT) - type 3.
  #  
  #   Marsaglia and Bray’s method: (Generates a pair of independent, 
  #    normally distributed variates, X1 and X2). This method uses the "rejection" method,
  #    tossing aside variates until a condition is met, before proceeding with the rest of
  #    the algorithm. Begin with a pair of uniform deviates U1 and U2, transform them such 
  #    that they are distributed over the range [-1, +1] rather than [0, 1]. 
  #    The pair of values are now distributed over the unit square:.
  #
  #   Box-Mueller algorithm: Founded on the principle of polar coordinates.
  #    Generating a pair of uniformly. distributed random deviates and transforming them to polar coordinates.
  #    The marginal distribution of these points in 2D space approximates the normal
  #    distribution in both the x- and y-dimension.
  #
  #   Central limit theorem: The mean of a sufficiently large number of independent random variables
  #    will be approximately normally distributed. Note this statement says nothing about
  #    the underlying distribution of these independent random variables. For purposes of this assignment,
  #    we will take the underlying distribution to be uniform and the sufficiently large number to be 16.
  # Args:
  #   n: number of times to repeat process for M&B
  #   mu: default 0, must be a sclar number 
  #   om: default 1, must be a sclar number between 0 and 1
  #   type: 
  #
  # Returns:
  #   x.return - a vector filled with unordered normally distributed numbers. 
  #
  # Reference: 
  #   Jones, O., R. Maillardet, and A. Robinson. 2009. Scientific Programming and Simulation Using R. CRC Press.
  #   Maindonald, John. 1984. Statistical Computation. John Wiley and Sons
  #   Extracts from MT 4113: Computing in Statistics, Assignment 1 paper

  require(stats)
  
box.muller <- function( n ){
  # Purpose:
  #  Generates normally distributed numbers in vector using Box-Mueller algorithm. 
  #  NOTE: if n is odd, it is converted to an even number 
  #
  # Args:
  #   n: NUMBER OF SCALARS PRODUCED number of times to repeat process for B&M
  #
  # Returns:
  #   x.list - a vector filled with unordered normally distributed numbers. 
  
  # check if n is odd or even, if odd make even by adding 1
  if (n%%2==0) {
  }  #  do nothing if even print("even") 
  else { 
    n <- n + 1  }  #  add one if odd print("odd")
  
    u1 <- runif( n/2 ) # create random uniform
    u2 <- runif( n/2 ) 
    x1 <- sqrt(-2 * log(u1) ) * cos(2 * pi * u2 ) 
    x2 <- sqrt(-2 * log(u1) ) * sin(2 * pi * u2 ) 
    xlist <- Reduce(c, x1, x2) # create random uniform single dimensional vector
    return( xlist )
  }
  
CLT <- function( n ){
  # Purpose:
  #  Generates normally distributed numbers in vector using Central limit theorem
  #
  # Args:
  #   n: NUMBER OF SCALARS PRODUCED number of times to repeat process for M&B
  #
  # Returns:
  #   x.list - a vector filled with unordered normally distributed numbers. 
    randommatrix <- matrix( runif(n * 16), ncol = 16, nrow = n)
    x.list <- (rowSums( randommatrix ) - 8) * ( 12/16 )^(1/2)
    return(x.list)
  }
  
argument.check( n , mu , om)

type=toString(type) # change type from number to a string

  switch(type, # which function to use
         "1" = (x.return=my.rnorm(n)),
         "2" = (x.return=box.muller(n)),
         "3" = (x.return=CLT(n)))

x.return= x.return * om + mu # apply mean and standard deviation amendments. 

return(x.return)
}


# ------------ TESTING --------------
distribution.testing <- function(x){
  # 
  # Purpose:
  #  Carries out an array of tests to quantify the extent of the normal distribution of the data 
  #  Also generates a q-q plot. 
  #  NOTE: Some tests require the dataset to be less than 5000 and above a minimum of 3
  #
  #  Explanation: 
  #   Uses the following normal distribution tests: 
  #   Kolmogorov-Smirnov normality test, Shapiro-Wilk's test for normality, Jarque-Bera test for normality,
  #   D'Agostino normality test, Cramer-von Mises normality test, Lilliefors normality test,
  #   Pearson chi--square normality test, Shapiro--Francia normality test.
  #   All additionally verified using a q-q plot
  #
  # Args:
  #   x: 1 dimensional dataset to check
  #
  # Returns:
  #   results: a matrix with the results of the test. 
  #   NOTE: also prints a q-q plot
  #
  # Example of use: 
  #   distribution.testing(general.rnorm(n=1000, mu=5, om=1, type=3)) #  test a function
  #   distribution.testing(runif(1000)) #  example of uniform distribution test
  #   distribution.testing(rnorm(1000)) #  example of normal distribution test
  #
  #
  # References: 
  #   Anderson T.W., Darling D.A. (1954); A Test of Goodness of Fit, JASA 49:765–69.
  #   Conover, W. J. (1971); Practical nonparametric statistics, New York: John Wiley & Sons.
  #   D'Agostino R.B., Pearson E.S. (1973); Tests for Departure from Normality, Biometrika 60, 613–22.
  #   Durbin J. (1961); Some Methods of Constructing Exact Tests, Biometrika 48, 41–55.
  #   Geary R.C. (1947); Testing for Normality; Biometrika 36, 68–97.
  #   Lehmann E.L. (1986); Testing Statistical Hypotheses, John Wiley and Sons, New York.
  #   Linnet K. (1988); Testing Normality of Transformed Data, Applied Statistics 32, 180–186.
  #   Moore, D.S. (1986); Tests of the chi-squared type, In: D'Agostino, R.B. and Stephens, M.A., eds., Goodness-of-Fit Techniques, Marcel Dekker, New York.
  #   Shapiro S.S., Francia R.S. (1972); An Approximate Analysis of Variance Test for Normality, JASA 67, 215–216.
  #   Shapiro S.S., Wilk M.B., Chen V. (1968); A Comparative Study of Various Tests for Normality, JASA 63, 1343–72.
  #   Thode H.C. (2002); Testing for Normality, Marcel Dekker, New York.
  #   Weiss M.S. (1978); Modification of the Kolmogorov-Smirnov Statistic for Use with Correlated Data, JASA 73, 872–75.
  #   Wuertz D., Katzgraber H.G. (2005); Precise finite-sample quantiles of the Jarque-Bera adjusted Lagrange multiplier test, ETHZ Preprint.

require(nortest)
require(fBasics)

# Plot using a qqplot - this compares the normal distribution to a theoretical one - should be straight line if normal distribution
qqnorm(x)
qqline(x, col = 2)

tksnorm <- ksnormTest(x) #  Kolmogorov-Smirnov normality test
tshapiro <- shapiro.test(x) #  Shapiro-Wilk's test for normality
tjarque <- jarqueberaTest(x) #  Jarque-Bera test for normality
tdago <- dagoTest(x) #  D'Agostino normality test 
tad <- ad.test(x) #  Anderson-Darling normality test    
tcvm <- cvm.test(x)  #    Cramer-von Mises normality test  
tlillie <- lillie.test(x)  #  Lilliefors normality test
tpearson <- pearson.test(x) #  Pearson chi--square normality test
tsf <- sf.test(x)  #  Shapiro-Francia normality test

# put appropraite test results into order 
a <- c("Test",tksnorm@title,tshapiro$method,tjarque@title,tdago@title,tad$method,tcvm$method,tlillie$method,tpearson$method,tsf$method,"Test","Test Statistic",tksnorm@test$statistic,tshapiro$statistic,tjarque@test$statistic,tdago@test$statistic[1],tad$statistic,tcvm$statistic,tlillie$statistic,tpearson$statistic,tsf$statistic,"Test Statistic","p-value:",tksnorm@test$p.value[3],tshapiro$p.value,tjarque@test$p.value,tdago@test$p.value[1],tad$p.value,tcvm$p.value,tlillie$p.value,tpearson$p.value,tsf$p.value,"p-value")
results <- matrix(c(a), nrow = 11, ncol = 3) #  put results into matrix. 

return(results)

}

function.testing <- function(n, mu = 5, om = 0.5) {
  # Purpose:
  #  Carries out a series of test on the general.rnorm function with user given means and standard deviation and compares that to 
  #  a mean of 0 and standard deviation of 1 (the standard data). Also checks that the correct number of data points are returned 
  #  compared to what is asked. Calculates a percentage error comparison between the user givenvalue and the calculated value from 
  #  the data returned for standard deviation and mean.
  #  Also generates histograms set out as a comparision for all the algorithms in general.rnorm
  #  NOTE: the function cannot deal with means or standard deviations of 0. 
  #
  # Args:
  #   n: number of normally distributed numbers to generate to test against
  #   mu: mean of data to produce 
  #   om: standard deviation of data to produce
  #
  # Returns:
  #   results: a matrix with the results of the test
  #   NOTE: also prints 6 histograms. 
  
  require(gridExtra)
  
  # generate variables which are not named mu and om as to avoid confusion later 
  test.mean <- mu
  test.sd <- om
  
  # error checks that the mean or standard deviation is not 0 as this will mess with 
  # percentage error calculations 
  if( test.mean == 0 ){ stop("Please Don't Give a Mean of 0.")}
  if( test.mean == 0 ){ stop("Please Don't Give a Standard Deviations of 0.")}
  
  results <- matrix(0, nrow = 4, ncol = 12) #  set up results matrix 
  
  # create titles for the matrix 
  results[1,1] = "test no."
  results[1,2] = "mean mu=0"
  results[1,3] = "% error"
  results[1,4] = paste("mean test =", toString(mu), sep = " ")
  results[1,5] = "% error"
  results[1,6] = "sd=1"
  results[1,7] = "% error"
  results[1,8] = paste("sd=", toString(om), sep = " ")
  results[1,9] = "% error"
  results[1,10] = "n given"
  results[1,11] = "n obtained"
  results[1,12] = "% error"
  
  # for each of the functions "types" in the general.rnorm function we calculate 
  # the mean & standard deviaion for the standard values and the user given values 
  # and place them in the matrix 
  for (i in 1:3) {
    mean1 <- mean( general.rnorm(n, mu = 0, om = 1, type = i)) #  calculate mean from standard data
    mean2 <- mean( general.rnorm(n, mu = test.mean, om = 1, type = i)) #  calculate mean from user given value
    
    sd1 <- sd( general.rnorm(n, mu = 0, om = 1, type = i)) #  calculate sd from standard data 
    sd2 <- sd( general.rnorm(n, mu = test.mean, om = test.sd, type = i)) #  calculate sd from user given value 
    
    n1 <- length(general.rnorm(n, mu = 0, om = 1, type = i)) #  store how many values are returned. 
    
    # fill out the results matrix 
    results[i+1,1] <- i
    results[i+1,2] <- mean1
    results[i+1,3] <- "N/A" #  we cannot work out the percentage error of a mean of 0 
    results[i+1,4] <- mean2
    results[i+1,5] <- abs(((mean2)/(test.mean)) - 1) #  percentage error calculation 
    results[i+1,6] <- sd1
    results[i+1,7] <- abs((sd1/1) - 1) #  percentage error calculation 
    results[i+1,8] <- sd2
    results[i+1,9] <- abs((sd2/test.sd) - 1) #  percentage error calculation 
    results[i+1,10] <- n
    results[i+1,11] <- n1
    results[i+1,12] <- abs((n1/n) - 1) #  percentage error calculation 
  }
  

  # generate titles for the plots 
  text.mean = toString(test.mean) #
  text.sd = toString(test.sd)
  title1 = paste("Marsaglia and Bray with mean =",text.mean,"sd = ", text.sd, sep = "")
  title2 = paste("Box-Mueller with mean =",text.mean,"sd = ", text.sd, sep = "")
  title3 = paste("Central Limit Theorem with mean =",text.mean,"sd = ", text.sd, sep = "")
  
  # Plot the data
  hist(general.rnorm(n, mu = 0, om = 1, type = 1), main = "Marsaglia and Bray with mean=0, sd=1", nclass = 40, lwd=2)
  hist(general.rnorm(n, mu = test.mean, om = test.sd, type = 1), main = title1, nclass = 40, lwd=2)
  hist(general.rnorm(n, mu = 0, om = 1, type = 2), main = "Box-Mueller with mean=0, sd=1", nclass = 40, lwd=2 )
  hist(general.rnorm(n, mu = test.mean, om = test.sd, type = 2), main = title2, nclass = 40, lwd=2)
  hist(general.rnorm(n, mu = 0, om = 1, type = 3), main = "Central Limit Theorem with mean=0, sd=1", nclass = 40, lwd=2)
  hist(general.rnorm(n, mu = test.mean, om = test.sd, type = 3), main = title3, nclass = 40, lwd=2)
  
  
  return(results)
  
}


