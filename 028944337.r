# MT4113 Assignment 1 

# I confirm that the attached is my own work, except where clearly indicated
# in the text.

# ------------------------- Algorithms for normally distributed deviates --------------------------

my.rnorm <- function(n, mean = 0, sd = 1) {
  # Purpose:
  #   Returns a vector of "n" pseudo-random normally-distributed values using the
  #   Marsaglia and Bray algorithm.
  # 
  # Input: 
  #   n:    number of pseudo-random normally-distributed values to return 
  #   mean: mean of values to return, with default mean = 0
  #   sd:   standard deviation of values to return, with default standard
  #         deviation = 1 
  #
  # Output:
  #   vector of "n" pseudo-random normally-distributed values 
  #
  # Reference:
  #   Maindonald, John. 1984. Statistical Computation. John Wiley and Sons.
  #
  # Error handling
  if (missing(n) == TRUE) {
    stop ("invalid arguments")
  }
  if (n < 1|sd < 0|length(n) != 1|is.numeric(n) == FALSE|
      is.numeric(mean) == FALSE|is.numeric(sd) == FALSE) {
    stop ("invalid arguments")
  }
  if (floor(n) != n) {
    stop ("invalid arguments")
  }
  results <- NULL
  while (length(results) < n) {
    success <- FALSE 
    while(!success) {
      x <- runif(2, min = 0, max = 1)
      y <- 2*x - 1
      w <- y[1]**2 + y[2]**2  # only values inside the unit circle are used
      
      if(w <= 1) {
        success <- TRUE
      }    
    }
    v <- sqrt((-2*log(w))/w)
    z <- y*v
    results <- c(results, z)
  }
  if (n%%2) {
    results <- results[-sample(1:length(results), 1)]  # removes a value from
    # results vector if an odd number of values is to be produced 
  }
  results <- results*sd + mean  # transforms standard normal random variables 
  # into normal random variables 
  print (results)
}

box.mueller <- function(n, mean = 0, sd = 1) {
  # Purpose: 
  #  Returns a vector of "n" pseudo-random normally-distributed values using the
  #  Box and Mueller algorithm. 
  #
  # Input: 
  #  n:    number of pseudo-random normally-distributed values to return 
  #  mean: mean of values to return, with default mean = 0
  #  sd:   standard deviation of values to return, with default standard 
  #        deviation = 1 
  #
  # Output: 
  #  vector of "n" pseudo-random normally-distributed values
  #
  # Reference:
  #  Jones, O., R. Maillardet, and A. Robinson. 2009. Scientific Programming 
  #  and Simulation Using R. CRC Press.
  #
  # Error handling
  if (missing(n) == TRUE) {
    stop ("invalid arguments")
  }
  if (n < 1|sd < 0|length(n) != 1|is.numeric(n) == FALSE|
      is.numeric(mean) == FALSE|is.numeric(sd) == FALSE) {
    stop ("invalid arguments")
  }
  if (floor(n) != n) {
    stop ("invalid arguments")
  }
  results <- NULL
  while (length(results) < n) {
    x <- runif(2, min = 0, max = 1)
    X1 <- sin(2*pi*x[1])*sqrt(-2*log(x[2]))
    X2 <- cos(2*pi*x[1])*sqrt(-2*log(x[2]))
    results <- c(results,X1,X2)
  }
  if (n%%2) {
    results <- results[-sample(1:length(results), 1)]  # removes a value from
    # results vector if an odd number of values is to be produced 
  }
  results <- results*sd + mean  # transforms standard normal random variables 
  # into normal random variables 
  print(results)
}

central.limit <- function(n, mean = 0, sd = 1) {
  # Purpose: 
  #  Returns a vector of "n" pseudo-random normally-distributed values using the
  #  Central-Limit theorem algorithm. 
  #
  # Input: 
  #  n:    number of pseudo-random normally-distributed values to return 
  #  mean: mean of values to return, with default mean = 0
  #  sd:   standard deviation of values to return, with default standard 
  #        deviation = 1 
  #
  # Output: 
  #  vector of "n" pseudo-random normally-distributed values
  #
  # Reference:
  #  Mood, A.M., F.A. Graybill, and D.C. Boes. 1974. Introduction to the Theory 
  #  of Statistics. Third Edition. McGraw Hill.
  # 
  # Error handling
  if (missing(n) == TRUE) {
    stop ("invalid arguments")
  }
  if (n < 1|sd < 0|length(n) != 1|is.numeric(n) == FALSE|
      is.numeric(mean) == FALSE|is.numeric(sd) == FALSE) {
    stop ("invalid arguments")
  }
  if (floor(n) != n) {
    stop ("invalid arguments")
  }
  results <- NULL
  while (length(results) < n) {
    a <- runif(16, min = 0, max = 1)
    b <- sum(a)
    d <- (b - 8)*sqrt(12/16)
    results <- c(results, d)
  }
  results <- results*sd + mean  # transforms standard normal random variables 
  # into normal random variables
  print(results)
}

# ----------------------------- general.rnorm -----------------------------------------------------

general.rnorm <- function(n, mean = 0, sd = 1, method = "1") {
  # Purpose: 
  #  Returns a vector of "n" pseudo-random normally-distributed values using
  #  one of three algorithms to be specified by the user: Marsaglia and Bray
  #  algorithm; Box and Mueller algorithm; Central-Limit theorem algorithm. 
  #  
  # Input: 
  #  n:      number of pseudo-random normally-distributed values to return
  #  mean:   mean of values to return, with default mean = 0
  #  sd:     standard deviation of values to return, with default standard
  #          deviation = 1 
  #  method: integer with value
  #  "1" corresponding to the Marsaglia and Bray algorithm;
  #  "2" corresponding to the Box and Mueller algorithm; 
  #  "3" corresponding to the Central-Limit theorem algorithm, 
  #  and default method = "1"
  #
  # Output: 
  #  vector of "n" pseudo-random normally-distributed values
  #
  # Reference:
  #  Mood, A.M., F.A. Graybill, and D.C. Boes. 1974. Introduction to the Theory 
  #  of Statistics. Third Edition. McGraw Hill.
  #
  # Error handling
  if (missing(n) == TRUE) {
    stop ("invalid arguments")
  }
  if (n < 1|sd < 0|length(n) != 1|is.numeric(n) == FALSE|
      is.numeric(mean) == FALSE|is.numeric(sd) == FALSE|method < 1) { 
    stop ("invalid arguments")
  }
  if (floor(n) != n) {
    stop ("invalid arguments")
  }
  switch (method,
         "1" = my.rnorm(n, mean, sd),
         "2" = box.mueller(n, mean, sd),
         "3" = central.limit(n, mean, sd),
         stop ("invalid arguments")
  )
}

# ---------------------------------- Testing functions --------------------------------------------

test.normality.general <- function(n, mean = 0, sd = 1, method = "1") {
  # Purpose: 
  #  tests that general.rnorm produces "n" pseudo-random values which are 
  #  normally-distributed, using a 95% Kolmogorov-Smirnov hypothesis test
  #
  # Input:
  #  n:      number of pseudo-random normally distributed values to be returned 
  #          by general.rnorm
  #  mean:   mean of the values to be returned by general.rnorm, with default 
  #          mean = 0 
  #  sd:     standard deviation of the values to be returned by general.rnorm, 
  #          with default standard deviation = 1 
  #  method: method to be used by general.rnorm with integer value
  #  "1" corresponding to the Marsaglia and Bray algorithm;
  #  "2" corresponding to the Box and Mueller algorithm; 
  #  "3" corresponding to the Central-Limit algorithm, 
  #  and default method = "1"
  #  
  # Output:
  #  vector of "n" pseudo-random normally-distributed values and either: 
  #  "TRUE" if the p-value returned by the K-S test is greater than the 
  #  significance level of 0.05
  #  "FALSE" if the p-value returned by the K-S test is less than the 
  #  significance level of 0.05
  #
  # Error handling
  if (missing(n) == TRUE) {
    stop ("invalid arguments")
  }
  if (n < 1|sd < 0|length(n) != 1|is.numeric(n) == FALSE|
      is.numeric(mean) == FALSE|is.numeric(sd) == FALSE|method < 1) {
    stop ("invalid arguments")
  }
  if (floor(n) != n) {
    stop ("invalid arguments")
  }
  results <- general.rnorm(n, mean, sd, method)
  test.ks <- ks.test(results, pnorm, mean, sd)
  test.result <- test.ks$p.value
  success <- TRUE 
  
  if (test.result < 0.05) {
    success <- FALSE
  }
  
  print("Is it true that general.rnorm passed the test?")
  return(success)
}

test.vector.general <- function(n, mean = 0, sd = 1, method = "1") {
  # Purpose:
  #  tests that general.rnorm produces a vector of numeric values of length n > 1
  #
  # Input: 
  #  n:      number of pseudo-random normally distributed values to be returned 
  #          by general.rnorm
  #  mean:   mean of the values to be returned by general.rnorm, with default 
  #          mean = 0 
  #  sd:     standard deviation of the values to be returned by general.rnorm, 
  #          with default standard deviation = 1 
  #  method: method to be used by general.rnorm with integer value
  #  "1" corresponding to the Marsaglia and Bray algorithm;
  #  "2" corresponding to the Box and Mueller algorithm; 
  #  "3" corresponding to the Central-Limit algorithm, 
  #  and default method = "1"
  # 
  # Output:
  #  vector of "n" pseudo-random normally-distributed values and either:
  #  "TRUE" if general.rnorm produces a vector of numeric values of length 
  #  n > 1
  #  "FALSE" if general.rnorm does not produce a vector of numeric values 
  #  of length n > 1
  # 
  # Error handling 
  if (missing(n) == TRUE) {
    stop ("invalid arguments")
  }
  if (n < 1|sd < 0|length(n) != 1|is.numeric(n) == FALSE|
      is.numeric(mean) == FALSE|is.numeric(sd) == FALSE|method < 1) {
    stop ("invalid arguments")
  }
  if (floor(n) != n) {
    stop ("invalid arguments")
  }
  results <- general.rnorm(n, mean, sd, method) 
  pass.test <- (length(results) == n & length(results) > 1 & is.numeric(results))
  print ("Is it true that general.rnorm passed the test?")
  return(pass.test)
}

test.normality.myrnorm <- function(n, mean = 0, sd = 1) {
 # Purpose: 
 #  tests that my.rnorm produces "n" pseudo-random values which are 
 #  normally-distributed, using a 95% Kolmogorov-Smirnov hypothesis test
 #
 # Input:
 #  n:    number of pseudo-random normally distributed values to be returned 
 #        by my.rnorm
 #  mean: mean of the values to be returned by my.rnorm, with default 
 #        mean = 0 
 #  sd:   standard deviation of the values to be returned by my.rnorm, 
 #        with default standard deviation = 1 
 #
 # Output:
 #  vector of "n" pseudo-random normally-distributed values and either: 
 #  "TRUE" if the p-value returned by the K-S test is greater than the 
 #  significance level of 0.05
 #  "FALSE" if the p-value returned by the K-S test is less than the 
 #  significance level of 0.05
 #
 # Error handling
 if (missing(n) == TRUE) {
  stop ("invalid arguments")
 }
 if (n < 1|sd < 0|length(n) != 1|is.numeric(n) == FALSE|
    is.numeric(mean) == FALSE|is.numeric(sd) == FALSE) {
  stop ("invalid arguments") 
 }
 if (floor(n) != n) {
  stop ("invalid arguments")
 }
 results <- my.rnorm(n, mean, sd)
 test.ks <- ks.test(results, pnorm, mean, sd)
 test.result <- test.ks$p.value
 success <- TRUE 

 if (test.result < 0.05) {
  success <- FALSE
 }

 print("Is it true that my.rnorm passed the test?")
 return(success)
}

test.vector.myrnorm <- function(n, mean = 0, sd = 1) {
 # Purpose:
 #  tests that my.rnorm produces a vector of numeric values of length n > 1
 #
 # Input: 
 #  n:    number of pseudo-random normally distributed values to be returned 
 #        by my.rnorm
 #  mean: mean of the values to be returned by my.rnorm, with default 
 #        mean = 0 
 #  sd:   standard deviation of the values to be returned by my.rnorm, 
 #        with default standard deviation = 1 
 # 
 # Output:
 #  vector of "n" pseudo-random normally-distributed values and either:
 #  "TRUE" if my.rnorm produces a vector of numeric values of length 
 #  n > 1
 #  "FALSE" if my.rnorm does not produce a vector of numeric values 
 #  of length n > 1
 # 
 # Error handling 
 if (missing(n) == TRUE) {
  stop ("invalid arguments")
 }
 if (n < 1|sd < 0|length(n) != 1|is.numeric(n) == FALSE|is.numeric(mean) == FALSE|
    is.numeric(sd) == FALSE) {
  stop ("invalid arguments")
 }
 if (floor(n) != n) {
  stop ("invalid arguments")
 }
 results <- my.rnorm(n, mean, sd) 
 pass.test <- (length(results) == n & length(results) > 1 & is.numeric(results))
 print ("Is it true that my.rnorm passed the test?")
 return(pass.test)
}
