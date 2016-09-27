#MT4113 Assignement 1
#I confirm that the attached is my own work, exceptwhere clearly indicated in the text 

#Purpose of my.rnorm:
#     Generate pseudo-random values from a normal distribution
#
#Input:
#    n - number of values produced
#    mean - the mean of the distribution
#    sd - the standard deviation of the distribution
#
#Output:
#    Vector with n pseudo-random normally distributed random deviates

my.rnorm <- function(n, mean=0, sd=1){
  #The following 3 if statements check that the input is of the required form
  if (is.numeric(n) != TRUE || n %% 1 != 0 || n <= 0 || length(n) != 1){
    stop('invalid arguments')
  }
  if (is.numeric(mean) != TRUE || length(mean) != 1){
    stop('invalid arguments')
  }
  if (is.numeric(sd) != TRUE || sd < 0 || length (sd) != 1){
    stop('invalid arguments')
  }
  nchanged <- 0
  if (n %% 2 == 1){  
    n <- n+1         #Adds 1 to n if odd so we can create pairs of variates
    nchanged <- 1    #nchanged be assigned 1 if it has been increased by 1 or 0 otherwise
  }
  results <- c(rep(0,n)) #Sets up empty vector to add our variates 
  for (i in 1:(n/2)){ #Loop until n/2 as we create a pair of variates at a time
    W <- 2            #Set arbitrarily to a number above 1 to start the while loop 
    escape <- 1       #Set an escape variable to exit the loop if we get stuck
    while (W > 1 & escape < 100){
      U <- runif(2,0,1)
      U1 <- 2*U[1] - 1
      U2 <- 2*U[2] - 1
      W <- (U1)**2 + (U2)**2
      escape <- escape + 1
    }
    V <- sqrt(((-2)*log(W))/W)
    X1 <- U1*V
    X2 <- U2*V
    results[2*i-1] <- X1 #Set the 1st then 3rd,5th ect position to each X1
    results[2*i] <- X2   #Set the 2nd then 4th,6th ect position to each X2
  }
  results <- results*sd + mean #Tranforms values to correct mean and sd
  if (nchanged == 0){
    return(results)
  }
  if (nchanged == 1){
    return(results[1:n-1]) #If we created one more deviate by changing n we return 
  }                        #them all apart from the last one
}

#############################################################################
#############################################################################

#Purpose of general.rnorm:
#   Generate pseudo-random values from a normal distribution with different
#   methods
#
#Input:
#   n - number of values produced
#   mean - the mean of the distribution
#   sd - the standard deviation of the distribution
#   method - select either 1, 2 or 3 based on which method should be used 
#            1 - Marsaglia and Bray algorithm 
#            2 - Box and Mueller algorithm
#            3 - Central limit theorem algorithm
#
#Output:
#   Vector with n pseudo-random normally distributed random deviates  

general.rnorm <- function(n, mean=0, sd=1, method=1){
  #The first 4 if statements check the inputs are of the required form
  if (is.numeric(n) != TRUE || n %% 1 != 0 || n <= 0 || length(n) != 1){
    stop('invalid arguments')
  }
  if (is.numeric(mean) != TRUE || length(mean) != 1){
    stop('invalid arguments')
  }
  if (is.numeric(sd) != TRUE || sd < 0 || length(sd) != 1){
    stop('invalid arguments')
  }
  if (is.numeric(method) != TRUE || is.element(method,c(1,2,3)) == FALSE || length(method) != 1){
    stop('invalid arguments')
  }
  if (method == 1){
    return(my.rnorm(n, mean, sd)) #If method is 1 then it uses my.rnorm
  }
  nchanged <- 0
  if (n %% 2 == 1){
    n <- n+1        #Adds 1 to n if odd so we can create pairs of variates
    nchanged <- 1   #nchanged be assigned 1 if it has been increased by 1 or 0 otherwise
  }
  results <- c(rep(0,n))   #Sets up empty vector to add our variates
  if (method == 2){
    for (i in 1:(n/2)){
      U <- runif(2,0,1)    
      X1 <- sin(2*pi*U[1])*sqrt((-2)*log(U[2]))
      X2 <- cos(2*pi*U[1])*sqrt((-2)*log(U[2]))
      results[2*i-1] <- X1 #Set the 1st then 3rd,5th ect position to each X1
      results[2*i] <- X2   #Set the 2nd then 4th,6th ect position to each X2
    }
  }
  if (method == 3){
    for (i in 1:n){
      U <- runif(16,0,1)
      X <- (sum(U)-8)*sqrt(12.0/16.0)
      results[i] <- X      #Sets each X variate produced to position i in results
    }
  }
  results <- results*sd + mean #Tranforms the variates to required mean and sd
  if (nchanged == 0){
    return(results)
  }
  if (nchanged == 1){
    return(results[1:n-1]) #If n has increased by 1 in our function it will return
  }                        #all of the variates in results apart from the last one
}

#The following test function is using the checking that was an example in the
#assignment sheet

#Purpose of testfunction:
#   Check whether both my.rnorm and general.rnorm produce a vector of n numbers
#
#Input:
#   n - number of values produced
#   mean - the mean of the distribution
#   sd - the standard deviation of the distribution
#   method - select either 1, 2 or 3 based on which method should be used 
#            1 - Marsaglia and Bray algorithm 
#            2 - Box and Mueller algorithm
#            3 - Central limit theorem algorithm
#
#Output:
#   A vector of length 2 contining either TRUE or FALSE depending on whether
#   the functions passed the test or not

testfunction <- function(n, mean=0, sd=1, method=1){
  myrnormvalues <- my.rnorm(n, mean, sd)
  generalrnormvalues <- general.rnorm(n, mean, sd, method)
  pass.test1 <- (length(myrnormvalues) == n & length(generalrnormvalues) == n)
  pass.test2 <- (is.numeric(myrnormvalues) == TRUE & is.numeric(generalrnormvalues) == TRUE)
  results <- c(pass.test1,pass.test2)
  return(results)
}

#Purpose of visualtests:
#   Produces histograms and qqplots for deviates produced which can be inspected
#   to see if the functions seem to be producing the correct values from a normal
#   distribution
#
#Input:
#   n - number of values produced
#   mean - the mean of the distribution
#   sd - the standard deviation of the distribution
#   method - select either 1, 2 or 3 based on which method should be used 
#            1 - Marsaglia and Bray algorithm 
#            2 - Box and Mueller algorithm
#            3 - Central limit theorem algorithm
#
#Output:
#   If method 1 is used then a histgram and q-q plot for both my.rnorm and 
#   general.rnorm are produced. If method 2 or 3 is used then one histogram and
#   q-q plot is produced

#If the functions are producing random values from normal distributions then the 
#histogram should look roughly symmetrical about the mean for large enough values 
#of n. The most frequent values should also occur around the mean. The histgrams
#can give an indication that the deviates were tranformed into values with the
#right mean and sd. The q-q plot should also be resemble a straight line for 
#normality


visualtests <- function(n, mean=0, sd=1, method=1){
  if (method == 1){
    par(mfrow=c(2,2))
    myrnormvalues <- my.rnorm(n, mean, sd)
    hist(myrnormvalues, main = 'Histogram for my.rnorm') 
    qqnorm(myrnormvalues, main = 'Normal Q-Q Plot for my.rnorm')
  }  else {
    par(mfrow=c(1,2))
  }
  generalrnormvalues <- general.rnorm(n, mean, sd, method)
  hist(generalrnormvalues, main = 'Histogram for general.rnorm')
  qqnorm(generalrnormvalues, main = 'Normal Q-Q Plot for general.rnorm')
  on.exit(par(mfrow=c(1,1)))
}

#Purpose of hypothesistests:
#   Carries out a two sample t-test to check whether the built in function
#   rnorm and the produced functions produce random deviates with the same mean.
#   Also carries out a Shapiro-wilk normality test where the null hypothesis is that
#   the produced deviates are from a normal distribution
#
#Input:
#   n - number of values produced
#   mean - the mean of the distribution
#   sd - the standard deviation of the distribution
#   method - select either 1, 2 or 3 based on which method should be used 
#            1 - Marsaglia and Bray algorithm 
#            2 - Box and Mueller algorithm
#            3 - Central limit theorem algorithm
#
#Output:
#   The result of the two sample t test and shapiro-wilk test

#For the two sample t-tests the null hypothesis is that the values from either
#my.rnorm or general.rnorm have the same mean as values produced by rnorm. We 
#reject the null hypothesis at the 5% significance level if the p-value is
#less than 0.05. It also returns the mean of each sample to compare and a 95%
#confidence interval for the difference in means. 

#For the shapiro wilk test we reject the null hypothesis at the 5% level if the
#p-value is less than 0.05 and hence would have evidence that the deviates are not
#from a normal distribution. This test only works for n between 3 and 5000 so an
#if statement is added to only run the test if n is less thank 5000.

hypothesistests <- function(n, mean=0, sd=1, method=1){
  if (method == 1){
    myrnormvalues <- my.rnorm(n, mean, sd)
    ttest <- t.test(myrnormvalues,rnorm(n, mean, sd))
    if (n < 5000){
      shapiro <- shapiro.test(myrnormvalues)
    print(shapiro)
    }
    return(ttest)
  } else {
    generalrnormvalues <- general.rnorm(n, mean, sd, method)
    ttest <- t.test(generalrnormvalues, rnorm(n, mean, sd))
    if (n < 5000){
      shapiro <- shapiro.test(generalrnormvalues)
    print(shapiro)
    }
    return(ttest)
  }
}


