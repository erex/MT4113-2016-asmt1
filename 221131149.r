#I confirm that the attached is my own work, except where clearly indicated in the text.


check.error <- function(method=1,n,mean,sd){
  #  
  #  Purpose:
  #     check.error is the function that will be called from within the other functions to check the input parameters for errors.
  #     method is assigned a pre-set value of 1 in case check. error is called by my.rnorm(instead of general.rnorm)
  #     as my.rnorm does not provide for this parameter
  #  
  #  Input:
  #     method - method is assigned a pre-set value of 1 in case check. error is called by my.rnorm(instead of general.rnorm)
  #     n, mean, sd - parameters like in the actual *.rnorm functions
  #       #
  #  Output:
  #     error code - 0, 1, or 2
  #
  #
  #First I check whether the length ofthe parameters is not different from one as the programme does not handle vectors of length >= 2
  if(length(n)!=1 || length(method)!=1 || length(mean)!=1 || length(sd)!=1) {   
    return(1)
    #If the length is incorrect check.error returns error code 1
  }
  #next check whether n is a whole number (decimals do not make any sense, only integers do)
  #i am using a simple modulus check so as to not change the datatype from double to integer
  if(n %% 1 != 0) {
    #If n is not a whole number chceck.error returns error code 2
    return(2)
  }
  #If no error is found, chceck.error returns code 0, indicating all parameters are fine
  return(0)
}

#my.rnorm is defined as a function with the parameters as requested
my.rnorm <- function(n,mean=0,sd=1) {
  #  
  #  Purpose:
  #     my.rnorm incorporates Marsaglia's and Bray's method to find random numbers from a normal distribution.
  #     
  #
  #  Input:
  #     n - number of random numbers to be generated
  #     mean - default value is 0 - define the mean of the normal distribution the random values are taken from
  #     sd - default value 1 -  standard deviatoion of the normal distribution
  #       
  #  Output:
  #     vector of length n containing n random numbers from the specified normal distribution
  #
  #
  #First: error.check is called and the error code returned is saved in error
  error <- check.error(n=n,mean=mean,sd=sd)
  #Check: if the error code is not 0, ie. an error has been detected, move to the stop command
  if(error != 0){
    #stop the execution and return the error message as requested
    stop('invalid arguments')
  }
  #If everything is fine continue with execution. Set the iterative variable i to 1
  i <- 1
  # now repeat the process until i reaches n, when n numbers have been generated (note that i will increase by 2, as pairs are generated)
  while(i<=n) {
    #set up the repeat loop so variables can be checked according to our criterion and recomputed if necessary
    repeat{
      #define U as a vector of length 2 containing to random numbers from a uniform distriution from 0 to 1 
      U <- runif(2)
      #Next transform U to the unit square
      U <- 2*U-1
      #set up w as defined in the algorithm by using the individual squared elements of U and adding them up
      w <- (U[1]^2+U[2]^2)
      #if our condition is fulfilled break the loop and continue with execution below.
      #otherwise generate a new set of numbers and try again
      if(w<=1) break
    }
    #define v as laid out in the algorithm from w
    v <- sqrt(((-2)*log(w))/w)
    #store a pair of valid numbers in x_temp, by mutiplying U with v as laid out in the algorithm
    #multiply by the standard deviation and add the mean to arrive at a final result
    x_temp <- U*v*sd+mean
    #if this is the first run of the while loop, store x_temp in a new result vector X
    if(i==1) { 
      X <- x_temp
    }
    #if this is not the first run of the while loop, add the current x-temp at the end of x to arrive at a full result vector of required length or (required length + 1)
    else {
      X <- c(X,x_temp)
    }
    #add 2 to the iterative variable to indicate that the length of X has increased by 2 and a pair of randoms has been generated
    i <- i+2
  }
  #now if an odd number of randoms has been requested, the length of X is now exactly 1 too high (ie. 3 requested, length(x)=4)
  #to avoid getting the wrong number of results, cut the length of X down to n, thus discarding one generated number
  X <- X[1:n]
  #return the result vector X as requested
  return(X)
}


#Similarly, define the general function with the additional parameter method
general.rnorm <- function(method,n,mean=0,sd=1) {
  #  
  #  Purpose:
  #     general.rnorm incorporates Marsaglia's and Bray's method, the Box-Mueller algortihm or the central limit theorem to find random numbers from a normal distribution.
  #     
  #
  #  Input:
  #     method - defines which method is used to find the algorithm:
  #         1 - Marsaglia's and Bray's method
  #         2 - Box-Mueller algorithm
  #         3 - Central Limit Theorem
  #     n - number of random numbers to be generated
  #     mean - default value is 0 - define the mean of the normal distribution the random values are taken from
  #     sd - default value 1 -  standard deviatoion of the normal distribution
  #       
  #  Output:
  #     vector of length n containing n random numbers from the specified normal distribution obtained with the specified method
  #
  #
  #the error check runs identically to above but including the method parameter
  error <- check.error(method=method,n=n,mean=mean,sd=sd)
  if(error != 0){
    stop('invalid arguments')
  }
  #just like in the my.rnorm function, set an iterative variable i to 1
  i <- 1
  #now check which method to use with an if.. else if..else if. else construct
  if(method==1){
    #if method 1 is requested simply call my.rnorm and save the result in the variable X. saves lines of code.
    X <- my.rnorm(n=n,mean=mean,sd=sd)
  } else if(method==2){
    #for method 2 set up a while loop for n/2 iterations
    while(i<=n){
      #define U as a vector of length 2 containing to random numbers from a uniform distriution from 0 to 1 
      U <- runif(2)
      #define the random numbers in accordance with the algorithm
      X1 <- sin(2*pi*U[1])*sqrt((-2)*log(U[2]))
      X2 <- cos(2*pi*U[1])*sqrt((-2)*log(U[2]))
      #if this is the first run of the while loop, store x1 and x2 in a new result vector X
      if(i==1) { 
        X <- c(X1,X2)
      }
      #if this is not the first run of the while loop, add the current x-temp at the end of x to arrive at a full result vector of required length or (required length + 1)
      else {
        X <- c(X,X1,X2)
      }
      #add 2 to the iterative variable to indicate that the length of X has increased by 2 and a pair of randoms has been generated
      i <- i+2
    }
    #now if an odd number of randoms has been requested, the length of X is now exactly 1 too high (ie. 3 requested, length(x)=4)
    #to avoid getting the wrong number of results, cut the length of X down to n, thus discarding one generated number
    X <- X[1:n]
    #scale the result
    X <- X*sd+mean
  } else if(method==3){
    #for the CLT method set up a loop with n iterations
    while(i<=n){
      #set U to a vector of 16 randomly generated numbers from a uniform distribution
      U <- runif(16)
      #sum up the variables in accordance with the method to find one random vaariable from a normal distribution
      X_temp <- (sum(U)-8)*sqrt(12/16)
      #create a result vector like above
      if(i==1) { 
        X <- X_temp
      }
      else {
        X <- c(X,X_temp)
      }
      i <- i+1
    }
    #scale the result
    X <- X*sd+mean
  } else{
    stop('invalid arguments')
  }
  #return the result vector X as requested
  return(X)
}

check.function <- function(siglevel=0.05,no=10,size=1000,compare='yes',m=0,sd=1){
  #  
  #  Purpose:
  #     The purpose of this function is to assess the output as well as the functions in this assignment.
  #     This function will generate (no=10) samples of (size=1000) random numbers each and perform a Shapiro - Wilk Normality test with significance level (siglevel=0.05) on all of them.
  #     This is repeated for all 3 methods. If the user requests the function will also compare the results to the R inbuilt rnorm function to check for a significantly better or worse result.
  #     The function will also plot quantile-quantile plots for a random sample of all 4 methods to check whether the distribution is properly normal or not (ie. resembling a straight line)
  #
  #  Input:
  #     siglevel - significance level at which to reject a given sample
  #     no - number of samples to test for each method
  #     size - size of each sample to test
  #     compare - if set to 'yes', the function compares this to the no of rejected samples from the inbuilt r generator
  #
  #  Output:
  #     The function tells the user how it is teasting and then states the number of rejected sampes for each method
  #     Also, the function generates 4 q-q plots. The user can then compare how well the samples align to a theoretical normal distribution.
  #
  #
  print(c('Testing at significance level of ',siglevel,' with ',no,' samples of size ',size))
  for(n in 1:3){
    i<-1
    X<-0
    print(c('Currently testing method:',n))
    repeat{
      A <- shapiro.test(general.rnorm(n,size,mean=m,sd=sd))
      i<-i+1
      if(A[2]<siglevel) {
        X<-X+1
      }
      if(i >= no){
        print(c('No. of rejected samples at significance level:',X))
        break
      }
    }
  }
  if(compare=='yes'){
    print('Compare to R inbuilt function rnorm:')
    i<-1
    X<-0
    repeat{
      A <- shapiro.test(rnorm(size,m,sd))
      i<-i+1
      if(A[2]<siglevel) {
        X<-X+1
      }
      if(i >= no){
        print(c('No. of rejected samples at significance level:',X))
        break
      }
    }
  }
  # 4 figures arranged in 2 rows and 2 columns
  attach(mtcars)
  par(mfrow=c(2,2))
  #generate the plots for all 4 samples and add a line through the data for illustration purposes
  qqnorm(general.rnorm(1,size,mean=m,sd=sd), main="Marsaglia and Bray's Method")
  qqline(general.rnorm(1,size,mean=m,sd=sd))
  qqnorm(general.rnorm(2,size,mean=m,sd=sd), main="Box-Mueller Algorithm")
  qqline(general.rnorm(2,size,mean=m,sd=sd))
  qqnorm(general.rnorm(3,size,mean=m,sd=sd), main="Central Limit Theorem")
  qqline(general.rnorm(3,size,mean=m,sd=sd))
  qqnorm(rnorm(size,m,sd), main="Inbuilt R Generator")
  qqline(rnorm(size,m,sd))
  detach(mtcars)
}


