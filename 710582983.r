#I confirm that the attached is my own work, except where clearly indicated in the text

my.rnorm <- function(n,mean=0,sd=1){
  #stops function when n,mean, or sd are non-scalars
  if ((length(n)!=1)|(length(mean)!=1)|(length(sd)!=1)) {
    stop('invalid argument')
  }
  #stops function when n is not a multiple of 2 and greater than 0
  if ((n<2) | (n %% 2!=0)) {
    stop('invalid argument')
  }
  #stops function if mean or sd are invalid
  if ((mean<0) | (sd<0)) {
    stop('invalid argument')
  }
  #maximum n is 1000 to stop large numbers making the function take a long time to run
  if (n>1000) {
    n<-1000
  }

  #"values" is vector which stores deviates
  values<-c()
  #adds deviates until there are n in "values"
  while (length(values)<n){
    #makes U1 and U2 and transforms them to unit square
    U <- (runif(2))*2 - 1
    #calculates w and adds X1and X2 to "values" if w<=1
    w<-U[1]**2+U[2]**2
    if (w<=1){
      v <- sqrt((-2*log(w))/w)
      X1 <- U[1]*v*sd - mean
      X2 <- U[2]*v*sd - mean
      values<-c(values,X1,X2)
    }
  }
  #function returns the vector of deviates
  return(values)
}


general.rnorm <- function(n,mean=0,sd=1,method=1) {
  #reuses some error checks from my.rnorm
  #these checks are done twice for method 1, but I think this way saves
  #reusing code the most without having to making a new function
  if ((length(n)!=1)|(length(mean)!=1)|(length(sd)!=1)) {
    stop('invalid argument')
  }
  if (n>1000) {
    n<-1000
  }
  if ((mean<0) | (sd<0)) {
    stop('invalid argument')
  }
  
  #if loops are used to select the correct method
  if (method == 1) {
    #simply uses the my.rnorm function
    return(my.rnorm(n,mean,sd))
  } else if (method == 2) {
    #this method generates pairs of values, therefore n must be even and larger than 0
    if ((n<2) | (n %% 2!=0)) {
      stop('invalid argument')
    }
    #similar to "values" in my.rnorm
    values<-c()
    #calculates X1 and X2 and adds them to the vector until it has n values
    while (length(values)<n){
      U<-runif(2)
      X1<-(sin(2*pi*U[1]))*sqrt(-2*log(U[2]))*sd - mean
      X2<-(cos(2*pi*U[1]))*sqrt(-2*log(U[2]))*sd - mean
      values<-c(values,X1,X2)
    }
    #returns vector of deviates
    return(values)
  } else if (method == 3) {
    #n only has to be an integer greater than 0
    if ((n<1) | (n %% 1!=0)){
      stop('invalid argument')
    }
    #similar to the other methods
    values<-c()
    while (length(values)<n){
      U<-runif(16)
      X<-(sum(U)-8)*sqrt(12/16)*sd - mean
      values<-c(values,X)
    }
    return(values)
  } else {
    #if method is not 1, 2, or 3 the function will stop and give the error message
    #I was unsure whether or not to make it use the default method in this case
    stop('invalid argument')
  }
}


#test functions
#there are only 3 arguments for my.rnorm so the only way I could think of to test the function
#was to see what happened when different values for the arguments were entered.
#these included non-numerical values, symbols, decimals, fractions, and negative numbers.
#the functions for general.rnorm were used in a similar way.

#used to see what the results are with different values for n
my.rnorm.test1<-function(n){
  x<-my.rnorm(n)
  return(x)
}

#used to see what the results are with different values for sd with n kept fixed at 20 so its easy to read 
my.rnorm.test2<-function(mean){
  x<-my.rnorm(n=20,mean=mean)
  return(x)
}

#used to see what the results are with different values for sd with n kept fixed at 20 so its easy to read
my.rnorm.test3<-function(sd){
  x<-my.rnorm(n=20,sd=sd)
  return(x)
}

#used to check that the results are all numbers for different values of n, mean, and sd
my.rnorm.test4<-function(n,mean=0,sd=1){
  x<-my.rnorm(n,mean,sd)
  x<-is.numeric(x)
  return(x)
}


#these functions are basically the same as above
#used to check the same things as for my.rnorm for the different methods
#can also be used to see what output you get for entering different values for method
general.rnorm.test1<-function(n,method){
  x<-general.rnorm(n,method=method)
  return(x)
}

general.rnorm.test2<-function(mean,method){
  x<-my.rnorm(n=20,mean=mean,method=method)
  return(x)
}

general.rnorm.test3<-function(sd,method){
  x<-my.rnorm(n=20,sd=sd,method=method)
  return(x)
}

general.rnorm.test4<-function(n,mean=0,sd=1,method){
  x<-my.rnorm(n,mean,sd,method=method)
  x<-is.numeric(x)
  return(x)
}