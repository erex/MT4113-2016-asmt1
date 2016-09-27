#I confirm that the attached is my own work, except where clearly indicated in the text.

##################### MT4113 Computing in Statistics: Assignment 1 #####################


##################### my.rnorm Function ################################################

my.rnorm <- function(n, mean=0, sd=1){                                 #Marsaglia & Bray's Method / function called my.rnorm
  
  #error check for n
  if ( !is.numeric(n) ) { stop("invalid arguments") }                  #if n isn't numeric then error message
  if ( (n<1) | n!=round(n) ) { stop("invalid arguments") }             #n<1 or n isn't an integer then error message
  
  #error check for mean
  if ( !is.numeric(mean) ) { stop("invalid arguments") }               #if mean isn't numeric then error message
  
  #error check for sd
  if ( !is.numeric(sd) ) { stop("invalid arguments") }                 #if sd isn't numeric then error message
  if ( sd<0 ) { stop("invalid arguments") }                            #if sd negative then error message
  
  dev <- seq(1,n)                                                      #create a vector length=n to contain the deviates
  
  #m is used in the next for loop
  if (n%%2==0) {
    m <- n/2                                                           #if n is even set m=n/2
  }else{
    m <- (n+1)/2                                                         #if n is odd set m=(n+1)/2
  }
 
  #if n is even loop through the following for loop to obtain m=n/2 pairs of deviates
  
  #if n is odd loop through the following for loop to obtain m=(n+1)/2 pairs of deviates
  #but only use one deviate from the last pair of deviates
  
  for (i in 1:m) {                                                      #start of for loop through i
    
    w <- 2                                                              #set w=2 so we're able to enter the while statement                                               
    #rejection step
    while ( w > 1 ) {                                                   #start of while loop
      u1 <- runif(1)                                                    #generate deviate from uniform(0,1)
      u2 <- runif(1)                                                    #generate 2nd deviate from unifrom(0,1)
      #Transform u1 and u2 to the unit square
      u1 <- 2*u1 - 1
      u2 <- 2*u2 - 1
      #define w
      w <- u1^2 + u2^2
    }                                                                   #end of while statement
    
    v <- sqrt( -2*log(w) / w )                                          #define v as sqrt( -2*log(w) / w )
    #define x1 and x2 as xi=ui*v, xi~N(0,1)
    x1 <- u1*v
    x2 <- u2*v
    #transform xi so xi~N(mean,sd^2)
    x1 <- x1*sd + mean
    x2 <- x2*sd + mean
    
    if(n%%2!=0 & i==m){                                                 #if n is odd and i=m i.e. our last iteration
      #put x1 into the last entry of dev. don't use x2 so we get an odd number of deviates
      dev[n] <- x1
    }else{                                                              #if n is even, or (n is odd and i!=m)
      #put x1 and x2 into dev
      dev[2*i-1] <- x1
      dev[2*i] <- x2
    }
  }                                                                     #end of for loop through i
  
  return(dev)                                                           #return the vector dev of deviates
}                                                                       #end of function my.rnorm

##################### general.rnorm Function ###########################################

general.rnorm <- function(n, mean=0, sd=1, method=1){                   #function called general.rnorm
  
  #error check for n
  if ( !is.numeric(n) ) { stop("invalid arguments") }                   #if n isn't numeric then error message
  if ( (n<1) | n!=round(n) ) { stop("invalid arguments")}               #if n<1 or n isn't an integer then error message
  
  #error check for mean
  if ( !is.numeric(mean) ) { stop("invalid arguments") }                #if mean isn't numeric then error message
  
  #error check for sd
  if ( !is.numeric(sd) ) { stop("invalid arguments") }                  #if sd isn't numeric then error message
  if ( sd<0 ){ stop("invalid arguments") }                              #if sd negative then error message
  
  #error check for method
  if ( !is.numeric(method) ) { stop("invalid arguments") }              #if method isn't numeric then error message
  if (method!=1 & method!=2 & method!=3) { stop("invalid arguments") }  #if method isn't equal to any of 1,2 or 3 then error message
  
  
  
  #method 1, Marsaglia & Bray's Method
  if (method==1) {                                                      #if method 1 has been chosen to be used
    dev <- my.rnorm(n, mean, sd)                                        #calculate the deviates using the function my.rnorm
    return(dev)                                                         #return the vector of deviates
  }                                                                     #end of method 1
  
  
  
  #method 2, Box-Mueller Algorithm
  else if (method==2) {                                                 #if method 2 has been chosen to be used

    dev <- seq(1,n)                                                     #create a vector with length=n to contain the deviates
    
    #m is used in the next for loop
    if (n%%2==0) {
      m <- n/2                                                          #if n is even set m=n/2
    }else{
      m <- (n+1)/2                                                      #if n is odd set m=(n+1)/2
    }
    
    #if n is even loop through the following for loop to obtain m=n/2 pairs of deviates
    
    #if n is odd loop through the following for loop to obtain m=(n+1)/2 pairs of deviates
    #but only use one deviate from the last pair of deviates
    
    for (i in 1:m) {                                                    #start of for loop through i
      u1 <- runif(1)                                                    #generate deviate from uniform(0,1)
      u2 <- runif(1)                                                    #generate 2nd deviate from unifrom(0,1)
      #define x1 and x2 , xi~N(0,1)
      x1 <- sin( 2*pi*u1 ) * sqrt( -2*log(u2) )
      x2 <- cos( 2*pi*u1 ) * sqrt( -2*log(u2) )
      #transform xi so xi~N(mean,sd^2)
      x1 <- x1*sd + mean
      x2 <- x2*sd + mean
      
      if(n%%2!=0 & i==m){                                               #if n is odd and i=m i.e. our last iteration
        #put x1 into the last entry of dev. don't use x2 so we get an odd number of deviates
        dev[n] <- x1
      }else{                                                            #if n is even, or (n is odd and i!=m)
        #put x1 and x2 into dev
        dev[2*i-1] <- x1
        dev[2*i] <- x2
      }
    }                                                                   #end of for loop through i
    
    return(dev)                                                         #return the vector dev which contains the deviates
  }                                                                     #end of method 2
  
  
  
  #method 3, Central Limit Theorem
  else {                                                                #if method 3 has been chosen to be used
    
    dev <- seq(1,n)                                                     #create a vector length=n to contain the deviates
    
    for (i in 1:n) {                                                    #loop over number of deviates wanted i.e. n / start of for loop through i
      ui <- seq(1,16)                                                   #create a vector to store the 16 ui
      
      for (j in 1:16){                                                  #loop 16 times to create 16 ui / start of for loop through j
        ui[j] <- runif(1)                                               #calculate ui
      }                                                                 #end of for loop through j
      
      x <- (sum(ui) - 8) * sqrt(12/16)                                  #calculate x, x~N(0,1)
      #transform x so x~N(mean,sd^2)
      x <- x*sd + mean
      dev[i] <- x                                                       #put x into dev
    }                                                                   #end of for loop through i 
    
    return(dev)                                                         #return the vector dev which contains the deviates
  }                                                                     #end of method 3
}                                                                       #end of function general.rnorm

##################### Functions Which Test general.rnorm and my.rnorm ##################

# 1)check number of deviates = n

test.length <- function(n, mean=0, sd=1, method=1){                     #function called test.length

    dev <- general.rnorm(n=n, mean=mean, sd=sd, method=method)          #obtain the deviates and call them dev
    if( length(dev)==n ) {                                              #beginning of if statement
      return("Correct number of deviates produced")                     #if number of deviates = n then return the character
    }else{
      return("Incorrect number of deviates produced")                   #if number of deviates != n then return the character
    }                                                                   #end of if statement  
}                                                                       #end of function test.length

# 2)check the deviates produced are numeric

test.dev.numeric <- function(n, mean=0, sd=1, method=1){                #function called test.dev.numeric
  
  dev <- general.rnorm(n=n, mean=mean, sd=sd, method=method)            #obtain the deviates and call them dev
  if( is.numeric(dev) ) {                                               #beginning of if statement
    return("Deviates are numeric")                                      #if the deviates are numeric then return the character
  }else{
    return("Deviates aren't numeric")                                   #if the deviates aren't numeric then return the character
  }                                                                     #end of if statement
}                                                                       #end of function test.dev.numeric

# 3)check a vector is returned

test.vector <- function(n, mean=0, sd=1, method=1){                     #function called test.vector
  
  dev <- general.rnorm(n=n, mean=mean, sd=sd, method=method)            #obtain the deviates and call them dev
  if( is.vector(dev) ) {                                                #beginning of if statement
    return("Deviates are contained in a vector")                        #if the deviates are in a vector then return the character
  }else{
    return("Deviates aren't contained in a vector")                     #if the deviates aren't in a vector then return the character
  }                                                                     #end of if statement
}                                                                       #end of function test.vector

# 4)makes a histogram and normal q-q plot to look at and see if the histogram looks like deviates from a normal distribution with 
#   given mean and sd and check visually for linearity in the normal q-q plot which could suggest normality (note:but neither prove normality)

test.visual <- function(n, mean=0, sd=1, method=1){                     #function called test.visual
  dev <- general.rnorm(n=n, mean=mean, sd=sd, method=method)            #obtain the deviates and call them dev
  par(mfrow=c(1,2))                                                     #fit 2 graphs on page (1 row, 2 columns)
  qqnorm(dev)                                                           #normal q-q plot
  qqline(dev)                                                           #line we expect normal deviates to follow
  hist(dev, xlab="Deviates", main="Histogram Of Deviates")              #create the histogram  
  return(NULL)                                                          #function doesn't return anything it just creates a histogram an q-q plot
}                                                                       #end of function test.visual

# 5)calculates the mean and sd from the deviates produces
#   and then you can compare that to the mean and sd which were wanted

test.mean.sd <- function(n, mean=0, sd=1, method=1){                    #function called test.mean.sd
  dev <- general.rnorm(n=n, mean=mean, sd=sd, method=method)            #obtain the deviates and call them dev
  calc_mean_dev <- mean(dev)                                            #calculate the mean of the deviates
  calc_sd_dev <- sd(dev)                                                #calculate the sd of the deviates
  return(list(n=n, entered_mean=mean, calculated_mean_of_deviates=calc_mean_dev, entered_sd=sd, calculated_sd_of_deviates=calc_sd_dev))    #return the calculated and entered, mean and sd in a list
}                                                                       #end of function called test.mean.sd

# 6) uses the shapiro-wilk test to test for normality H0: the deviates come from a normal distribution, H1: the deviates don't come from a normal distribution
#    (notes sample size must be between 3 and 5000 for shapiro.test() to work)

test.normality <- function(n, mean=0, sd=1, method=1){                  #function called test.normality
  dev <- general.rnorm(n=n, mean=mean, sd=sd, method=method)            #obtain the deviates and call them dev
  swt <- shapiro.test(dev)                                              #call the results from the shapiro-wilk test swt
  return(swt)                                                           #return the results from the shapiro-wilk test
}                                                                       #end of function called test.normality


#Check the following by using the functions my.rnorm and general.rnorm to see if an error message of "invalid arguments" is produced: 

#7)check if n isn't numeric then error
#8)check if n<1 then error
#9)check if n isn't a natural number then error
#10)check if mean isn't numeric then error
#11)check if sd isn't numeric then error
#12)check if sd<0 then error
#13)check if method isn't numeric then error
#14)check if method isn't 1,2 or 3 then error