#I confirm that the attached is my own work, except where clearly indicated in the text.


my.rnorm<- function(n,mean=0,sd=1) { 
# Purpose:
#   Defines a function, namely "my.rnorm", which returns a vector of n pseudo-random values 
#    from a normal distribution with mean=mean and standard deviation=sd,
#    using the Marsaglia and Bray algorithm with arguments n, mean and sd
  
# Inputs:
#   n- a numeric scalar integer greater than or equal to 1
#   mean- a numeric scalar 
#   sd- a numeric scalar greater than or equal to 0
  
#  Outputs:
#    vector of n pseudo-random deviates
  
  
  #  Creates error traps which stop the function and show "invalid arguments" 
  #   when the inputs are incorrect
  #  First check that n, mean and sd are all numeric scalar arguments
  #  If true ensure that if n is not an integer or n is less than 1 or sd is less than 0
  #   that an error message of "invalid arguments" is shown
  #  If these checks hold true but at least one of n, mean and sd are either non-numeric 
  #   or non-scalar then shows "invalid arguments"  
  if (is.numeric(c(n,mean,sd)) & length(c(n,mean,sd))==3){ 
    if (n%%1!=0|n<1|sd<0){
    stop("invalid arguments")
    }
  }else {
    stop("invalid arguments")
  }
  
  rnum<-c()  # creates an empty vector, "rnum", to store random deviates
  
  # While loop is used to reject random deviates which do not lie within the unit circle i.e w<1
  # Floor used so function works for an odd n
  for (i in 1:(floor((n+1)/2))){
    w<-2 #set w equal to 2 so that w fails the initial while loop check and re-evaluated
    while (w>1) {
      U1<-2*runif(1,0,1)-1  # Change range of random deviates from [0, 1] to [-1, 1]
      U2<-2*runif(1,0,1)-1
      w<-U1^2+U2^2
    }
    
    # Make random deviates normally distributed
    v<-sqrt(-2*log(w)/w)
    X1<-U1*v
    X2<-U2*v
    rnum<-c(rnum,X1,X2)  # Add new random deviates to the current list of random deviates
  }
  return(rnum[1:n]*sd+mean)  # return transformed standard normal values
  # [1:n] used above to ensure that the function returns only the number of random deviates 
  # required and therefore works for an odd n
}


general.rnorm<-function(n,mean=0,sd=1,method=1){
# Purpose:
#  Defines a function, "general.rnorm" returning a vector of pseudo-random 
#   normally-distributed deviates
#   with arguments n, mean=mean, sd= standard deviation and method=algorithm used
  
# Inputs:
#  n- a numeric scalar integer greater than or equal to 1
#  mean- a numeric scalar 
#  sd- a numeric scalar greater than or equal to 0
#  method- a numeric scalar between 1 and 3 whereby 1 corresponds to Marsaglia and Bray algorithim, 
#   2 to Box and Mueller algorithm and 3 corresponds to central-limit theorem algorithm

# Outputs:
#  vector of n pseudo-random deviates
  
  # Creates error traps which stop the function and report "invalid arguments" 
  #  when the inputs are incorrect
  # First check that n, mean, sd and method are all numeric scalar arguments
  # If true ensure that if n is not an integer or n is less than 1 sd is less than 0 or 
  #  method is not in the interval 1:3 that "invalid arguments" is displayed
  # If these checks hold true but at least one of n, mean, sd or method are either non-numeric or 
  #  non-scalar arguments then show "invalid arguments" 
  if (is.numeric(c(n,mean,sd,method)) & length(c(n,mean,sd,method))==4){
    if (n%%1!=0|n<1|sd<0|method<1|method>3){
      stop("invalid arguments")
    }
  }else {
    stop("invalid arguments")
  }
  
  # If the Marsaglia and Bray algorithm is required ie method=1, call the function "my.rnorm" 
  #  to produce the random deviates 
  if (method==1){ 
    return(my.rnorm(n,mean,sd))
  } else if (method==2){ # calculate random deviates using the Box-Mueller algorithm
    rnum<-c() # creates an empty vector, "rnum", to store random deviates
    # Floor used so function works for an odd n
    for (i in 1:(floor((n+1)/2))){
      U1<-runif(1,0,1)
      U2<-runif(1,0,1)
      # Make random deviates normally distributed 
      X1<-sin(2*pi*U1)*sqrt(-2*log(U2))
      X2<-cos(2*pi*U1)*sqrt(-2*log(U2))
      rnum<-c(rnum,X1,X2) # Add new random deviates to the current list of random deviates
    }
    return(rnum[1:n]*sd+mean) # return transformed standard normal values
    # [1:n] used above to ensure that the function returns only the number of random deviates 
    # required and therefore works for an odd n 
    
    # If method is neither 1 nor 2 then calculate random deviates using the Central-Limit 
    #  Theorem algorithm, else can be used here since the error checks dictate that the method input 
    #  must be either 1,2 or 3 and hence there is no need to explicitly state "if method==3"
    }else{
      rnum<-c() # creates an empty vector, "rnum", to store random deviates
      
      # run a loop n times which will calculate 16 random uniform deviates
      for (i in 1:n){ 
        U<-c()
        for (i in 1:16){ 
        U[i]<-runif(1,0,1)
        }
      X<-(sum(U)-8)*sqrt(12/16)  # Make one new random deviate from normal distribution 
      # using 16 random uniform deviates
      rnum<-c(rnum,X) # Add new random deviate to the current list of random deviates
      }
      return(rnum*sd+mean) # return transformed standard normal values 
    }
}


number.and.length.check<-function(){
# Purpose:
#   Checks that when both my.rnorm and general.rnorm receive an input of n=10 
#    that 10 numeric random values are returned
# Inputs:
#   None
# Outputs:
#  "True" if both functions produce 10 numeric random values and
#   "False" if either condition is not met

  x<-my.rnorm(n=10) # set n=10 for both functions being tested
  if (!(length(x)==10 & is.numeric(x))){
    return(FALSE)
  }
  for (i in 1:3){
    y<-general.rnorm(n=10,method=i)
    if (!(length(y)==10 & is.numeric(y))){
      return(FALSE)
    }
  }
  return(TRUE)
}


sd.zero.check<-function(){
# Purpose:
#   Checks that when both my.rnorm and general.rnorm receive an input of sd=0
#   that the resulting random values produced have a mean equal to the mean specified 
# Inputs:
#   None
# Outputs:
#  "True" if the mean of the random values produced is equal to the mean specified in the function
#   and "False" if either condition is not met
  
  #set sd=0 and mean=5, the resulting random values should then have a mean equal to 5 
  x<-my.rnorm(n=10,mean=5,sd=0) 
  if (mean(x)!=5){
    return(FALSE)
  }
  for (i in 1:3){
    y<-general.rnorm(n=10,mean=5,sd=0,method=i)
    if (mean(y)!=5){
      return(FALSE)
    } 
  }
  return(TRUE)
}


mean.check<-function(){
# Purpose:
#   Checks that when both my.rnorm and general.rnorm receive an input of sd=1, large n
#   and mean=100 that the mean value of the random deviates produced is close to 100
# Inputs:
#   None
# Outputs:
#  "True" if the mean of the random values produced is equal to the mean specified in the function
#   and "False" if either condition is not met
  
  x<-my.rnorm(n=1000,mean=100,sd=1)
  if (mean(x)<99.5 | mean(x)>100.5){
    return(FALSE)
  }
  for (i in 1:3){
    y<-general.rnorm(n=1000,mean=100,sd=1,method=i)
    if (mean(y)<99.5 | mean(y)>100.5){
      return(FALSE)
    } 
  }
  return(TRUE)
}


hist.check<-function(){
# Purpose:
#  Plot histograms for each method contained within "my.rnorm" and "general.rnorm" 
#   with 10,000 random deviates
#   in order to provide a visual check of how well the data fit with a 
#   Normal Distribution (should be a bell-shaped curve)
# Inputs:
#  None
# Outputs:
#  One histogram for each method contained within "my.rnorm" and "general.rnorm"
  par(mfrow=c(rows=2, columns=2)) #Set up display window to hold 4 plots
  hist(my.rnorm(n=10000),breaks=100,main="Histogram of my.rnorm",xlab="x")
  for (i in 1:3){
    hist(general.rnorm(n=10000,method=i),breaks=100,main=c("Histogram of general.rnorm - Method", i),xlab="x")
  }
}


QQ.plot.check<-function(){
# Purpose:
#  Produces Q-Q Plots for each method contained within "my.rnorm" and "general.rnorm" 
#   with 10,000 random deviates
#   in order to provide a visual check of how well the data fit with a Normal Distribution
#   (should be a straight line through the origin)
# Inputs:
#  None
# Outputs:
#  Four Q-Q Plots for each method contained within "my.rnorm" and "general.rnorm"
  par(mfrow=c(rows=2, columns=2)) #Set up display window to hold 4 plots
  qqnorm(my.rnorm(n=10000),main="Histogram of my.rnorm",xlab="x")
  for (i in 1:3){
    qqnorm(general.rnorm(n=10000,method=i),main=c("Histogram of general.rnorm - Method", i),xlab="x")
  }
}


invalid.arg.check<-function(){
# Purpose:
#  Test the initial error traps so that when an invalid argument is passed through the function, 
#   an error message is produced
# Inputs:
#  None
# Outputs:
#  True if each invalid argument produces an error message and false if any one fails 
  
  
  # "try" checks if an error message would be produced and then "silent=true" 
  #   supresses this error message
  a<-try(general.rnorm(n=0),silent=TRUE)
  b<-try(general.rnorm(n=10,sd=-1),silent=TRUE)
  c<-try(general.rnorm(n=10,mean="m"),silent=TRUE)
  d<-try(general.rnorm(n=10,method=4),silent=TRUE)
  
  #if the class of each test is not an error then the test is failed 
  if (class(a)!="try-error"|class(b)!="try-error"|class(c)!="try-error"|class(d)!="try-error"){ 
    return(FALSE)
  }
  return(TRUE)
}


pass.test<-function(){
# Purpose:
#  To run a quick check that all tests return True
# Inputs:
#  None
# Outputs:
#  True if each of the 4 tests on my.rnorm and general.rnorm are all true, 
#   if at least one fails the test then returns false
  
  if (invalid.arg.check() & number.and.length.check() & sd.zero.check() & mean.check()){
    return(TRUE)
  } else{
    return(FALSE)
  }
}