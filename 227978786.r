# Matriculation number 140000628

# I confirm that the attached is my own work, except where clearly indicated in the text.

# I defined functions within functions because 'my.rnorm should be a drop-in replacement for rnorm', so it should be self-contained.

# Define the function with arguments n (number of values to return), mean (mean of values) and sd (standard deviation of values).
# Set the default by doing mean=0 and sd=1.
my.rnorm<-function(n,mean=0,sd=1){
  # Check the input arguments are all numeric.
  arguments<-c(n,mean,sd)
  for(i in 1:3){
    if(!is.numeric(arguments[i])) stop('invalid arguments')
  }
  # Check the input n is an integer.
  if(!n==as.integer(n)) stop('invalid arguments')
  # Check the input n is greater than zero.
  if(n<=0) stop('invalid arguments')
  
  # Define this function to avoid repeating code.
  # This function generates two random normal deviates using the Marsaglia and Bray algorithm.
  generate.deviates.method1<-function(){
    # Generate a pair of uniform deviates distributed over [0,1].
    uniform.deviates<-runif(2,0,1)
    # Transform the deviates so they're distributed over [-1,1], the unit square.
    uniform.deviates<-2*uniform.deviates-1
    # While these values are outside the unit circle, reject them and generate a new pair.
    while((uniform.deviates[1])^2+(uniform.deviates[2])^2>1){
      uniform.deviates<-runif(2,0,1)
      uniform.deviates<-2*uniform.deviates-1
    }
    # Define w.
    w<-(uniform.deviates[1])^2+(uniform.deviates[2])^2
    # Define v.
    v<-sqrt((-2*log(w))/w)
    # Define the pair of N(0,1) distributed variates.
    new.normal.deviates<-uniform.deviates*v
    # Return this pair of independent normally distributed deviates.
    return(new.normal.deviates)
  }
  
  # Create an empty vector to store the random normal deviates in.
  normal.deviates<-c()

  # Generate pairs of deviates for however many times 2 divides n (excluding the remainder).
  if(n>1){
    for(i in 1:((n-n%%2)/2)){
      # Generate the random normal deviates.
      new.normal.deviates<-generate.deviates.method1()
      # Add this pair to the vector of normal deviates.
      normal.deviates<-c(normal.deviates,new.normal.deviates)
    }
  }
  # Then if n is odd we need one more random deviate.
  # This says 'if n mod 2 = 1'.
  if(n%%2==1){
    # Generate the random normal deviates.
    new.normal.deviates<-generate.deviates.method1()
    # Now have a pair of deviates, so choose one from the pair randomly.
    # Choose 1 or 2 randomly, then take the first or second element of the pair accordingly.
    j<-sample(c(1,2),1)
    # Add this one random deviate to the list.
    normal.deviates<-c(normal.deviates,new.normal.deviates[j])
  }
  # Transform the N(0,1) deviates into values with mean and standard deviation as defined in the input.
  # Multiply by the standard deviation and then add the mean.
  normal.deviates<-normal.deviates*sd+mean
  # Return the normal deviates.
  return(normal.deviates)
}


# For the general.rnorm function I have repeated the code from my.rnorm (for method 1), as opposed to calling my.rnorm within this function.
# I did this in order to make general.rnorm self-contained.

# Define a function with the same arguments as my.rnorm, with one additional argument method (set default 1 by method=1).
general.rnorm<-function(n,mean=0,sd=1,method=1){
  # Check the input arguments are all numeric.
  arguments<-c(n,mean,sd)
  for(i in 1:3){
    if(!is.numeric(arguments[i])) stop('invalid arguments')
  }
  # Check the input n is an integer.
  if(!n==as.integer(n)) stop('invalid arguments')
  # Check the input n is greater than zero.
  if(n<=0) stop('invalid arguments')
  
  # Create an empty vector to store the random normal deviates in.
  normal.deviates<-c()
  
  # Define a function that generates two random normal deviates, to avoid repeating code, as in my.rnorm (see above for comments).
  generate.deviates.method1<-function(){
    uniform.deviates<-runif(2,0,1)
    uniform.deviates<-2*uniform.deviates-1
    while((uniform.deviates[1])^2+(uniform.deviates[2])^2>1){
      uniform.deviates<-runif(2,0,1)
      uniform.deviates<-2*uniform.deviates-1
    }
    w<-(uniform.deviates[1])^2+(uniform.deviates[2])^2
    v<-sqrt((-2*log(w))/w)
    new.normal.deviates<-uniform.deviates*v
    return(new.normal.deviates)
  }
  
  # Method 1 - Marsaglia and Bray.
  # For comments see my.rnorm function above, I've used the same code.
  if(method==1){
    if(n>1){
      for(i in 1:((n-n%%2)/2)){
        new.normal.deviates<-generate.deviates.method1()
        normal.deviates<-c(normal.deviates,new.normal.deviates)
      }
    }
    if(n%%2==1){
      new.normal.deviates<-generate.deviates.method1()
      j<-sample(c(1,2),1)
      normal.deviates<-c(normal.deviates,new.normal.deviates[j])
    }
  }
  
  # Method 2 - Box and Mueller.
  # Define a function using the Box and Mueller algorithm to generate (approximate) random normal deviates.
  generate.deviates.method2<-function(){
    # Generate a pair of uniformly distributed random deviates.
    uniform.deviates<-runif(2,0,1)
    # Transform them to polar coordinates.
    x1<-sin(2*pi*uniform.deviates[1])*sqrt(-2*log(uniform.deviates[2]))
    x2<-cos(2*pi*uniform.deviates[1])*sqrt(-2*log(uniform.deviates[2]))
    # Put these new values together in a list.
    new.normal.deviates<-c(x1,x2)
    # Return this pair of random normally distributed deviates.
    return(new.normal.deviates)
  }
  if(method==2){
    # Generate pairs of deviates for however many times 2 divides n (excluding the remainder).
    if(n>1){
      for(i in 1:((n-n%%2)/2)){
        # Generate the random normal deviates.
        new.normal.deviates<-generate.deviates.method2()
        # Add this pair to the vector of normal deviates.
        normal.deviates<-c(normal.deviates,new.normal.deviates)
      }
    }
    # Then if n is odd we need one more random deviate.
    if(n%%2==1){
      # Generate the random normal deviates.
      new.normal.deviates<-generate.deviates.method2()
      # Now have a pair of deviates, so choose one from the pair randomly.
      # Choose 1 or 2 randomly, then take the first or second element of the pair accordingly.
      j<-sample(c(1,2),1)
      # Add this one random deviate to the list.
      normal.deviates<-c(normal.deviates,new.normal.deviates[j])
    }
  }
  
  # Method 3 - central limit theorem.
  if(method==3){
    # This algorithm generates one number so perform it n times.
    for(i in 1:n){
      # Generate 16 uniformly distributed random deviates over [0,1].
      uniform.deviates<-runif(16,0,1)
      # Calculate the mean of these independent random deviates, which will be approximately normally distributed.
      new.normal.deviate<-(sum(uniform.deviates)-8)*sqrt(12/16)
      # Add this value to the vector of normal deviates.
      normal.deviates<-c(normal.deviates,new.normal.deviate)
    }
  }
  
  # Transform the N(0,1) deviates into values with given mean and standard deviation.
  normal.deviates<-normal.deviates*sd+mean
  # Return the normal deviates.
  return(normal.deviates)
}

# TESTS

# Test for n numbers in the output.
test1<-function(n,mean=0,sd=1,general=FALSE,method=1){
  # If general=TRUE then the functions tests the general.rnorm function, otherwise it tests my.rnorm.
  if(general==FALSE){
    x<-my.rnorm(n,mean,sd)
  } else{
    x<-general.rnorm(n,mean,sd,method)
  }
  return((length(x)==n & is.numeric(x)))
}
# Returns TRUE if the function returns n numeric values.

# Test most numbers are within 3 standard deviations of the mean.
test2<-function(n,mean=0,sd=1,general=FALSE,method=1){
  # Only allow n greater than or equal to 30.
  if(n<30) stop('n needs to be greater than or equal to 30 for this test to work effectively')
  if(general==FALSE){
    x<-my.rnorm(n,mean,sd)
  } else{
    x<-general.rnorm(n,mean,sd,method)
  }
  return((sum(-3*sd<x & x<3*sd)/n)>=0.9)
  # Returns TRUE if 90% of the output numbers are all within 3 standard deviation of the mean.
  # I'm using a generous 90% to allow for sample errors.
}


# Test the output sample mean is close to the input mean.
test3<-function(n,mean=0,sd=1,general=FALSE,method=1){
  # Need n to be large enough to have a good sample of deviates.
  if(n<5) stop('n needs to be greater than or equal to 5 for this test to work effectively')
  if(general==FALSE){
    x<-my.rnorm(n,mean,sd)
  } else{
    x<-general.rnorm(n,mean,sd,method)
  }
  return(mean-sd<mean(x) & mean(x)<mean+sd)
}
# Returns TRUE if the sample mean of the output deviates is within one standard deviation of the input mean.

# Test the output appear to come from a normal distribution, by plotting a histogram of the output.
test4<-function(n,mean=0,sd=1,general=FALSE,method=1){
  if(general==FALSE){
    x<-my.rnorm(n,mean,sd)
  } else{
    x<-general.rnorm(n,mean,sd,method)
  }
  # Only want to view one plot at once.
  par(mfrow=c(1,1))
  hist(x)
  return(NULL)
}
# The histogram produced should look like that of a normal distribution.

# Test for normality by using the pnorm function on the output from my.rnorm and rnorm.
test5<-function(n,mean=0,sd=1,general=FALSE,method=1){
  # Need n to be at least 100.
  if(n<100) stop('n needs to be greater than or equal to 100 for this test to work effectively')
  if(general==FALSE){
    x<-my.rnorm(n,mean,sd)
  } else{
    x<-general.rnorm(n,mean,sd,method)
  }
  # Do this so we can see two plots at once.
  par(mfrow=c(1,2))
  hist(pnorm(x,mean,sd),main='my.rnorm');hist(pnorm(rnorm(n,mean,sd)),main='rnorm')
  return(NULL)
}
# Can compare the two plots to see what my.rnorm should look like against rnorm.
# Both plots should be fairly constant all the way across, but this improves as n increases.

# Kolmogorov-Sminov test to examine whether the my.rnorm output comes from a normal distribution.
# The null hypothesis is that the my.rnorm output forms a random sample from a normal distribution.
test6<-function(n,mean=0,sd=1,general=FALSE,method=1){
  if(general==FALSE){
    x<-my.rnorm(n,mean,sd)
  } else{
    x<-general.rnorm(n,mean,sd,method)
  }
  y<-ks.test(x,"pnorm",mean=mean,sd=sd,alternative="two.sided")
  if(y$p.value<0.05){
    z<-paste('p-value ',y$p.value,' is less than 0.05 so reject the null hypothesis that the my.rnorm output is normally distributed')
  } else{
    z<-paste('p-value ',y$p.value,' is greater than 0.05 so there is no evidence to reject the null hypothesis that the my.rnorm output is normally distributed')
  }
  return(z)
}
# Returns the p-value and an explanation of the p-value.

# QQ plot tests for normality.
test7<-function(n,mean=0,sd=1,general=FALSE,method=1){
  if(general==FALSE){
    x<-my.rnorm(n,mean,sd)
  } else{
    x<-general.rnorm(n,mean,sd,method)
  }
  par(mfrow=c(1,1))
  # Plot a QQ plot and add the straight line x=y, for comparison.
  qqnorm(x);abline(0,1)
  return(NULL)
}
# If the plotted points are close to the straight line then the normal distribution is a good fit for the output of my.rnorm.

# Test that the default mean is 0 and the default standard deviation is 1, by testing if the sample mean is within [-1,1].
# This test is approximate, it doesn't tell us for sure that the defaults are 0 and 1.
test8<-function(n,general=FALSE,method=1){
  # Need n to be large enough to have a good sample of deviates.
  if(n<5) stop('n needs to be greater than or equal to 5 for this test to work effectively')
  if(general==FALSE){
    x<-my.rnorm(n)
  } else{
    x<-general.rnorm(n=n,method=method)
  }
  return(-1<mean(x) & mean(x)<1)
}
# Returns TRUE if the sample mean of the output deviates is within [-1,1].