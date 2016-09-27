##Assignment 1
##MT4113
#I confirm that the attached is my own work, except where clearly indicated in the text.

 

my.rnorm <- function(n,mean=0,sd=1) {
  if(n<1) stop("invalid arguments")                   #n must be greater or equal to 1
  if(sd<=0) stop("invalid arguments")
  
  pseudo.vec <- c(rep(0,n))                           #A vector of zeros length n
  
  
  is.wholenumber <-                                                     ##
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol    #function gven to me by R, which I have used to test if n is odd or even
  if (is.wholenumber(n/2) == TRUE) {                                    ##
    
    for (i in 1:(n/2)) {                             #Run n/2 times if n is even, as each run gives a pair of values
      
      repeat    {                                    #repeat this section if the condition w<1 is not satisfied
        deviate <- runif(n, min=0, max=1)            #gives n deviates on uniform(0,1).
        U1 <- deviate[1]
        U2 <- deviate[2]
        
        U1 <- (U1*2)-1
        U2 <- (U2*2)-1                               #changes values to uniform(-1,1)
        w <- ((U1^2)+(U2^2))                         #equation given to find w from U1&U2
        
        if (w <1) {                                  #only continues if w is less than 1
          break                                      
        }
      }
      
      v <- ((-2*log(w))/w)^0.5                       #given equation to find v from w.
      
      X1 <- U1*v                                     #final values we want in our vector
      X2 <- U2*v
      
      
      
      pseudo.vec[2*i-1] <- X1                         #sub each X1 into the odd positions in the vector, so pseudo.vec[1,3,5,7,etc.], depending on i.
      pseudo.vec[2*i] <- X2                           #sub each X2 into the even positions in the vector, so pseudo.vec[2,4,6,8,etc.], depending on i.
    }
  } else {
    pseudo.vector <- c(rep(0,(n+1)))                  #vector of n+1 deviates
    for (i in 1:((n/2)+0.5)) {                        #if n is odd, find pairs of deviates for (n-1)/2
      
      repeat    {                                     #this gives us n-1 values, as we get pairs of values.
        deviate <- runif((n+1), min=0, max=1)
        U1 <- deviate[1]
        U2 <- deviate[2]
        
        U1 <- (U1*2)-1
        U2 <- (U2*2)-1
        w <- ((U1^2)+(U2^2))
        
        if (w <1) {
          break
        }
      }
      
      v <- ((-2*log(w))/w)^0.5
      
      X1 <- U1*v
      X2 <- U2*v
      
      
      
      pseudo.vector[2*i-1] <- X1                          #similar to last section for even n.
      pseudo.vector[2*i] <- X2
      
    }
    pseudo.vec <- pseudo.vector[1:n]                      #our final vector with n values, (eliminates the last value, as values come in pairs)
  }
  answer <- (pseudo.vec+mean)*sd                          #if we want to change mean and sd from 0 and 1
  return(answer)                                          #returns our vector
}









general.rnorm <- function(n,mean=0,sd=1,method) {
  if (missing(method)) {                           #Default method is method 1
    method <- 1
  }
  if (missing(mean))  {                            #default mean=0
    mean <- 0 
  }
  if (missing(sd)) {                               #default sd=1
    sd <- 1
  }
  
  if(method<1) stop("invalid arguments")           #method must be 1,2 or 3.
  if(method>3) stop("invalid arguments")
  
  if (method == 1)  {                               
    print("Marsaglia and Bray's method")
    return(my.rnorm(n,mean,sd))                     #Simply returns method/function in previous question
  }
  
  
  
  if (method == 2) {
    print("Box-Mueller algorithm")
    
    vec2 <- (rep(0,n))
    is.wholenumber <-
      function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (is.wholenumber(n/2) == TRUE) {                                   #similar to method 1, testing if n is odd or even, then acting accordingly
      
      for (i in 1:(n/2)) {                              #run for even values, produce n/2 pairs, giving n values
        
        deviate <- runif(n, min=0, max=1)
        U1 <- deviate[1]                                #produce 2 deviates
        U2 <- deviate[2]
        
        X1 <- sin(2*pi*U1)*(((-2)*log(U2))^(0.5))       #given equations for X1 and X2
        X2 <- cos(2*pi*U1)*(((-2)*log(U2))^(0.5))
        
        vec2[2*i-1] <- X1                               #plug in our values into the vector
        vec2[2*i] <- X2
      }
    }  else {                                           #if n is odd
      pairs.vec <- c(rep(0,(n+1)))                      #vector of length n+1
      for (i in 1:((n/2)+0.5)) {                        #run (n+1)/2 times, so we get n+1 values out
        
        deviate <- runif((n+1), min=0, max=1)
        U1 <- deviate[1]
        U2 <- deviate[2]
        
        X1 <- sin(2*pi*U1)*(((-2)*log(U2))^(0.5))
        X2 <- cos(2*pi*U1)*(((-2)*log(U2))^(0.5))
        
        pairs.vec[2*i-1] <- X1
        pairs.vec[2*i] <- X2
      }
      vec2 <- pairs.vec[1:n]                            #our vector of length n, eliminating the final value
    }
    return(vec2)
  }
  
  
  if (method == 3) {
    print("Central Limit Theorem")
    
    vec3 <- c(rep(0,n)) 
    
    for (j in 1:n) {                               #we want n values of X, so run n times
      
      vec16 <- c(rep(0,16))                        #create a vector of length 16 for 16 deviates to be summed later on
      for (i in 1:16) {                            #runs 16 times to select 16 deviates
        deviate <- runif(n, min=0, max=1)
        U <- deviate[1]
        vec16[i] <- U                             #deviates are filled into the vector
      }
      X <- (((sum(vec16))-8)*((12/16)^0.5))       #summation of 16 deviates put into an equation, giving X
      vec3[j] <- X
    }
    return(vec3)
  }
}


 








##Test Functions                              #we only need to test general.rnorm, as my.rnorm runs within general.rnorm

pass.test <- function(n,mean,sd,method) {          
  x <- general.rnorm(n,mean,sd,method)
  test1 <- length(x)==n                       #testing the vector is of length n, as we want
  test2 <- is.numeric(x)                      #testing my.rnorm gives numeric values
  T <- c(test1,test2)
  return(T)                                   #returns true or false statements for both tests
}


Shap.test <- function(n,mean,sd,method) {               #hypothesis test for normality for n values
  #is the function 'general.rnorm' normally distributed
  S.test <- shapiro.test(general.rnorm(n,mean,sd,method))              #returns p-value, for p<0.05, results are not normal
    if (S.test$p.value > 0.05) {
      print("TRUE")                                     #returns 'TRUE' if p-value is greater than 0.05
    } else {
      print("FALSE")                                    #returns 'FALSE' if p-value is less than 0.05
    }
  }                                                     #this does not prove normality!
                                                        #this will only show if the results are very bad and very clearly NOT normally distributed

test.hist <- function(n,mean,sd,method)  {
  
  x <- general.rnorm(n,mean,sd,method)
  H <- hist(x)                                #shows a histogram of our results from my.rnorm so we can see if it looks like a normal distribution
  return(NULL)                                #the larger the sample size, the more the histogram should look like a normal distribution, with the peak at the mean.
}

test.qq <- function(n,mean,sd,method) {       #Q-Q plot, gives us a good idea if results are normally distributed with a sufficiently large sample size.
  Q <- qqnorm(general.rnorm(n,mean,sd,method))
  return(Q)
}

mean.function <- function(n,mean,sd,method) {
  M <- mean(general.rnorm(n,mean,sd,method))
  return(M)
}                                             #this simply allows to see if the mean is sufficiently close the the mean we enter (default of 1`)




kurtosis.test <- function (n,mean,sd,method) {         #A method found online to test the skewness of the distribution
  x <- general.rnorm(n,mean,sd,method)
  m4 <- sum((x-mean(x))^4)/length(x)
  s4 <- var(x)^2
  kurt <- (m4/s4) - 3
  sek <- sqrt(24/length(x))
  totest <- kurt/sek
  pvalue <- pt(totest,(length(x)-1))
  pvalue                                               #the value given should be below 1.5 for the values to be acceptable for a normal distribution
}

skew.test <- function (n,mean,sd,method) {             #A method found online to test the skewness of the distribution
  x <- general.rnorm(n,mean,sd,method)
  m3 <- sum((x-mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  skew <- m3/s3
  ses <- sqrt(6/length(x))
  totest <- skew/ses
  pt(totest,(length(x)-1))
  pval <- pt(totest,(length(x)-1))
  pval
}                                                     #similar to the kurtosis test, value should be below 1.5 for normal distribution                                           












## Test functions for normality which give back p-values
#these test functions were given to me by R

#all of these return p-values, i have tested to see if the p-values are greater than 0.05
#if above 0.05, they return 'TRUE', otherwise return 'FALSE'
#TRUE does not prove normal, but FALSE will show that it is NOT normal!

norm.test <- function(n,mean,sd,method) {
  N <- nortest::ad.test(general.rnorm(n,mean,sd,method))
  if (N$p.value > 0.05) {
    print("TRUE")
  } else {
    print("FALSE")
  }
  return(N)
}

norm.test2 <- function(n,mean,sd,method) {
  N <- nortest::cvm.test(general.rnorm(n,mean,sd,method))
  if (N$p.value > 0.05) {
    print("TRUE")
  } else {
    print("FALSE")
  }
  return(N)
}

norm.test3 <- function(n,mean,sd,method) {
  N <- nortest::pearson.test(general.rnorm(n,mean,sd,method))
  if (N$p.value > 0.05) {
    print("TRUE")
  } else {
    print("FALSE")
  }
  return(N)
}


#there are more of these on the nortest package.





