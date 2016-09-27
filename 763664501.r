#I confirm that the attached is my own work, except where clearly indicated in the text

mars.bray <- function(n, mean = 0, sd = 1){
#function to carry out Marsaglia & Bray's method
  output = c()  #define output as an empty vector
#initiate loop to run from 1 to n in steps of 2 so we can work out if n is odd and work accordingly
  for(i in seq(1,n,2)) {
    y = 0
    while (y == 0) {  
      u1 = runif(1); u2 = runif(1)   #generate uniform deviates
      u1 = 2*u1 - 1; u2 = 2*u2 - 1   #transform to unit square    
      w = (u1*u1 + u2*u2)   #define w
      if (w <= 1) y = 1     #check condition, if met then generate new u1, u2
    }
    v = sqrt(-2*log(w)/w)  #define v
    u1 = u1*v; u2 = u2*v   #define pair of normally distr variates
#add transformed variates to output vector (if n is odd, only one of the pair will be added) 
    if (i == n) {
      output = c(output,(u1*sd + mean))     
} else {
      output = c(output,(u1*sd + mean), (u2*sd + mean))  
      }
  }
  return(output)
}

box.mue <- function(n, mean = 0, sd = 1) {
#function to carry out Box-Mueller algorithm
  output = c()  #define output as an empty vector
#initiate loop to run from 1 to n in steps of 2 so we can work out if n is odd and work accordingly
   for(i in seq(1,n,2)) {
    u1 = runif(1); u2 = runif(1)   #generate uniform deviates
    x1 = sin(2*pi*u1)*sqrt(-2*log(u2)) #transform to polar coords
    x2 = cos(2*pi*u1)*sqrt(-2*log(u2)) #transform to polar coords
#add transformed variates to output vector (if n is odd, only one of the pair will be added)   
    if (i == n) {
      output = c(output,(x1*sd + mean))  #add transformed variates to output vector
    } else {
      output = c(output,(x1*sd + mean), (x2*sd + mean))  #add transformed variates to output vector
    }
  }
  return (output)
}

clt <- function(n, mean = 0, sd = 1){
#function to carry out central limit theorem method
  output = c()  #define output as an empty vector
#start loop to carry out the algorithm n times
  for (i in 1:n) {
    x = 0 #reinitiate x to 0 for each run of the algorithm
    x = sum(runif(16)) #find sum of 16 variates
    x = (x - 8) * sqrt(12/16)
    output = c(output, x*sd + mean) #add algorithm output to vector
  }
  return(output)
}

argument.check <- function(n, mean = 0, sd = 1, method = 1){
#function to check for input errors in any function
  #check that n, mean and sd are numeric
  if (!(is.numeric(c(n,mean,sd)))) {  
    stop("invalid arguments")  #if not give error message
  #check that n is a positive integer and sd is non-negative
  } else if (!(n %% 1 == 0) | !(n > 0) | (sd < 0)) {
      stop("invalid arguments")  #if not give error message
    #ensure none of the input is TRUE or FALSE as these would pass the first test
  } else if (isTRUE(n) | isTRUE(mean) | isTRUE(sd) | identical(mean, FALSE) | identical(sd, FALSE)) {  
    stop("invalid arguments")  #if they are, give error message
  }
  if (method != 1 && method != 2 && method != 3) {
    stop("invalid arguments")  #give error if method != 1,2,3
  }
}

my.rnorm <- function(n, mean = 0, sd = 1) {
  argument.check(n = n, mean = mean, sd = sd) #check for errors in argument list
  return(mars.bray(n,mean,sd))
}

general.rnorm <- function(n, mean = 0, sd = 1, method = 1){
  argument.check(n = n, mean = mean, sd = sd, method = method) #check for errors in argument list
  if (method == 1) {
    return (my.rnorm(n,mean,sd))
  }
  if (method == 2) {
    return (mars.bray(n,mean,sd))
  }
  if (method == 3) {
    return (box.mue(n,mean,sd))
  }
}


#Test function section

size.check <- function (n){
# #function to check that my.rnorm and general.rnorm produce right size of vector for sizes 1 to n
  argument.check(n = n) #check n is valid
#loop for every value from i to n and ensure these produce the right size of vector for each method  
  for (i in 1:n) {
    x = (my.rnorm(i))   #use my.rnorm function to create random vector 
    y1 = (general.rnorm(i))   #use general.rnorm, method 1, function to create random vector
    y2 = (general.rnorm(i, method = 2))   #use general.rnorm, method 2, function to create random vector
    y3 = (general.rnorm(i, method = 3))   #use general.rnorm, method 3, function to create random vector
     size.check = (length(x)==i & length(y1)==i & length(y2)==i & length(y3)==i)
    if (size.check==FALSE) { 
      return(size.check) #if any method produces wrong size, test = FALSE
    }
  }
  return(size.check) #if all produce right size, test = TRUE
}

norm.test1 <- function (x) {
#function to test for normality using Kolmogorov-Smirnov test
  n=length(x)
  output = ks.test(x,rnorm(n)) #perform test
  pval = output[2]
  if (pval < 0.05) norm.test1 = FALSE #if p value < 0.05, function = FALSE and test is failed
  else norm.test1 = TRUE #otherwise function = TRUE and test is passed
  return (norm.test1)
} 

norm.test2 <- function (x) {
#function to test for normality using Shapiro Wilk test
  output = shapiro.test(x) #perform test
  pval = output[2]
  if (pval < 0.05) norm.test2 = FALSE #if p value < 0.05, function = FALSE and test is failed
  else norm.test2 = TRUE #otherwise function = TRUE and test is passed
  return (norm.test2)
}

plot.test <- function (x) {
#function to visually test for normality using plots
  par(mfrow=c(1,2)) #format so two plots will appear side by side
  hist(x) #plot a histogram of the data
  qqnorm(x); qqline(x) #plot all points on the data and add a line of best fit
  #visually we can check that the data follow a normal distribution
}
