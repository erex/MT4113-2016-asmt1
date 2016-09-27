#I confirm that the attached is my own work, except where clearly indicated in the text.
Marsaglia.Bray <- function(){
#Purpose:
#   Generates a pair of uniform deviates U1 and U2 using Marsaglia and Bray's method.
#Input:
#   There is no input in this function. 
#Output:
#   A pair of uniform deviates that are distributed over the range [-1,1].
  while(TRUE){
    u <- runif(2,0,1)
    u.sqr1 <- (2*u[1])-1
    u.sqr2 <- (2*u[2])-1
    w <- (u.sqr1^2)+(u.sqr2^2)
    if (w < 1){
      v <- sqrt((-2)*log(w)/w)
      break
    }
  }
  x1 <- u.sqr1*v
  x2 <- u.sqr2*v
  return(c(x1, x2))
}
######################################
Box.Mueller <- function(){
#Purpose:
#   Generates a pair of uniform deviates using Box-Mueller algorithm.
#Input:
#   There is no input in this function.
#Output:
#   A pair of deviates in polar coordinates.
  u <- runif(2,0,1)
  x1 <- sin(2*pi*u[1])*sqrt(-2*log(u[2]))
  x2 <- cos(2*pi*u[1])*sqrt(-2*log(u[2]))
  return(c(x1, x2))
}
#####################################
Central.thm <- function(){
#Purpose:
#   Generate a deviate using Central limit theorem.
#Input:
#   There is no input in this function.
#Out:
#   A deviate.
  u <- runif(16,0,1)
  x <- (sum(u)-8) * sqrt(12/16)
  return(x)
}
########################################
my.rnorm <- function(n, mean=0, sd=1){
#Purpose:
#   Generates a vector of pseudo-random values from a normal distribution using
#   Marsaglia and Bray algorithm.
#Input:
#   n - number of values 
#   mean - mean of values
#   sd - standard deviation of values
#Output:
#   Pair of normal distributed deviates.
  if ((n <= 0) | (sd <= -1)){
    stop("invalid arguments")
  }
  x = NULL
  if (n%%2 == 0){
    len <- n/2
    for (i in (1:len)){
      x1 <- Marsaglia.Bray()
      x <- append(x,x1)
    }
  } else {
    len <- (n+1)/2
    for (i in (1:len)){
      x1 <- Marsaglia.Bray()
      x <- append(x,x1)
    }
    x <- x[-(n+1)]
  }
  if (mean != 0 & sd != 1){
    a <- mean
    b <- sd
    x <- (x*b)+a
  }
  return(x)
}    
###############################
general.rnorm <- function(n,mean=0,sd=1,method){
#Purpose:
#   Generates a vector of pseudo-random normally-distributed deviates.
#Input:
#   n - number of values 
#   mean - mean of values
#   sd - standard deviation of values
#   method - choosing which method will be used in the function:
#                 -1 corresponding to Marsaglia and Bray algorithm
#                 -2 corresponding to Box and Mueller algorithm
#                 -3 corresponding to central-limit theorem alogorithm
#Output:
#   Pair of normal distributed deviates.
  if ((n <= 0) | (sd <= -1)){
    stop("invalid arguments")
  }
  if ((method < 1) | (method >3)){
    stop("invalid arguments")
  }
  if (method == 1) {
    x <- my.rnorm(n,mean,sd)
  }
  if (method == 2) {
    x <- NULL
    if (n%%2 == 0){
      len <- n/2
      for (i in (1:len)){
        x1 <- Box.Mueller()
        x <- append(x,x1)
      }
    } else {
      len <- (n+1)/2
      for (i in (1:len)){
        x1 <- Box.Mueller()
        x <- append(x,x1)
      }
      x <- x[-(n+1)]
    }
  }
  if (method == 3) {
    x <- NULL
    for (i in (1:n)){
        x1 <- Central.thm()
        x <- append(x,x1)
    }
  }
  if (mean != 0 & sd != 1){
    a <- mean
    b <- sd
    x <- (x*b)+a
  }
  return(x)
}
##################################
test.myfunction <- function(n, mean = 0, sd = 1, method){
#Purpose:
#   To test whether the results form the my.rnorm and general.rnorm functions are normally 
#   distributed or not, using qqplot.
#Input:
#   n - number of values
#   mean - mean of values
#   sd - standard deviation of values
#   method - choosing which method will be used in the function:
#                 -1 corresponding to Marsaglia and Bray algorithm
#                 -2 corresponding to Box and Mueller algorithm
#                 -3 corresponding to central-limit theorem alogorithm
#Output:
#   The normal Q-Q plot from my.rnorm and general.rnorm functions.
  if ((n <= 0) | (sd <= -1)){
    stop("invalid arguments")
  }
  if ((method < 1) | (method >3)){
    stop("invalid arguments")
  }
  par(mfrow = c(1,2) )
  a <- my.rnorm(n,mean,sd)
  b <- general.rnorm(n,mean,sd,method)
  qqnorm(a, main = "Marsaglia and Bray algorithm")
  qqline(a, col = 2)
  if (method == 1){
    qqnorm(b,main = "Marsaglia and Bray algorithm" )
    qqline(b, col = 2)
  }else if (method == 2) {
    qqnorm(b,main = "Box and Mueller algorithm" )
    qqline(b, col = 2)
  }else {
    qqnorm(b,main = "Central-Limit Theorem algorithm" )
    qqline(b, col = 2)
  }
}
  


