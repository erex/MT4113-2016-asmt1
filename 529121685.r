#I confirm that the attached is my own work, except where clearly indicated in the text

#This function my.rnorm will take into account Marsaglia and Bray algorithm
# Input:
# n - number of values to return
# mean - specified mean of the normal distribution, 0 by default
# sd - standard deviation, 1 by default 
# threads - number of treads to run to prevent infinte loops
#Output: returns back n number of randomly generated normally distributed numbers
#with specified mean and standart deviation

my.rnorm <- function ( n = NULL, mean = 0, sd = 1, threads = 1000){
  
  #Handling all the standart errors in the input
  handlingDefault( n, mean, sd)
  
  #To handdle an odd number of numbers, generating an even number of numbers 
  #and outputting m-1
  iterations <- ceiling( n/2 )
  answer<-runWithRejection( iterations, threads)
  
  #Checking if there any extra values t
  if ( length( answer ) != n ){
    answer <- answer[-1]
  }
  
  #Returns the answer taking into account non standart sd and mean 
  return( standartise( answer, sd, mean))
  
}

#Handler for common input errors
handlingDefault <- function( n, mean, sd){
  
  errorMsg <- "invalid arguments"
  
  #Handling the lack of n error
  if ( is.null(n)) stop(errorMsg)
  
  #Handling negative n values
  if ( n <= 0) stop(errorMsg)
  
  #Throws a message if the user did not specify the mean value
  if ( mean == 0) message(errorMsg)
  
  #Throws a message if the user did not specify the sd value
  if( sd == 1) message(errorMsg)
  
  #Throws an error if given sd is negative
  if ( sd < 0) stop(errorMsg)
  
}

#Running the rejection stage: reject all the values outside of the unit square
#Limiting the loop to the number of threads and iterations
runWithRejection <- function( iterations, threads){
  
  #Creating an empty vector to store our results
  answer <- c()
  errorMsg <- "invalid arguments"
  for (m in 1:iterations){
    
    #To catch an infinte loop creating a "running out of threads" error
    i <- 0
    
    #Starting values
    unif <- runif( 2, -1, 1)
    w <- sum( unif**2)
    while ( w > 1){
      unif <- toUnitSquare( generateUnif())
      w <- sum( unif**2)
      i <- i+1
      if (i > threads) stop(errorMsg)
    }
    
    v <- sqrt(-2*log(w))/sqrt(w)
    x <- unif*v
    answer <- append( answer, x)
  }
  
  return( answer)
}

#Generating two unif values as a vector
generateUnif <- function() {
  u <- runif( 2, 0, 1) 
  return(u)
}

#Transform the uniform values to the unit square
toUnitSquare <- function(u){
  u <- 2*u -1
  return(u)
}


#_________________Testing_______________________________________

#The code below is just a collection of unit tests with a brief explanation in the comments

test.my.rnorm <- function() {
  #Creating a data frame for storing the tests results
  results <- data.frame()
  #Testing normality using Shapiro-Wilk test. 
  #User must keep in mind since generated numbers produces from pseudo-random numbers, the observed p-value may be below 5% 
  # but the code would still work correctly
  a <- my.rnorm( n = 1000, mean = 10, sd = 5)
  plot(a)
  if (shapiro.test(a)$p > 0.1 ) {
    results <- append( results, "test passed")
  } else {
    results <- append( results, "test failed")
  }
  
  #Unit tests for all the functions and errors
  # Handling negative sd
  res1 <- try(handlingDefault(n = 3, mean = 0, sd = -1),silent = TRUE)
  if (class(res1)  == "try-error"){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  
  #Chechikng functionality
  if (toUnitSquare(2) == 3){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  #Gives the specified n numbers
  if (length(runWithRejection( 3, 100)) == 6){
    results <- append(results, "test passed")
  }else {
    results <-  append(results, "failed")
  }
  #Throws an error if no n given
  res2 <- try(my.rnorm() ,silent = TRUE)
  if (class(res2)  == "try-error"){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  #Gives the specified n numbers
  if (length(my.rnorm(3))  == 3){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  #Throws an error if sd is negative
  res3 <- try(  my.rnorm( n = 5, sd = -4) ,silent = TRUE)
  if (class(res3)  == "try-error"){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  names(results) <- c("Shapiro-Wilk test", "Negative sd for handler", "UnitSquare", "Specified n in runWithRejection()", "No n in my.rnorm", "Specified n in my.rnorm", "Negative sd in my.rnorm")
  return(results)
  
}

#_________________________________________________________________________

#This function general.rnorm will take into account Marsaglia and Bray algorithm, 
#Box-Mueller algorithm and the Central Limit theorem 
# Input:
# n - number of values to return
# mean - specified mean of the normal distribution, 0 by default
# sd - standard deviation, 1 by default 
#number of treads to run
# threads - number of treads to run to prevent infinte loops
# method - 1 indicated Marsaglia and Bray, 2 for Box-Mueller algorithm 
#and 3 for the Central Limit theorem 
#Output: returns back n number of randomly generated normally distributed numbers
#with specified mean, standart deviation and using the specified method

general.rnorm <- function( n, mean = 0, sd = 1, threads = 1000, method = 1){
  
  #Handling the default methods
  handlingDefault( n, mean, sd)
   errorMsg <- "invalis arguments"
  #Throws the warning if the user did not specify the method
  if ( method == 1){# add elif
    message("Using Marsaglia and Bray algorithm")
    return(my.rnorm( n, mean, sd, threads))
  }else if( method == 2){
    message("Using Box-Mueller algorithm")
    answer <- box.mueller(n)
    return( standartise( answer, sd, mean))
  }else if( method == 3){
    message("Using Central Limit theorem")
    answer <- central(n) 
    return(standartise( answer, sd, mean))
  } else {
    stop(errorMsg)
  }
}

#Modifies the answer according to the specified sd and mean
standartise <- function( a, sd, mean){
  
  a <- a*sd + mean
  return(a)
}

#_______Central limit theorem method__________________________
central <- function(n){
  #Same approach as before, creates an empty vector to store the answer values
  answer <- c()
  for ( i in 1:n){
    U <- runif( 16, 0, 1)
    x <- (sum(U) - 8)*sqrt(3)/sqrt(4)
    answer <- append( answer, x)
  }
  return(answer)
}

#______Box-Mueller algorithm________________________________
box.mueller <- function(n){

  #In order to hangle odd n, as before, create the number of iterations
  iterations <- ceiling(n/2)
  answer <- c()
  for (i in 1:iterations){
    u <- generateUnif()
    x1 <- sin(2*pi*u[1])*sqrt(-2*log(u[2]))
    x2 <- cos(2*pi*u[1])*sqrt(-2*log(u[2]))
    answer <- append( answer, c( x1, x2))
  }
  #Clean the frame from extra values
  if (length(answer) != n){
    answer <- answer[-1]
  }
  
  return(answer)
}


#__________Testing_____________________________

#The code below is just a collection of unit tests with a brief explanation in the comments

test.general.rnorm <- function() {
  #Creating a data frame for storing the tests results
  results <- data.frame()
  #Testing normality using Shapiro-Wilk test. 
  #User must keep in mind since generated numbers produces from pseudo-random numbers, the observed p-value may be below 5% 
  # but the code would still work correctly
  a <- general.rnorm( n = 4000, sd = 4, mean = 50, method = 2)
  b <- general.rnorm( n = 4000, sd = 4, mean = 50, method = 3)
  plot(a)
  if (shapiro.test(a)$p > 0.1 ) {
    results <- append( results, "test passed")
  } else {
    results <- append( results, "test failed")
  }
  plot(b)
  if (shapiro.test(b)$p > 0.1 ) {
    results <- append( results, "test passed")
  } else {
    results <- append( results, "test failed")
  }
  
  #Unit tests for all the functions and errors
  # Handling no n in box.mueller()
  res1 <- try(box.mueller(),silent = TRUE)
  if (class(res1)  == "try-error"){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  
  #Chechikng if returns the correct n
  if (length(box.mueller(5)) == 5){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  # Handling no n in central()
  res1 <- try(central(),silent = TRUE)
  if (class(res1)  == "try-error"){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  
  #Chechikng if returns the correct n
  if (length(central(5)) == 5){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  
  #Throws an error if no n given
  res2 <- try(general.rnorm() ,silent = TRUE)
  if (class(res2)  == "try-error"){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  #Throws an error if sd is negative
  res3 <- try( general.rnorm( n = 3, sd = -1) ,silent = TRUE)
  if (class(res3)  == "try-error"){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  
  #Throws an error if a wrong method was parsed
  res3 <- try( general.rnorm( n = 3, sd = -1, method = 4) ,silent = TRUE)
  if (class(res3)  == "try-error"){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  
  if (length(general.rnorm( n = 3, sd = 4)) == 3){
    results <- append(results, "test passed")
  } else {
    results <-  append(results, "failed")
  }
  names(results) <- c("Shapiro-Wilk test for Box-Mueller algorithm", "Shapiro-Wilk test for Central Limit Theorem", "No n in box.mueller()","Specified n in box.mueller()",
                      "No n in central()", "Specified n in central()", "No n in general.rnorm()", 
                      "Negative sd for general.rnorm()", "Method input test", "Specified n in general.rnorm()")
  return(results)
  
}

