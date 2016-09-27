#I confirm that the attached is my own work, except where clearly indicated in the text.

check.input<-function(n, mean, sd){
  #check that inputs are numeric
  if(!is.double(n) | !is.double(mean) | !is.double(sd)){stop("invalid arguments")}
  #since each vector contains at least one argument checking the length of the vector containing all three arguments checks
  #that all of the arguments are scalars
  if(length(c(n, mean, sd))!=3){stop("invalid arguments")}
  #check that inputs are in the right range
  if(n!=floor(n) | n<0 | sd<0){stop("invalid arguments")}
  return(NULL)
}

my.rnorm <- function(n, mean = 0, sd = 1){
  #  
  #  Purpose:
  #     Generate vector of normally distributed deviates
  #  
  #  Input:
  #     n - number of deviates desired
  #     mean - the desired mean of the deviates
  #     sd - the desired standard deviation of the deviates
  #
  #  Output:
  #     vector of length n filled with normally distributed deviates with mean = mean and standard deviation = sd
  #
  check.input(n, mean, sd)
  values <- vector(mode = "numeric", length = n) #creates a vector of length n with 0 in each entry
  for(i in 0:n){#Marsaglia & Bray algorithm generates pairs so we only need to run n/2 times. 
    #However if n is odd we need to run (n+1)/2 times and discard a value
    repeat{#Marsaglia and Bray's method(Maindonald 1984)
      U <- runif(2,0,1)
      U<-2*U-1
      w=U[1]**2 + U[2]**2
      if(w<=1){break}
    }
    v=(-2*log(w)/w)**0.5
    X = U * v
    values[i]<-X[1]
    if(i+ceiling(n/2)<=n){
      values[i+ceiling(n/2)]<-X[2]
    }
  }
  return((values*sd)+mean)#transform to desired mean and standard deviation
}

box.rnorm <- function(n, mean = 0, sd = 1){
  #  
  #  Purpose:
  #     Generate vector of normally distributed deviates
  #  
  #  Input:
  #     n - number of deviates desired
  #     mean - the desired mean of the deviates
  #     sd - the desired standard deviation of the deviates
  #
  #  Output:
  #     vector of length n filled with normally distributed deviates with mean = mean and standard deviation = sd
  #
  check.input(n, mean, sd)
  values <- vector(mode = "numeric", length = n) #creates a vector of length n with 0 in each entry
  for(i in 0:n){#BBox-Mueller algorithm generates pairs so we only need to run n/2 times. 
    #However if n is odd we need to run (n+1)/2 times and discard a value
    #Box-Mueller Algorithm (Jones et al. (2009))
    U <- runif(2,0,1)
    X<-c(sin(2*pi*U[1])*(-2*log(U[2]))**0.5, cos(2*pi*U[1])*(-2*log(U[2]))**0.5)
    values[i]<-X[1]
    if(i+ceiling(n/2)<=n){
      values[i+ceiling(n/2)]<-X[2]
    }
  }
  return((values*sd)+mean)#transform to desired mean and standard deviation
}

CLT.rnorm <- function(n, mean = 0, sd = 1){
  #  
  #  Purpose:
  #     Generate vector of normally distributed deviates
  #  
  #  Input:
  #     n - number of deviates desired
  #     mean - the desired mean of the deviates
  #     sd - the desired standard deviation of the deviates
  #
  #  Output:
  #     vector of length n filled with normally distributed deviates with mean = mean and standard deviation = sd
  #
  check.input(n, mean, sd)
  values <- vector(mode = "numeric", length = n) #creates a vector of length n with 0 in each entry
  for(i in 0:n){#CLT algorithm generates one variate at a time so we need to run n times. 
    #Central limit theorem (Mood et al. (1974))
    U <- runif(16,0,1)
    X <- (sum(U)-8)*(12/16)**0.5
    values[i]<-X
  }
  return((values*sd)+mean)#transform to desired mean and standard deviation
}

general.rnorm <- function(n, mean=0, sd=1, method=1){
  #
  #  Purpose: 
  #     Generate a vector of normally distributed deviates using the algorithm specified by the input
  #
  #  Input:
  #     n - number of deviates desired
  #     mean - the desired mean of the deviates
  #     sd - the desired standard deviation of the deviates
  #     method - an integer corresponding to the desired algorithm
  #
  #  Output:
  #     vector of length n filled with normally distributed deviates with mean = mean and standard deviation = sd
  #
  if(!(method %in% c(1,2,3))){stop("invalid arguments")}#check method is a valid input
  #call appropriate function
  return(switch(method,
                my.rnorm(n=n, mean=mean, sd=sd),
                box.rnorm(n=n, mean=mean, sd=sd),
                CLT.rnorm(n=n, mean=mean, sd=sd)))
}

###Tests
###Each test accepts 1,2,3 or 4 for the argument test which decides which function is tested:
### 1 - my.rnorm
### 2 - general.rnorm with method = 1
### 3 - general.rnorm with method = 2
### 4 - general.rnorm with method = 3

test.length <-function(test = 1, len){
  #
  #  Purpose:
  #     To test that the output of each function is of the right length
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #     len - the length of vector the function should produce so that the user can test even and odd lengths
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  if(!is.double(len)){stop("invalid arguments")}#check len is numeric
  if(length(len)!=1){stop("invalid arguments")}#check len is a scalar
  if(floor(len)!=len | len<0){stop("invalid arguments")}#check len is a non-negative integer
  switch(test,
         pass.test<-(length(my.rnorm(n = len , mean = 0, sd = 1))==len),
         pass.test<-(length(general.rnorm(n = len, mean = 0, sd = 1, method = 1))==len),
         pass.test<-(length(general.rnorm(n = len, mean = 0, sd = 1, method = 2))==len),
         pass.test<-(length(general.rnorm(n = len, mean = 0, sd = 1, method = 3))==len))#Check the length of the output
  return(pass.test)
}

test.numeric <-function(test = 1){
  #
  #  Purpose:
  #     To test that the output of each function is numeric
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  switch(test,
         pass.test<-(is.numeric(my.rnorm(n = 10 , mean = 0, sd = 1))),
         pass.test<-(is.numeric(general.rnorm(n = 10, mean = 0, sd = 1, method = 1))),
         pass.test<-(is.numeric(general.rnorm(n = 10, mean = 0, sd = 1, method = 2))),
         pass.test<-(is.numeric(general.rnorm(n = 10, mean = 0, sd = 1, method = 3))))#Check the output is numeric
  return(pass.test)
}

test.transformation<-function(mean=5, sd=10, test=1, seed=123456789){
  #
  #  Purpose:
  #     Test that the transformation from N(0, 1) to N(mu, sigma) has been completed correctly by 
  #     generating both from the same seed and checking the transformation
  #
  #  Input:
  #     mean - the desired mean of the deviates
  #     sd - the desired standard deviation of the deviates
  #     test - integer indicating which function to test
  #     seed - an integer giving the seed which the prng should start from
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  if(!is.double(seed)){stop("invalid arguments")}#check seed is numeric
  if(length(seed)!=1){stop("invalid arguments")}#check seed is a scalar
  if(floor(seed)!=seed | seed<0){stop("invalid arguments")}#check seed is a non-negative integer
  check.input(20, mean, sd)
  set.seed(seed)#set the seed
  switch(test,
         rand.values.untransformed<-my.rnorm(n = 20, mean = 0, sd = 1),
         rand.values.untransformed<-general.rnorm(n = 20, mean = 0, sd = 1, method = 1),
         rand.values.untransformed<-general.rnorm(n = 20, mean = 0, sd = 1, method = 2),
         rand.values.untransformed<-general.rnorm(n = 20, mean = 0, sd = 1, method = 3))#generate the untransformed values
  set.seed(seed)#reset the seed
  switch(test,
         rand.values<-my.rnorm(n = 20, mean = mean, sd = sd),
         rand.values<-general.rnorm(n = 20, mean = mean, sd = sd, method = 1),
         rand.values<-general.rnorm(n = 20, mean = mean, sd = sd, method = 2),
         rand.values<-general.rnorm(n = 20, mean = mean, sd = sd, method = 3))#generate the transformed values
  result<-isTRUE(all.equal(rand.values.untransformed,(rand.values-mean)/sd))#check equality
  return(result)
}

test.negative.n <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a negative value is input for n
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = -1 , mean = 0, sd = 1),
         general.rnorm(n = -1, mean = 0, sd = 1, method = 1),
         general.rnorm(n = -1, mean = 0, sd = 1, method = 2),
         general.rnorm(n = -1, mean = 0, sd = 1, method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.non.integer.n <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a non integer value is input for n
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = 0.5, mean = 0, sd = 1),
         general.rnorm(n = 0.5, mean = 0, sd = 1, method = 1),
         general.rnorm(n = 0.5, mean = 0, sd = 1, method = 2),
         general.rnorm(n = 0.5, mean = 0, sd = 1, method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.negative.sd <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a negative value is input for sd
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = 10 , mean = 0, sd = -1),
         general.rnorm(n = 10, mean = 0, sd = -1, method = 1),
         general.rnorm(n = 10, mean = 0, sd = -1, method = 2),
         general.rnorm(n = 10, mean = 0, sd = -1, method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.non.numeric.n <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a non numeric value is input for n
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = "10" , mean = 0, sd = 1),
         general.rnorm(n = "10", mean = 0, sd = 1, method = 1),
         general.rnorm(n = "10", mean = 0, sd = 1, method = 2),
         general.rnorm(n = "10", mean = 0, sd = 1, method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.non.numeric.mean <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a non numeric value is input for mean
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = 10 , mean = "0", sd = 1),
         general.rnorm(n = 10, mean = "0", sd = 1, method = 1),
         general.rnorm(n = 10, mean = "0", sd = 1, method = 2),
         general.rnorm(n = 10, mean = "0", sd = 1, method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.non.numeric.sd <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a non numeric value is input for sd
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = 10 , mean = 0, sd = "1"),
         general.rnorm(n = 10, mean = 0, sd = "1", method = 1),
         general.rnorm(n = 10, mean = 0, sd = "1", method = 2),
         general.rnorm(n = 10, mean = 0, sd = "1", method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.non.scalar.n <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a non scalar value is input for n
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = c(10, 10) , mean = 0, sd = 1),
         general.rnorm(n = c(10, 10), mean = 0, sd = 1, method = 1),
         general.rnorm(n = c(10, 10), mean = 0, sd = 1, method = 2),
         general.rnorm(n = c(10, 10), mean = 0, sd = 1, method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.non.scalar.mean <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a non scalar value is input for mean
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = 10 , mean = c(0,0), sd = 1),
         general.rnorm(n = 10, mean = c(0,0), sd = 1, method = 1),
         general.rnorm(n = 10, mean = c(0,0), sd = 1, method = 2),
         general.rnorm(n = 10, mean = c(0,0), sd = 1, method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.non.scalar.sd <-function(test = 1){
  #
  #  Purpose:
  #     To test that the right error is thrown when a non scalar value is input for sd
  #
  #  Input:
  #     test - integer indicating the function which should be tested
  #
  if(!(test %in% c(1,2,3,4))){stop("invalid arguments")}#check test is a valid input
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(switch(test,
         my.rnorm(n = 10 , mean = 0, sd = c(1,1)),
         general.rnorm(n = 10, mean = 0, sd = c(1,1), method = 1),
         general.rnorm(n = 10, mean = 0, sd = c(1,1), method = 2),
         general.rnorm(n = 10, mean = 0, sd = c(1,1), method = 3))) #Try appropriate function
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}

test.invalid.method <-function(){
  #
  #  Purpose:
  #     To test that the right error is thrown when an invalid input is entered for the method argument
  #
  pass.test <- FALSE
  options(show.error.messages = FALSE) #Suppress error messages
  try(general.rnorm(n = 10, mean = 0, sd = 1, method = 5))#Try function with invalid method
  err <- geterrmessage()
  if(substring(err, nchar(err)-17, nchar(err)-1)=="invalid arguments") pass.test<-TRUE #If the error message ends "invalid arguments" ten the test is passed
  options(show.error.messages = TRUE) #Restore error message settings
  return(pass.test=pass.test)
}