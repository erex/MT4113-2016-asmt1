# I confirm that the attached is my own work, except where clearly indicated in the text.

# Helper functions____________________________________________________________________________

# Arguments: Deviate, desired mean, desired sd
# Output: Transforms deviate from N(0,1) to N(Mean, SD)
getTransformed <- function(dev, mean, sd){
  dev <- dev*sd+mean
}

# Arguments: n, mean, sd
# Output: Warnings if values are inadequate and interruption of the execution
# NB: Checks are kept within separate if clauses - enhances readability and 
# maintains option to add individual error messages
checkArguments <- function(n = 1, mean = 0, sd = 1, method = 1){
  
  # Store args in list for easier checking
  arguments <- c(n, mean, sd, method)
  
  # Expected number of args
  numOfArgs <- 4
  
  # Define the error message that should be shown
  errorMsg <- "invalid arguments"
  
  # Catch if any argument is non-numeric and interrupt execution
  if(any(!is.numeric(arguments))){
    stop(errorMsg)
  }
  
  # Catch if there are more than the specified number of arguments (would 
  # indicate non-scalar input) and interrupt execution
  if(length(arguments) != numOfArgs){
    stop(errorMsg)
  }
  
  # Catch n smaller than 1 and interrupt execution
  if(n < 1){
    stop(errorMsg)
  }
  
  # Catch non-integer n and interrupt execution
  if(!n%%1 == 0){
    stop(errorMsg)
  }
  
  # Catch if sd lower than 0 and interrupt execution
  if(sd <= 0){
    stop(errorMsg)
  }
  
  # Catch undefined method arg and interrupt execution
  if((!is.element(method, c(1,2,3)))){
    stop(errorMsg)
  }
  
}

# Marsaglia and Bray’s method (Maindonald 1984).________________________________________________________________
my.rnorm <- function(n, mean = 0, sd = 1){
  
  # Check the adequacy of the arguments
  checkArguments(n, mean, sd)
  
  # Arguments: u
  # Output: u transformed to unit square
  getUnitSquare <- function(u){
    u <- 2 * u - 1
    return(u)
  }
  
  # Arguments: List of two floats
  # Output: Scalar, sum of the squares of the individual values in the list
  getW <- function(U){
    w <- sum(U**2)
    return(w)
  }
  
  # Arguments: Value w
  # Output: Transformed scalar value
  getV <- function(w){
    v = sqrt(-2*log(w)/w)
    return(v)
  }
  
  # Empty vector for deviates
  deviates <- NULL
  
  # Get the number of iterations needed
  its <- ceiling(n/2)
  
  # Iterate over the number of deviates that are needed
  for (i in seq(1, its)){
    
    # Set U to values that are going to be rejected to get the while loop started
    U <- c(2, 2)
    
    # While the w is larger than 1, get new seed
    while (getW(U) > 1){
      
      # Begin with a pair of uniform deviates U1 and U2
      U <- runif(2, -1, 1)
      
      # Transforming to unit square
      U <- sapply(X=U, FUN=getUnitSquare)
    }
    
    devPair <- U * getV(getW(U))
    deviates <- append(deviates, devPair)
  }
  
  # Cut off the unneeded deviate (if present)
  deviates <- deviates[1:n]
  
  # Transform the deviates from N(0,1) to N(mean, sd)
  deviates <- sapply(X = deviates, getTransformed, mean = mean, sd = sd)
  
  # return the vector with the n deviates
  return(deviates)
}

# Box-Mueller algorithm. Jones et al. (2009)_____________________________________________________
box.mueller.rnorm <- function(n, mean = 0, sd = 1){
  
  # Get the number of iterations needed
  its <- ceiling(n/2)
  deviates <- NULL
  
  # Iterate over the number of deviates that are needed
  for (i in seq(1, its)){
    U <- runif(2)
    x1 <- sin(2*pi*U[1])*sqrt(-2*log(U[2]))
    x2 <- cos(2*pi*U[1])*sqrt(-2*log(U[2]))
    devPair <- c(x1, x2)
    deviates <- append(deviates, devPair)
  }
  
  # Cut off the unneeded deviate (if present)
  deviates <- deviates[1:n]
  
  # Transform the deviates from N(0,1) to N(m, sd)
  deviates <- sapply(X=deviates, getTransformed, mean = mean, sd = sd)
  return(deviates)
}

# Central Limit Theorem: Mood et al. (1974)_______________________________________________________
clt.rnorm <- function(n, mean = 0, sd = 1){
  
  # Empty vector for deviates
  deviates <- NULL
  
  # Get the number of iterations needed
  its <- n
  
  # Iterate over the number of deviates that are needed
  for (i in seq(1, its)){
    U <- runif(16)
    X <- (sum(U) - 8)* sqrt(12/16)
    deviates <- append(deviates, X)
  }
  
  # Cut off the unneeded deviate (if present)
  deviates <- deviates[1:n]
  
  # Transform the deviates from N(0,1) to N(m, sd)
  deviates <- sapply(X=deviates, getTransformed, mean = mean, sd = sd)
  return(deviates)
}

# Arguments: N, Mean, SD, Method
# Output: Vector of deviates with characteristics from above
general.rnorm <- function(n, mean = 0, sd = 1, method = 1){
  
  # Check the provided arguments for adequacy 
  checkArguments(n, mean, sd, method)
  
  if(method == 1){
    return(my.rnorm(n, mean, sd))
  }
  else if(method == 2){
    return(box.mueller.rnorm(n, mean, sd))
  }
  else if(method == 3){
    return(clt.rnorm(n, mean, sd))
  }  
}

# Testing _____________________________________________________________________________________

# Objective: In case there are any mistakes in the code above, it will fail to 
# produce deviates with the required distribution characteristics. The KS Test 
# enables the user to test whether the produced deviates are from the required 
# distribution, namely N(mean, sd). In addition, as the R implementation of KS 
# is not suitable for normality testing, Shapiro-Wilk is carried out, too. 
# Caveat: As the deviates are produces from pseudo-random numbers, it is 
# possible, that the tests indicate an p-value below e.g. 5% while actually the 
# code does everything correctly.

test.deviates <- function(n, mean = 0, sd = 1){
  
  # Check the adequacy of the arguments
  checkArguments(n, mean, sd)
  
  # Empty vector for storing individual p-values
  KS.p.values <- NULL
  SW.p.values <- NULL
  
  # Boolean indicating if the test was passed
  testPassed <- TRUE
  
  # Iterate over each method
  for(i in 1:3){
    x <- general.rnorm(n, mean, sd, method=i)
    
    # Check if n deviates were generated
    if(length(x) != n){
      warning("The number of generated deviates deviates from the required number n.")
    }
    
    # Check if all returned deviates are numeric
    if(!is.numeric(x)){
      warning("The deviates contain non-numeric characters.")
    }
    
    # Get the individual p-values for both tests
    oneKS <- ks.test(x, "pnorm", mean, sd)$p.value
    oneSW <- shapiro.test(x)$p.value
    
    # Add the p-values to the total
    KS.p.values <- append(KS.p.values, oneKS)
    SW.p.values <- append(SW.p.values, oneSW)
    
    # Issue a warning in case one or more of the p-values are below 5%
    if(oneKS < 0.05 | oneSW < 0.05){
      warning("One or more tests returned a p-value below 5%.")
      testPassed <- FALSE
    }
  }
  
  # Specify the names of the methods
  names <- c("Marsaglia & Bray", "Box-Mueller", "Central Limit")
  
  # Compile a dataframe summarising the test results
  test.summary <- data.frame(names, KS.p.values, SW.p.values )
  print("Test passed:")
  print(testPassed)
  return(test.summary)
}