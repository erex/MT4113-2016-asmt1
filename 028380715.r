# I confirm that the attached work is my own work, except where clearly indicated in the text.

# my.rnorm function -------------

my.rnorm <- function(n,mean=0,sd=1){
  # Set up vector to hold normal values generated in function
  vec2 <- vector(mode="numeric",length=0)
  
  # Create dummy variable used in checks
  dummy <- c(n,mean,sd)
  
  # Check only numerical data types are entered into function
  # If any valued entered is not numerical then stop function
  p<-1
  for (p in 3){
    if (is.numeric(dummy[p])!=TRUE){
      print("non-numerical data type entered")
      stop("invalid arguments")}
    else{
      p<-p+1
    } 
  }
  
  # Check no vectors are entered
  # If the length is not 3 (ie. a non-scalar has been input) then stop function
  if (length(dummy) !=3){
    print("vector entered")
    stop("invalid arguments")
  }
  
  # Check the number of normal values to return is an integer value
  # If the rounded value of n is not the same as the original value of n then
  # we know an integer has not been entered so we stop the function
  if (round(dummy[1])!=dummy[1]){
    print("output number of normal values must be an integer")
    stop("invalid arguments")
  }
  
  # Check the number of normal values to return is greater than 0
  if (dummy[1] < 1){
    print("output number of normal values must be greater than 0")
    stop("invalid arguments")
  }
  
  # Check the sd is non-negative
  if (dummy[3]<0){
    print("sd less than 0")
    stop("invalid arguments")
  }
  
  # Set initial value of i
  i<-1
  # First while loop
  while (i <= (n/2+0.5)){
    # Preload w to be out of range
    w <- 2
    # Loop to ensure w < 1
    while (w > 1) {
      # Vector of two random uniform values
      uniform <- runif(2)
      uniform <- 2*uniform -1
      w<-((uniform[1])^2+(uniform[2])^2)
    }
    # Calculations to convert uniform values to normal values
    v<-(-2*log(w)/w)^0.5
    normal <- (uniform * v)
    # Adjustment for different mean and sd
    normal <- normal*sd + mean
    # If statement to check for the last loop of an odd number
    if ((n/2+0.5)==i){
      # This is true for the last loop of odd number
      # Adds only one of the normal dist numbers
      vec <- c(normal[1])
    } else {
      # All other scenarios add both numbers to vector
      vec <- c(normal[1],normal[2])
    }
    # Append values in vec to our vec2 vector
    vec2 <- c(vec2,vec)
    i=i+1
  }
  return(vec2)
}

# general.rnorm function ----------

general.rnorm <- function(n,mean=0,sd=1,method=1){
  # Set up vector to hold normal values generated in function
  vec2 <- vector(mode="numeric",length=0)
  
  # Create dummy variable used in checks
  dummy <- c(n,mean,sd)
  
  # Check only numerical data types are entered into function
  # If any valued entered is not numerical then stop function
  p<-1
  for (p in 3){
    if (is.numeric(dummy[p])!=TRUE){
      print("non-numerical data type entered")
      stop("invalid arguments")
    }
    else{
      p<-p+1
    } 
  }
  
  # Check no vectors are entered
  # If the length is not 3 (ie. a non-scalar has been input) then stop function
  if (length(dummy) !=3){
    print("vector entered")
    stop("invalid arguments")
  }
  
  # Check the number of normal values to return is an integer value
  # If the rounded value of n is not the same as the original value of n then
  # we know an integer has not been entered so we stop the function
  if (round(dummy[1])!=dummy[1]){
    print("output number of normal values must be an integer")
    stop("invalid arguments")
  }
  
  # Check the number of normal values to return is greater than 0
  if (dummy[1] < 1){
    print("output number of normal values must be greater than 0")
    stop("invalid arguments")
  }
  
  # Check the sd is non-negative
  if (dummy[3]<0){
    print("sd less than 0")
    stop("invalid arguments")
  }
  
  # Set initial value of i
  i<-1
  # If statement for method 1
  if (method==1){
    vec2 <- my.rnorm(n,mean,sd)
  } else if (method==2){
    # While loop
    while (i <= (n/2+0.5)){
      # Vector of two random uniform values
      uniform <- runif(2)
      x1<-sin(2*pi*uniform[1])*(-2*log(uniform[2]))^0.5
      x2<-cos(2*pi*uniform[1])*(-2*log(uniform[2]))^0.5
      normal <- c(x1,x2)
      # Adjustment for different mean and sd
      normal <- normal*sd + mean
      # If statement to check for the last loop of an odd number
      if ((n/2+0.5)==i){
        # This is true for the last loop of odd number
        # Adds only one of the normal dist numbers
        vec <- c(normal[1])
      } else {
        # All other scenarios add both numbers to vector
        vec <- c(normal[1],normal[2])
      }
      # Append values in vec to our vec2 vector
      vec2 <- c(vec2,vec)
      i=i+1
    }
  }
  # Else statement to use method 3
  else if (method==3){
    # While loop
    while (i<=n){
      # Vector of 16 uniform values
      uniform <- runif(16)
      normal <- (sum(uniform)-8)*(12/16)^0.5
      # Adjustment for different mean and sd
      normal <- normal*sd + mean
      # Append value to our vector vec2
      vec2 <- c(vec2,normal)
      i=i+1
    }
    # Else statement for value entered for method other than 1, 2 or 3
  } else {
    print("method needs to be either 1, 2 or 3")
    stop("invalid arguments")
  }
  return(vec2)
}

# Test Functions ------------

# Overall function to check all functions run true for a given input
general.test <- function(n=5,mean=0,sd=1){
  if (test.length(n,mean,sd)==TRUE & test.method1(n,mean,sd)==TRUE & test.method2(n,mean,sd)==TRUE & test.method3(n,mean,sd)==TRUE & test.numeric(n,mean,sd)==TRUE){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# rnorm test functions

# 1. Function to check the length of the output vector
# of normal values is the same as the number of values we wanted to return.
# We would expect the test.length function to return TRUE to pass the test
test.length <- function(n,mean=0,sd=1){
  a <- my.rnorm(n,mean,sd)
  # if the length of a is the same as the number of output values we expected
  # then return TRUE, otherwise return FALSE
  if (length(a)==n){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# 2. Function to check the output is numerical
test.numeric <- function(n,mean=0,sd=1){
  a <- my.rnorm(n,mean,sd)
  if (is.numeric(a)==TRUE){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# 3. Function to check the output values are from the normal distribution
# Can also enter different values for mean and sd and check the dist looks approx what we would expect.
# Enter a high value of n and visibly check if the distribution looks approx normal.
test.normal <- function(n,mean=0,sd=1){
  a <- my.rnorm(n,mean,sd)
  hist(a)
  return(NULL)
}


# general.rnorm test functions

# 1. Function to check method 1 works
test.method1 <- function(n,mean=0,sd=1,method=1){
  a <- general.rnorm(n,mean,sd,method)
  # check the length is what we expect and all outputs are numerical values
  if (length(a)==n & is.numeric(a)==TRUE){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# 2. Function to check method 2 works
test.method2 <- function(n,mean=0,sd=1,method=2){
  a <- general.rnorm(n,mean,sd,method)
  # check the length is what we expect and all outputs are numerical values
  if (length(a)==n & is.numeric(a)==TRUE){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# 3. Function to check method 3 works
test.method3 <- function(n,mean=0,sd=1,method=3){
  a <- general.rnorm(n,mean,sd,method)
  # check the length is what we expect and all outputs are numerical values
  if (length(a)==n & is.numeric(a)==TRUE){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# 4. Function to check the output values are from the normal distribution
# Can also enter different values for mean and sd and check the dist looks approx what we would expect.
# Enter a high value of n and visibly check if the distribution looks approx normal.
test.normal.general <- function(n,mean=0,sd=1, method){
  a <- general.rnorm(n,mean,sd,method)
  hist(a)
  return(NULL)
} 