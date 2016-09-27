  # I confirm that the attached is my own work, except where clearly indicated in the text.

  # Student ID 160020697
  # MT4113

  # References
  #
  # Jones, O., Maillardet, R., and Robinson, A. (2014). Scientific Programming 
  # and Simulation Using R. 2nd Edition. Boca Raton, FL: CRC Press.
  #
  # Maindonald, J. (1984). Statistical Computation. New York: John Wiley and Sons.
  #
  # Mood, A., Graybill, F., and Boes, D. (1974). Introduction to the 
  # Theory of Statistics. 3rd Edition. New York: McGraw Hill.

  ################################## my.rnorm #####################################

  # This is the first function, my.rnorm. It should generate pairs of normally-distributed
  # variates according to the Marsaglia and Bray algorithm (Maindonald, 1984).

my.rnorm<-function(n=NULL,mean=0,sd=1){
  # my.rnorm has default values of mean=0 and sd=1. n does not have a default (It is
  # defaulted to NULL for coding purposes)

  # This section of code contains initial checks in order to determine whether
  # or not the inputs are acceptable for the my.rnorm function. If the input fails
  # any one of these checks, the function will stop and return an error message
  # of "invalid arguments".
  
  if(is.null(n)==TRUE){             # This checks if n has been given an argument not
                                    # equal to NULL. This will be equal to NULL if no 
                                    # argument has been given.
    
    stop("invalid arguments")       # Stops function if n is equal to NULL
  
  }
  
  if ((length(n)==1 & length(mean)==1 & length(sd)==1)==FALSE){                
                                    # For the purposes of this function, 
                                    # the n, mean, and sd parameters need 
                                    # to be scalars instead of vectors with lengths
                                    # that are not equal to 1.
  
    stop("invalid arguments")       # Stops function if false
  }
  
  if((is.finite(n)==TRUE & is.finite(mean)==TRUE & is.finite(sd)==TRUE)==FALSE) {  
                                    # The parameters need to have finite values. This
                                    # is.finite function also allows us to check if
                                    # the input values are numeric or not.
  
    stop("invalid arguments")       # Stops function if false
  }
  
  if(is.numeric(c(n,mean,sd))==FALSE){
                                    # This test is here in the case that the is.infinite
                                    # test missed any values. This test also forces the
                                    # input values to be strictly real numbers.
    
    stop("invalid arguments")       # Stops function if false
  }
          
  if ((n>0 & sd>0)==FALSE){         # n and sd should be strictly positive.
    
    stop("invalid arguments")       # Stops function if false
  }
  
  # If every one of the checks is passed, we can use the arguments provided to generate
  # our normally-distributed variates.
    
  check.divisible<-n/2              # For this method, we need n to be a positive
                                    # multiple of 2 as the method only produces
                                    # normally-distributed variates in pairs.
          
  check.div<-check.divisible-floor(check.divisible)
                                    # check.div will be equal to 0 if n is a
                                    # multiple of 2. Otherwise, check.div will not be
                                    # equal to 0.
          
  if(check.div!=0){                 # This is the actual check for check.div. This value
                                    # should be equal to 0 if n is actually divisible by 2.
            
    stop("invalid arguments")
  }  
  
  # If n is a valid number, we can use it to generate our normally-distributed variates.
            
  normal.numbers<-rep(0,n)          # This creates a vector of length n to store the 
                                    # generated normally-distributed variates.
                                    
            
  for(i in 1:check.divisible){      # For loop to generate n/2 pairs of variates
              
  # This section of the code contains the implementation of the Marsaglia and Bray algorithm
  # (Maindonald, 1984).               
              
    rad.unit<-2                     # This value is set to 2 so that our while loop will
                                    # run at least once. The rad.unit is the distance of a
                                    # point from (0,0) on an xy-plane.
              
    while(rad.unit>1){              # Two uniformly-distributed variates are generated and
                                    # used to plot the coordinates of a point within the unit
                                    # square with a sidelength of 2 centred about (0,0) on 
                                    # an xy-plane.
                                    # The variates are kept if the point lies within the
                                    # unit circle with radius 1 centred about (0,0) and
                                    # rejected otherwise. The process will continue until a
                                    # pair of U1 and U2 are kept.
   
      U1<-runif(1,0,1)*2-1
                                    # Generating a uniformly-distributed variate to convert
                                    # to an x-coordinate
      U2<-runif(1,0,1)*2-1
                                    # Generating a uniformly-distributed variate to convert
                                    # to a y-coordinate
      rad.unit<-U1^2 + U2^2
                                    # Using the x- and y-coordinates to determine the distance
                                    # of the point from (0,0)
    }
              
  # This section uses the U1 and U2 values that are kept to generate the normally-
  # distributed variates
    
  # The U1 and U2 values are multiplied by a factor of v=sqrt(-2log(w)/w) where
  # w = U1^2 + U2^2. 
              
    v.factor<-sqrt((-2*log(rad.unit,base=exp(1))/rad.unit))
                                    # The U1 and U2 are multiplied by this factor
                                    # to generate the normally-distributed variates, X1 and X2
    X1<-U1*v.factor
              
    X2<-U2*v.factor
                                    # The normally-distributed variates are stored in the
                                    # normal.numbers vector.
                                    
    normal.numbers[2*i-1]<-X1
                                    
    normal.numbers[2*i]<-X2
                                    
  }
                                    
  # The normally-distributed variates are converted from the standard normal
  # distribution to a normal distribution with mean = mean and sd = sd. The
  # default values of mean=0 and sd=1 are used if no values are given.       
                                    
  normal.numbers<-normal.numbers*sd+mean
                                    
  return(normal.numbers)            # Returns the values for the normally-distributed
                                    # variates

}

  ##################### general.rnorm ################################


  # This is the second function, general.rnorm. It allows the user to choose a method
  # out of three available methods: 1. Marsaglia and Bray algorithm (Maindonald, 1984),
  # 2. Box-Mueller algorithm (Jones et al., 2014), and 3. Central-limit theorem 
  # algorithm (Mood et al., 1974). 

general.rnorm<-function(n=NULL,mean=0,sd=1,method=1){
  # general.rnorm has default values of mean=0, sd=1, and method=1. n does not have
  # a default (It is defaulted to NULL for coding purposes)
  
  # This section of code will contain the checks to determine if the arguments
  # are valid. It will return a message of "invalid arguments" if any of the
  # arguments given are invalid
  
  if(is.null(n)==TRUE){             # We want n to have a value that is not NULL.
                                    # If no value for n is given, n is assigned
                                    # the default value of NULL and fails the test.
    
    stop("invalid arguments")       # Stops function if n is equal to NULL
    
  }
  
  if ((length(n)==1 & length(mean)==1 & length(sd)==1 & length(method)==1)==FALSE){                
                                    # For the purposes of this function, 
                                    # the n, mean, sd, and method arguments need 
                                    # to be scalars instead of vectors with lengths
                                    # that are not equal to 1. We will need to be more 
                                    # strict with method in the following code.
    
    stop("invalid arguments")       # Stops function if false
  }
  
  if((is.finite(n)==TRUE & is.finite(mean)==TRUE & is.finite(sd)==TRUE)==FALSE) {  
                                    # The parameters need to have finite values. This
                                    # is.finite function also allows us to check if
                                    # the input values are numeric or not.
                                    # We don't need to check method at this point as we
                                    # can make method only take on values of 1, 2, or 3.
    
    stop("invalid arguments")       # Stops function if false
  }
  
  if(is.numeric(c(n,mean,sd))==FALSE){
                                    # This test is here in the case that the is.infinite
                                    # test missed any values. This test also forces the
                                    # input values to be strictly real numbers.
    
    stop("invalid arguments")       # Stops function if false
  }
  
  if ((n>0 & sd>0)==FALSE){         # n and sd should be strictly positive.
    
    stop("invalid arguments")       # Stops function if false
  }
  
  # If every one of the checks is passed, we can use the arguments provided to generate
  # our normally-distributed variates.
  
  if(method==1){                      # User selects method 1 
  
  # The next block of code refers to method 1, the Marsaglia and Bray algorithm (Maindonald, 1984).
    
    check.divisible<-n/2              # For this method, we need n to be a positive
                                      # multiple of 2 as the method only produces
                                      # normally-distributed variates in pairs.
                                      
    check.div<-check.divisible-floor(check.divisible)
                                      # check.div will be equal to 0 if n is a
                                      # multiple of 2. Otherwise, check.div will not be
                                      # equal to 0.
    
    if(check.div!=0){                 # This is the actual check for check.div. This value
                                      # should be equal to 0 if n is actually divisible by 2.
      
      stop("invalid arguments")       # Stops if n is not divisible by 2
    }  
    
  # If n is a valid number, we can use it to generate our normally-distributed variates
    
    normal.numbers<-rep(0,n)          # This creates a vector of length n to store the 
                                      # generated normally-distributed variates.
    
    
    for(i in 1:check.divisible){      # For loop to generate n/2 pairs of variates
      
  # This section of the code contains the implementation of the Marsaglia and Bray algorithm
  # (Maindonald, 1984).               
      
      rad.unit<-2                     # This value is set to 2 so that our while loop will
                                      # run at least once. The rad.unit is the distance of a
                                      # point from (0,0) on an xy-plane.
      
      while(rad.unit>1){              # Two uniformly-distributed variates are generated and
                                      # used to plot the coordinates of a point within the unit
                                      # square with a sidelength of 2 centred about (0,0) on 
                                      # an xy-plane.
                                      # The variates are kept if the point lies within the
                                      # unit circle with radius 1 centred about (0,0) and
                                      # rejected otherwise. The process is repeated until 
                                      # a pair of U1 and U2 are kept
        
        U1<-runif(1,0,1)*2-1
                                      # Generating a uniformly-distributed variate to convert
                                      # to an x-coordinate
        U2<-runif(1,0,1)*2-1
                                      # Generating a uniformly-distributed variate to convert
                                      # to a y-coordinate
        rad.unit<-U1^2 + U2^2
                                      # Using the x- and y-coordinates to determine the distance
                                      # of the point from (0,0)
      }
      
  # This section uses the U1 and U2 values that are kept to generate the normally-
  # distributed variates.
      
  # The U1 and U2 values are multiplied by a factor of v=sqrt(-2log(w)/w) where
  # w = U1^2 + U2^2. 
      
      v.factor<-sqrt((-2*log(rad.unit,base=exp(1))/rad.unit))
                                      # The U1 and U2 are multiplied by this factor
                                      # to generate the normally-distributed variates, X1 and X2.
      X1<-U1*v.factor
      
      X2<-U2*v.factor
                                      # The normally-distributed variates are stored in the
                                      # normal.numbers vector.
      
      normal.numbers[2*i-1]<-X1
      
      normal.numbers[2*i]<-X2
    
    } 
  } else if(method==2){               # User selects method 2
    
  # This is the Box-Mueller algorithm (Jones et al., 2014). 
    
    check.divisible<-n/2              # For this method, we need "n" to be a positive
                                      # multiple of 2 as the method only produces
                                      # normally-distributed variates in pairs.
    
    check.div<-check.divisible-floor(check.divisible)
                                      # check.div will be equal to 0 if n is a
                                      # multiple of 2. Otherwise, check.div will not be
                                      # equal to 0.
    
    if(check.div!=0){                 # This is the actual check for check.div. This value
                                      # should be equal to 0 if n is actually divisible by 2.
      
      stop("invalid arguments")       # Stops if n is not divisible by 2
    }  
    
  # If n is a valid number, we can use it to generate our normally distributed variates
    
    normal.numbers<-rep(0,n)          # We have a vector of length n to store the generated
                                      # normally-distributed variates.
    
    for(i in 1:check.divisible){      # For loop to generate n/2 pairs of variates
    
      U1<-runif(1,0,1)                # Generate two uniformly-distributed variates 
      U2<-runif(1,0,1)
      
  # Use the two uniformly-distributed variates to generate two normally-distributed
  # variates using the Box-Mueller algorithm (Jones et al., 2014).
      
  # The algorithm uses two uniformly-distributed variates, U1 and U2, to generate two normally-
  # distributed variates, X1 and X2, such that
  # X1 = sin(2*pi*U1)*sqrt(-2*log(U2)) and
  # X2 = cos(2*pi*U1)*sqrt(-2*log(U2))  
                                      
                                      # Generating the pair of normally-distributed variates
      X1<-sin(2*pi*U1)*sqrt(-2*log(U2,base=exp(1)))
      
      X2<-cos(2*pi*U1)*sqrt(-2*log(U2,base=exp(1)))                      
                                      
                                      # Storing the pair in normal.numbers 
      normal.numbers[2*i-1]<-X1
      
      normal.numbers[2*i]<-X2
    }
      
  } else if(method==3){               # User selects method 3
    
  # This method is the Central-limit theorem algorithm (Mood et al., 1974).  
    
    if(n - floor(n)!=0){              # We need n to be a non-negative integer. n is
                                      # already greater than 0.
      
      stop("invalid arguments")       # Stops if n is not an integer     
    }
    
    normal.numbers<-rep(0,n)          # We have a vector of length n to store the generated
                                      # normally-distributed variates
    
  # For the algorithm (Mood et al., 1974), we need a large number of independent random variables.
  # We have chosen to use 16 independent, uniformly-distributed variates to generate a single 
  # normally-distributed variate, X.
    
  # The algorithm (Mood et al., 1974) the becomes
  # X = ((sum of 16 uniformly-distributed variates) - 8)*sqrt(12/16) 
        
    for(i in 1:n){                    # For loop to generate n normally-distributed variates
      
      U1<-runif(16,0,1)               # Generating a vector of 16 uniformly-distributed variates
      
                                      # Implementing the algorithm and storing the results in
                                      # normal.numbers
      normal.numbers[i] <- (sum(U1)-8)*sqrt(12/16)
    }
    
  } else stop("invalid arguments")  # Stops if method is not 1,2, or 3.
  
  # The normally-distributed variates are converted from the standard normal
  # distribution to a normal distribution with mean = mean and sd = sd. The
  # default values of mean=0 and sd=1 are used if no values are given.       
  
  normal.numbers<-normal.numbers*sd+mean
  
  return(normal.numbers)            # Returns the values for the normally-distributed
                                    # variates
}

  #################### Testing functions #############################################

  # The following programs can be used for testing the code

  # There are three testing functions:
  # test.rnorm, sum.test, and failtest.rnorm

  # The test.rnorm function allows the user to try inputing in values for either of the
  # rnorm functions, my.rnorm and general.rnorm. It will return the list of variates in a
  # vector form along with a histogram of the generated variates

test.rnorm <- function(pick=1,n=NULL,mean=0,sd=1,method=NULL){
                                  # The default function is my.rnorm when pick = 1. The
                                  # User can choose to use pick = 2 to use the general.rnorm
                                  # function. n, mean, and sd have the same default values
                                  # as the my.rnorm and general.rnorm functions.
  
  if(pick==1){                    # User selects the my.rnorm function
    
    if (is.null(method)==FALSE){  # Method is not null
      
      stop("my.rnorm does not use the method argument")
                                  # To prevent the user from using an unused argument
    }
    
    test.norm<-my.rnorm(n,mean,sd)
                                  # Stores the generated variates in an object test.norm
    
  } else if(pick==2){             # User selects the general.rnorm function
    
    if (is.null(method)==TRUE){   # User either has not set a method value or has set it
                                  # to NULL
      
      method<-1                   # method is defaulted to a value of 1
      
    }
    test.norm<-general.rnorm(n,mean,sd,method)
                                  # Stores the generated variates in an object test.norm 
    
  } else stop("Please use pick=1 for my.rnorm and pick=2 for general.rnorm")
                                  # Error message to let the user know how to select the
                                  # proper functions
  
  hist(test.norm, xlab="Variate values", ylab="Frequency",
       main=c("Histogram of generated, normally-distributed variates",
              paste("using n =", n,", mean=", mean, ", and sd=", sd)))                 
                                  # Generates a histogram of the generated variates
    
  return(test.norm)               # Returns the generated variates
  
}

  # The following sum.test provides summary statistics for a set of generated
  # normally-distributed variates.
  # The list of variates are not returned but a histogram of the variates is provided

sum.test<-function(pick=1,n=NULL,mean=0,sd=1,method=NULL){
  # The default function is my.rnorm when pick = 1. The
  # User can choose to use pick = 2 to use the general.rnorm
  # function. n, mean, and sd have the same default values
  # as the my.rnorm and general.rnorm functions.
  
  if(pick==1){                    # User selects the my.rnorm function
    
    if (is.null(method)==FALSE){  # Method is not null
      
      stop("my.rnorm does not use the method argument")
      # To prevent the user from using an unused argument
    }
    
    test.norm<-my.rnorm(n,mean,sd)
    # Stores the generated variates in an object test.norm
    
  } else if(pick==2){             # User selects the general.rnorm function
    
    if (is.null(method)==TRUE){   # User either has not set a method value or has set it
      # to NULL
      
      method<-1                   # method is defaulted to a value of 1
      
    }
    test.norm<-general.rnorm(n,mean,sd,method)
    # Stores the generated variates in an object test.norm 
    
  } else stop("Please use pick=1 for my.rnorm and pick=2 for general.rnorm")
  # Error message to let the user know how to select the
  # proper functions
  
  if (length(test.norm)==n){      # Tests if the function has generated the right
                                  # number of variates
    
    print(paste("The function has generated the correct number of variates", n))
                                  # The function has succeeded, print number of variates
  } else print(paste("The function has generated an incorrect number of variates", length(test.norm),
               "the correct number of variates is", n))
                                  # The function has failed, print wrong number and then right number
  print(paste("The sample mean of the variates is", round(mean(test.norm),digits=4)))
                                  # Prints the sample mean of the variates
  print(paste("The absolute difference between the sample mean of the variates and the actual mean by", 
              round(abs(mean(test.norm)-mean),digits=4)))
                                  # Prints the absolute difference between the sample mean and the actual mean 
  print(paste("The sample standard deviation is", round(sqrt(var(test.norm)),4)))
                                  # Prints the sample standard deviation
  print(paste("The absolute difference between the sample standard deviation of the variates and the actual standard deviation by", 
              round(abs(sqrt(var(test.norm))-sd),digits=4)))
                                  # Prints the absolute difference between the sample standard
                                  # deviation and the actual standard deviation
  hist(test.norm, xlab="Variate values", ylab="Frequency",
       main=c("Histogram of generated, normally-distributed variates",
              paste("using n =", n,", mean=", mean, ", and sd=", sd)))                 
                                  # Generates a histogram of the generated variates
  return()                        # Does not return anything
  
}
  ############################# Support functions ###################
 
  # The following generate.* functions are used in the failtest.rnorm function

generate.odd<-function(work=NULL){              # Function for generating odd numbers
  
  if(work==1){                                  # Only works if given a value of 1
    
    random.ness<-floor(1000*runif(1,0,1))
    return(random.ness*2 + 3)                   # Generates an odd number
    
  }
  
}

generate.noninteger<-function(work=NULL){       # Function for generating non-integer values
  
  if(work==1){                                  # Only works if given a value of 1
    
    random.ness<-floor(1000*runif(1,0,1))
    return(random.ness*2 + 0.01)                # Generates a non-integer value
    
  }
  
}

generate.complex<-function(work=NULL){          # Function for generating a complex value
  
  if(work==1){                                  # Only works if given a value of 1
    
    random.ness<-floor(1000*runif(1,0,1))
    return(complex(1,random.ness+1,random.ness+2))  
                                                # Generates a complex value
  }
  
}

generate.negative<-function(work=NULL){         # Generates a negative value
  
  if(work==1){                                  # Only works if given a value of 1
   
    random.ness<-floor(1000*runif(1,0,1))
    return(-1*random.ness*2-2)                  # Generates a negative value
    
  }
  
}

generate.vector<-function(work=NULL){           # Function for generating vectors of length 2
  
  if(work==1){                                  # Only works if given a value of 1
    
    random.ness<-floor(1000*runif(1,0,1))
    return(c(random.ness+1,random.ness+2))      # Generates a vector of length 2
    
  }
  
}
  # The failtest.rnorm function will try to input inappropriate data for each of the arguments.
  # Each argument is tested individually.
  
  # For the following failtest.rnorm function, the tests are expected to result in error messages
  # for the listed arguments.

  # Infinite    -   n   mean  sd
  # Odd         -   n (only my.rnorm and general.rnorm, 1 and 2)
  # Non-integer -   n   
  # String      -   n   mean  sd
  # Complex     -   n   mean  sd
  # Vector      -   n   mean  sd
  # Negative    -   n         sd

failtest.rnorm<-function(pick=1,argument=1,test=1, method=NULL){
  # This is the function for testing/trapping the my.rnorm and general.rnorm functions if
  # invalid arguments are given. Not all tests are expected to result in errors.
  # Pick 1 - my.rnorm, Pick 2 - general.rnorm
  # Argument 1- n, Argument 2 - mean, Argument 3 - sd
  # Test 1 - Infinite, Test 2 - Odd, Test 3 - Non-integer, Test 4 - String
  # Test 5 - Complex, Test 6 - Vector, Test 7 - Negative
  
  
  
  if(pick==1){                              # User chooses my.rnorm or default value is used
    
    if(is.null(method)==FALSE){             # Checks if the method argument
                                            # is used        
      
      stop("my.rnorm does not use the argument 'method'")
                                            # Stops if method is used for my.rnorm
    }
    
    if(argument==1){                        # n
      
      if(test==1){                          # Infinite values
        
        my.rnorm(Inf)                       # Tests function, n = Inf
        
      } else if(test==2){                   # Odd values
        
        n<-generate.odd(1)                  # Makes n an odd value
      
        my.rnorm(n)                         # Tests function
        
      } else if(test==3){                   # Non-integer values
        
        n<-generate.noninteger(1)           # Makes n a non-integer value
        
        my.rnorm(n)                         # Tests function
        
      } else if(test==4){                   # Strings
        
        n<-"2"                              # Makes n a string
        
        my.rnorm(n)                         # Tests function
        
      } else if(test==5){                   # Complex values
        
        n<-generate.complex(1)              # Makes n a complex value
        
        my.rnorm(n)                         # Tests function

      } else if(test==6){                   # Vectors
        
        n<-generate.vector(1)               # Makes n a vector of length 2
        
        my.rnorm(n)                         # Tests function
        
      } else if(test==7){                   # Negative values
        
        n<-generate.negative(1)             # Makes n negative
        
        my.rnorm(n)                         # Tests function
        
      } else stop("Please pick a test from 1 to 7")
                                            # Error message (Didn't pick a valid test)
      
    } else if(argument==2){                 # mean
      
      if(test==1){                          # Infinite values
        
        my.rnorm(n=100, mean=Inf)           # Tests function, mean = Inf 
        
      } else if(test==2){                   # Odd values
        
        k<-generate.odd(1)                  # Makes mean an odd value
        
        my.rnorm(n=100, mean=k)             # Tests function
        
      } else if(test==3){                   # Non-integer values
        
        k<-generate.noninteger(1)           # Makes mean a non-integer value
        
        my.rnorm(n=100, mean=k)             # Tests function
        
      } else if(test==4){                   # Strings
        
        k<-"2"                              # Makes mean a string
        
        my.rnorm(n=100, mean=k)             # Tests function
        
      } else if(test==5){                   # Complex values
        
        k<-generate.complex(1)              # Makes mean a complex value
        
        my.rnorm(n=100, mean=k)             # Tests function
        
      } else if(test==6){                   # Vectors
        
        k<-generate.vector(1)               # Makes mean a vector of length 2
        
        my.rnorm(n=100, mean=k)             # Tests function
        
      } else if(test==7){                   # Negative values
        
        k<-generate.negative(1)             # Makes mean a negative value
        
        my.rnorm(n=100, mean=k)             # Tests function
      
      } else stop("Please pick a test from 1 to 7")
                                            # Error message (Didn't pick a valid test)
      
    } else if(argument==3){                 # sd
      
      if(test==1){                          # Infinite values
        
        my.rnorm(n=100, sd=Inf)             # Tests function, sd = Inf
        
      } else if(test==2){                   # Odd values
        
        k<-generate.odd(1)                  # Makes sd an odd value
        
        my.rnorm(n=100, sd=k)               # Tests function
        
      } else if(test==3){                   # Non-integer
        
        k<-generate.noninteger(1)           # Makes sd a non-integer
        
        my.rnorm(n=100, sd=k)               # Tests function
        
      } else if(test==4){                   # Strings
        
        k<-"2"                              # Makes sd a string
        
        my.rnorm(n=100, sd=k)               # Tests function
        
      } else if(test==5){                   # Complex values
        
        k<-generate.complex(1)              # Makes sd a complex value
        
        my.rnorm(n=100, sd=k)               # Tests function
        
      } else if(test==6){                   # Vectors
        
        k<-generate.vector(1)               # Makes sd a vector of length 2
        
        my.rnorm(n=100, sd=k)               # Tests function
        
      } else if(test==7){                   # Negative values
        
        k<-generate.negative(1)             # Makes sd a negative value
        
        my.rnorm(n=100, sd=k)               # Tests function
        
      } else stop("Please pick a test from 1 to 7")
                                            # Error message (Didn't pick a valid test)
      
    } else stop("Please pick an argument from 1 to 3")
                                            # Error message (Didn't pick a valid argument)
    
  } else if(pick==2){                       # User chooses general.rnorm
    
    p<-method                               # Converts argument into a more useable object
    
    if(is.null(p)==TRUE){                   # Sets default value
      
      p<-1                                  # Default value of method for general.rnorm
      
    }
    
    if(p!=1){                               # Method is not 1
      
      if(p!=2){                             # Method is not 2
      
        if(p!=3){                           # Method is not 3
        
          general.rnorm(n=100,method=p)     # Runs general.rnorm
          
        }
        
      }
      
    }
    
    if(argument==1){                        # n
      
      if(test==1){                          # Infinite values
        
        general.rnorm(Inf,method=p)         # Tests function
        
      } else if(test==2){                   # Odd values
        
        n<-generate.odd(1)                  # Makes n an odd value
        
        general.rnorm(n,method=p)           # Tests function
        
      } else if(test==3){                   # Non-integer values
        
        n<-generate.noninteger(1)           # Makes n a non-integer value
        
        general.rnorm(n,method=p)           # Tests function
        
      } else if(test==4){                   # String
        
        n<-"2"                              # Makes n a string
        
        general.rnorm(n,method=p)           # Tests function
        
      } else if(test==5){                   # Complex values
        
        n<-generate.complex(1)              # Makes n a complex value
        
        general.rnorm(n,method=p)           # Tests function
        
      } else if(test==6){                   # Vectors
        
        n<-generate.vector(1)               # Makes n a vector of length 2
        
        general.rnorm(n,method=p)           # Tests function
        
      } else if(test==7){                   # Negative values
        
        n<-generate.negative(1)             # Makes n a negative value
        
        general.rnorm(n,method=p)           # Tests function
        
      } else stop("Please pick a test from 1 to 7")
                                            # Error message (Didn't pick a valid test)
      
    } else if(argument==2){                 # Mean
      
      if(test==1){                          # Infinite values
        
        general.rnorm(n=100, mean=Inf,method=p)
                                            # Tests function with mean as an infinite value
      } else if(test==2){                   # Odd values
        
        k<-generate.odd(1)                  # Makes mean an odd value
        
        general.rnorm(n=100, mean=k, method=p)
                                            # Tests function
      } else if(test==3){                   # Non-integer
        
        k<-generate.noninteger(1)           # Makes mean a non-integer value
        
        general.rnorm(n=100, mean=k, method=p)
                                            # Tests function
      } else if(test==4){                   # String
        
        k<-"2"                              # Makes mean a string
        
        general.rnorm(n=100, mean=k, method=p)
                                            # Tests function
      } else if(test==5){                   # Complex values
        
        k<-generate.complex(1)              # Makes mean a complex value
        
        general.rnorm(n=100, mean=k, method=p)
                                            # Tests function
      } else if(test==6){                   # Vectors
        
        k<-generate.vector(1)               # Makes mean a vector of length 2
        
        general.rnorm(n=100, mean=k, method=p)
                                            # Tests function
      } else if(test==7){                   # Negative values
        
        k<-generate.negative(1)             # Makes mean a negative value
        
        general.rnorm(n=100, mean=k, method=p)
                                            # Tests function
      } else stop("Please pick a test from 1 to 7")
                                            # Error message (Didn't select a valid test)
    } else if(argument==3){                 # sd
      
      if(test==1){                          # Infinite values
        
        general.rnorm(n=100, sd=Inf, method=p)
                                            # Tests function with sd as an infinite value
      } else if(test==2){                   # Odd values
        
        k<-generate.odd(1)                  # Makes sd an odd value
        
        general.rnorm(n=100, sd=k, method=p)
                                            # Tests function
      } else if(test==3){                   # Non-integer values
        
        k<-generate.noninteger(1)           # Makes sd a non-integer value
        
        general.rnorm(n=100, sd=k, method=p)
                                            # Tests function
      } else if(test==4){                   # String
        
        k<-"2"                              # Makes sd a string
        
        general.rnorm(n=100, sd=k, method=p)
                                            # Tests function
      } else if(test==5){                   # Complex values
                                  
        k<-generate.complex(1)              # Makes sd a complex value
        
        general.rnorm(n=100, sd=k, method=p)
                                            # Tests function
      } else if(test==6){                   # Vectors
        
        k<-generate.vector(1)               # Makes sd a vector of length 2
        
        general.rnorm(n=100, sd=k, method=p)
                                            # Tests function
      } else if(test==7){                   # Negative values
        
        k<-generate.negative(1)             # Makes sd a negative value
        
        general.rnorm(n=100, sd=k, method=p)
                                            # Tests function
      } else stop("Please pick a test from 1 to 7")
                                            # Error message (Didn't select a valid test)
    } else stop("Please pick an argument from 1 to 3")
                                            # Error message (Didn't select a valid argument)
  } else stop("Please pick a 'pick' value of 1 or 2")
                                            # Error message (Didn't select a valid function)
  print("This passes")                      # Print this if there are no error messages
  
}