#I confirm that the attached is my own work, except where clearly indicated otherwise

#my.rnorm function
my.rnorm <- function(n="default", mean=0, sd=1) { #create function my.rnorm that takes aguments n (number of vales to return), default mean = 0 (mean of values to return) and default standard deviation = 1 (sd of values to return)
   if (length(n) != 1 | length(mean) != 1 | length(sd) != 1) { #if function to ensure arguments are scalar   
      return("invalid arguments")
   } else if (is.numeric(n) != TRUE | is.numeric(mean) != TRUE | is.numeric(sd) != TRUE) { #test if arguments are numeric
      return("invalid arguments") 
   } else if (n < 0 | (n%%1) != 0) { #if loop returns input error if n is not a integer gretaer than or equal to 0 
      return("invalid arguments")
   } else if (sd < 0) { #else if to ensure standard deviation is positive
      return("invalid arguments")
   } else if (n == 0) { #create empty vector if n is 0
      vector <- vector(mode="numeric", length=0)
   } else { # continues function if number of values n to return is equal to or greater than 1 
      vector <- vector(mode="numeric", length=0) #create empty vector that can be added to
      for (i in 1:ceiling((n/2))) { #for loop to generate (n/2) if even, (n+1)/2 if odd number of pairs X1 and X2  
         i.w <- 2 #define w greater than 1 so that it can be used in while function as a condition
         while (i.w > 1) { #while loop that terminates once w is equal or less than 1 producing U1 and U2
            i.U1 <- runif(1, min = -1, max = 1) #runif function generates random deviate from a uniform distribution over the interval(-1,1) that is assigned U1  
            i.U1 <- 2*i.U1 - 1 #U1 transformed to unit square
            i.U2 <- runif(1, min = -1, max = 1) #runif function generates random deviate from a uniform distribution over the interval(-1,1) that is assigned U2 
            i.U2 <- 2*i.U2 - 1 #U2 transformed to unit square
            i.w <- i.U1**2 + i.U2**2 #generate w used for rejection step, terminating the while loop producing suitable U1 and U2
         } # end of while loop
         i.v <- sqrt((-2*log(i.w))/i.w) #generate v from already generated w
         i.X1 <- i.U1 * i.v #generate value X1 to be returned
         i.X2 <- i.U2 * i.v #generate value X2 to be returned
         i.X1 <- i.X1*sd + mean #allow default returned value X1 to be altered by altered mean/sd
         i.X2 <- i.X2*sd + mean #allow default returned value X2 to be altered by altered mean/sd 
         vector <- append(vector, c(i.X1,i.X2)) #attach generated variables X1 and X2 to vector       
      } #end for loop
      if ((n%%2) != 0) { #if loop to remove final X2 if n is odd so vector returns n values
         x <- length(vector)
         vector <- vector[-x]
      } else { # if n is even, vector returns all X1 and X2
         vector <- vector 
      } # end of else loop
   } #end of if/ else loop
   return(vector) #returns vector of n values comrpised of X1 and X2 values
} #end of function     

#general.rnorm function
general.rnorm <- function(n="default", mean=0, sd=1, method=1) { #create function general.rnorm that takes the same arguments as my.rnorm with the addition of method
   if (length(n) != 1 | length(mean) != 1 | length(sd) != 1 | length(method) != 1) { #if function to ensure arguments are scalar   
      return("invalid arguments")
   } else if (is.numeric(n) != TRUE | is.numeric(mean) != TRUE | is.numeric(sd) != TRUE | is.numeric(method) != TRUE) { #test if arguments are numeric
      return("invalid arguments") 
   } else if (n < 0 | (n%%1) != 0) { #if loop returns input error if n is not a integer greater than or equal to 0 
      return("invalid arguments")
   } else if (sd < 0) { #else if to ensure standard deviation is positive
      return("invalid arguments")
   } else if (method < 1 | method > 3 | (method%%1) !=0) { #ensure method selected = 1, 2 or 3
      return("invalid arguments")
   } else if (n == 0) { #create empty vector if n is 0
      vector <- vector(mode="numeric", length=0)
   } else if (method == 1) { #else if command to return n values using Marsaglia and Brays method
      return(my.rnorm(n,mean,sd))
   } else if (method == 2) { # else if command to return n values using Box and Mueller method
      vector <- c() #create empty vector that can be added to
      for (i in 1:ceiling((n/2))) { #for loop to generate (n/2) for even, (n+1)/2 for odd number of pairs X1 and X2 totalling n values
         i.U1 <- runif(1) #runif function generates random deviate from a uniform distribution over the interval(0,1) that is assigned U1
         i.U2 <- 0
         while (i.U2 <= 0) { #while function to produce U2 that is > 0 and <= 1 from a uniform distributuion so that -2*log(U2) >= 0 so the function sqrt() returns a real number
            i.U2 <- runif(1)
         } # end while function 
         i.X1 <- sin(2*pi*i.U1) * sqrt(-2*log(i.U2)) #generate value X1 to be returned
         i.X2 <- cos(2*pi*i.U1) * sqrt(-2*log(i.U2)) #generate value X2 to be returned
         i.X1 <- i.X1*sd + mean #allow default returned value X1 to be altered by altered mean/sd
         i.X2 <- i.X2*sd + mean #allow default returned value X2 to be altered by altered mean/sd 
         vector <- append(vector, c(i.X1,i.X2)) #attach generated variables X1 and X2 to vector       
      } #end for loop
   if ((n%%2) != 0) { #if loop to remove final X2 if n is odd so vector returns n values
      x <- length(vector) #defines x to be the length of the vector equal to n+1
      vector <- vector[-x] #removes (n+1)th value to make length of vector equal to n
   } else { # if n is even, vector returns all X1 and X2
      vector <- vector 
   } # end of else loop
   } else if (method == 3) { #else if command to return n values using central-limit theroem where n is between 1:16
      vector <- c()
      for (i in 1:n) { #for loop to generate n values of X
         sum.U <- 0 # define sum.U  
         for (j in 1:16) { #for loop to generate 16 j.U and sum 16 generated U
            j.U <- runif(1) #define j.U as a random deviate between interval (0,1)
            sum.U <- sum.U + j.U
         } #end of for j loop
         i.X <- ((sum.U)-8) * sqrt(12/16) #define x using central limit calculation
         i.X <- i.X*sd + mean #transform X to respond to changes from default mean and sd               
         vector <- append(vector, i.X) #attach generated variables X1 and X2 to vector       
      } #end for loop
   } else { #returns error no arguments matched
      return("invalid arguments")
   } #end of if/else 
   return(vector) #returns vector of n values comrpised of X1 and X2 values
} #end of function 

#1st test function
#Test if function my.rnorm returns a vector of length n containg numbers only, given logical arguments n, mean and sd
test.my.vector <- function(n="default", mean=0, sd=1) { #create test function that takes arguments n, mean and sd 
   if (length(n) != 1 | length(mean) != 1 | length(sd) != 1) { #if function to ensure arguments are scalar   
      return("invalid arguments")
   } else if (is.numeric(n) != TRUE | is.numeric(mean) != TRUE | is.numeric(sd) != TRUE) { #test if arguments are numeric
      return("invalid arguments") 
   } else if (n < 0 | (n%%1) != 0) { #if loop returns input error if n is not a integer greater than or equal to 0 
      return("invalid arguments")
   } else if (sd < 0) { #else if to ensure standard deviation is positive
      return("invalid arguments")
   } else {
      x <- my.rnorm(n=n,mean=mean,sd=sd) #define x to equal vector generated by my.rnorm and arguments
      if (length(x)==n & is.numeric(x)) { #if conditions to see if the number of values returned is equal to a and they are all numeric
         return("PASS") #returns true if test conditions are met
      } else {
         return("FAIL") #returns fail if test conditions are not met
      } #end else 
   } #end else
} #end test function

#2nd test function
#Test if function general.rnorm returns a vector of length a containg numbers only, given arguments n, mean and method
test.general.vector <- function(n="default", mean=0, sd=1, method=1) { #create test function that takes argument a and selects method
   if (length(n) != 1 | length(mean) != 1 | length(sd) != 1) { #if function to ensure arguments are scalar   
      return("invalid arguments")
   } else if (is.numeric(n) != TRUE | is.numeric(mean) != TRUE | is.numeric(sd) != TRUE) { #test if arguments are numeric
      return("invalid arguments") 
   } else if (n < 0 | (n%%1) != 0) { #if loop returns input error if n is not a integer greater than or equal to 0 
      return("invalid arguments")
   } else if (sd < 0) { #else if to ensure standard deviation is positive
      return("invalid arguments")
   } else if (method < 1 | method > 3 | (method%%1) !=0) { #ensure method selected = 1, 2 or 3
      return("invalid arguments")
   } else {
      x <- general.rnorm(n=n,mean=mean,sd=sd,method=method) #define x to equal vector generated by my.rnorm and arguments
      if (length(x)==n & is.numeric(x)) { #if conditions to see if the number of values returned is equal to a and they are all numeric
         return("PASS") #returns true if test conditions are met
      } else {
         return("FAIL") #returns fail if test conditions are not met
      } #end else 
   }
} #end test function

#3rd test function
#Test if function my.rnorm and general.rnorm return a vector who's observed mean lies with the standard deviation (0,y) of the expected mean=x over 1000 values
test.my.mean <- function(mean, sd, method=1) { #create test function with that takes arguments desired mean, standard deviation and default method=1
   if (length(mean) != 1 | length(sd) != 1) { #if function to ensure arguments are scalar   
      return("invalid arguments")
   } else if (is.numeric(mean) != TRUE | is.numeric(sd) != TRUE) { #test if arguments are numeric
      return("invalid arguments") 
   } else if (sd < 0) { #else if to ensure standard deviation is positive
      return("invalid arguments")
   } else if (method < 1 | method > 3 | (method%%1) !=0) { #ensure method selected = 1, 2 or 3
      return("invalid arguments")   
   } else {
      my.observed.m <- mean(my.rnorm(n=1000,mean=mean,sd=sd)) #define m.observed.mean to equal the mean of my.rnorm using arguments n=1000, mean and sd 
      gen.observed.m <- mean(general.rnorm(n=1000,mean=mean,sd=sd,method=method)) #define gen.observed.mean to equal the mean of general.rnorm using arguments n=1000, mean and sd 
      my.difference.m <- (my.observed.m-mean)**2 #define difference between desired and observed mean for my.rnorm, squared to make it positive
      gen.difference.m <- (gen.observed.m-mean)**2 #define difference between desired and observed mean for general.rnorm, squared to make it positive
      if (my.difference.m < sd**2 & gen.difference.m < sd**2) { 
         return("PASS") #return pass if observed mean is within sd of desired mean
      } else { 
         return("Fail") #return fail if observed mean is outside sd of desired mean
      } #end if loop
   } #end of else loop
} #end function