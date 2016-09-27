#I confirm that the attached is my own work, except where clearly indicated in the text.

#a function to produce n random variates from the normal distribution
my.rnorm <- function(n,mean=0,sd=1) {

  
  #checks that the input values are numbers
  if (is.numeric(n)==FALSE|is.numeric(mean)==FALSE|is.numeric(sd)==FALSE){
    return("invalid arguments")
  }
  #checks that the input values are scalars
  if (length(n)!=1|length(mean)!=1|length(sd)!=1) {
    return("invalid arguments")
  }
  #checks n is >= 1 and a whole number
  if (n<1|n%%1!=0){
    return("invalid arguments")
  }
  
  m<-ceiling(n/2) #if a method returns 2 values, this will allow the fuction to run half the number of times
  x<-c() #assigning x as a blank vector
  
  #runs the Marsaglia and Bray algorithm m times to produce m random variates
  for (i in 1:m) { 
    
    #generates uniform deviates distributed over 1,-1
    u1 <- 2*runif(1,0,1)-1
    u2 <- 2*runif(1,0,1)-1
    w <- (u1^2) + (u2^2) #used in rejection step
    
    #rejection step, rejects values outside the required range (w = (u1^2) + (u2^2) < 1)
    while (w > 1) {
      u1 <- 2*runif(1,0,1)-1
      u2 <- 2*runif(1,0,1)-1
      w <- (u1^2) + (u2^2)
    }
    
    #part of the Marsaglia and Bray algorithm
    v<-sqrt(-2*log(w)/w)
    
    #defines normally distributed variates
    x1<-u1*v*sd+mean
    x2<-u2*v*sd+mean
    
    #gives answer in a vector
    x<-c(x,x1,x2)
  }
  
  #removes a value from the answer if n is odd as the method only generates even numbers of variates.
  if (n%%2!=0) {
    x<-x[1:n]
  }
  
  #return x
  return(x)
}


###################################################
###----------------general.rnorm----------------###
###################################################


#a function to produce n random variates from the normal distribution using different methods
general.rnorm <- function(n,mean=0,sd=1,method=1) {
  
  
  #checks that the input values are numbers
  if (is.numeric(n)==FALSE|is.numeric(mean)==FALSE|is.numeric(sd)==FALSE){
    return("invalid arguments")
  }
  #checks that the input values are scalars
  if (length(n)!=1|length(mean)!=1|length(sd)!=1) {
    return("invalid arguments")
  }
  #checks n is >= 1 and a whole number
  if (n<1|n%%1!=0){
    return("invalid arguments")
  }
  #checks "method" is correct input
  if (method!=1&method!=2&method!=3){
    return("invalid arguments")
  }
    
  m<-ceiling(n/2) #if a method returns 2 values, this will allow the fuction to run half the number of times
  x<-c() #assigning x as a blank vector
  
  ### Method 1 ###
  
  if (method==1){
    
    #runs the Marsaglia and Bray algorithm m times to produce m random variates
    for (i in 1:m) { 
      
      #generates uniform deviates distributed over [-1,1]
      u1 <- 2*runif(1,0,1)-1
      u2 <- 2*runif(1,0,1)-1
      w <- (u1^2) + (u2^2) #used in rejection step
      
      #rejection step, rejects values outside the required range (w = (u1^2) + (u2^2) < 1)
      while (w > 1) {
        u1 <- 2*runif(1,0,1)-1
        u2 <- 2*runif(1,0,1)-1
        w <- (u1^2) + (u2^2)
      }
      
      #part of the Marsaglia and Bray algorithm
      v<-sqrt(-2*log(w)/w)
      
      #defines normally distributed variates
      x1<-u1*v*sd+mean
      x2<-u2*v*sd+mean
      
      #gives answer in a vector
      x<-c(x,x1,x2)
    }
    
    #removes a value from the answer if n is odd as the method only generates even numbers of variates.
    if (n%%2!=0) {
      x<-x[1:n]
    }
  }
  
  ### Method 2 ###
  
  if(method==2){
    for (i in 1:m) {
      
      #generates uniform deviates distributed over [0,1]
      u1 <- runif(1,0,1)
      u2 <- runif(1,0,1)
      
      #the Box-Mueller algorithm gives deviates directly
      x1<-sin(2*pi*u1)*sqrt(-2*log(u2))*sd+mean
      x2<-cos(2*pi*u1)*sqrt(-2*log(u2))*sd+mean
      
      #gives answer in a vector
      x<-c(x,x1,x2)
    }
    
    #removes a value from the answer if n is odd as the method only generates even numbers of variates.
    if (n%%2!=0) {
      x<-x[1:n]
    }
  }
  
  ### Method 3 ###
  
  if (method==3){
    
    #loops to produce n variates
    for (i in 1:n) {
      
      u<-c()
      #the central limit theorum
      for(i in 1:16){
        u<-c(u,runif(1,0,1))
      }
      xi<-(sum(u)-8)*sqrt(12/16)*sd+mean
     
      #gives answer in a vector 
      x<-c(x,xi)
    }
  }

  #returns x, which is a vector of "n" pseudo-random values from a normal distribution
  return(x)
}



### Test section 1, wrong input tests




#tests if inputing strings into general.rnorm causes it to fail
string_input.test<-function(){
  si1 <- general.rnorm("feel free to change this for any string")#tests just n as a string
  si2 <- general.rnorm(n="hello",mean="hi",sd="boat",method="run")#tests all variables as strings
  si3 <- general.rnorm(n=2,sd="car")#tests correct n, with other variable as a string
  if (si1=="invalid arguments"&si2=="invalid arguments"&si3=="invalid arguments"){#checks each test returns "invalid argument"
    return("Passed, deals with string input")#if passes above line, returns message
  }
  else
    return("Failed, doesn't deal with string inputs")#if fails, returns other message
}
 
#tests if inputing vectors into general.rnorm causes it to fail
vector_input.test<-function(){
  vi1 <- general.rnorm(c(1,2,3,4,5))#tests just n as a vector
  vi2 <- general.rnorm(n=c(1,2),mean=c(3,4),sd=c(5,6)) #tests all variables as vectors
  vi3 <- general.rnorm(n=2,mean=c(1,2)) #tests correct n, with mean a vector
  if (vi1=="invalid arguments"&vi2=="invalid arguments"&vi3=="invalid arguments"){#checks each test returns "invalid argument"
    return("Passed, deals with vector input")#if passes above line, returns message
  }
  else 
    return("Failed, doesn't deal with vector inputs")#if fails, returns other message
}

#tests if inputing a negitive value for n causes it to fail
n_values_input.test<-function(){
  ni1 <- general.rnorm(n=-1)#tests a negitive n input
  ni2 <- general.rnorm(n=5.7)#tests a non integer n input
  if (ni1=="invalid arguments"&ni2=="invalid arguments"){#checks each test returns "invalid argument"
    return("Passed, deals with negitive n and non integer n input")#if passes above line, returns message
  }
  else 
    return("Failed, doesn't deal with negitive n or non integer n inputs")#if fails, returns other message
}

#tests if inputing an incorrect method value causes it to fail
method_input.test<-function(){
  mi1 <- general.rnorm(n=1,method=7)
  if (mi1=="invalid arguments"){#checks each test returns "invalid argument"
    return("Passed, deals with incorrect method input")#if passes above line, returns message
  }
  else
    return("Failed, doesn't deal with incorrect method inputs")#if fails, returns other message
}



### Test section 2, correct inputs, expected values



#used to test if each method gives expected results
method.test<-function(){
  
  mj1 <- general.rnorm(n=1000,mean=1,sd=5,method=1)#generates 1000 values of mean 1 and sd 5 using method 1
  mj2 <- general.rnorm(n=1000,mean=1,sd=5,method=2)#generates 1000 values of mean 1 and sd 5 using method 2
  mj3 <- general.rnorm(n=1000,mean=1,sd=5,method=3)#generates 1000 values of mean 1 and sd 5 using method 3
  
  #below calculates mean of generates values, returns message if mean is too extreme for each method
  if ((sum(mj1)/1000)>2|(sum(mj1)/1000)<0){
    return("Method one very probably gives incorrect values")
  }
  if ((sum(mj2)/1000)>2|(sum(mj2)/1000)<0){
    return("Method two very probably gives incorrect values")
  }
  if ((sum(mj3)/1000)>2|(sum(mj3)/1000)<0){
    return("Method three very probably gives incorrect values")
  }
  
  #below calculates if sd makes a large difference from sd = 1
  #I dont know a better way to test sd
  if (((prod(mj1))^2)<2){
    return("Method one very probably doesn't account for standard deviation")
  }
  if (((prod(mj2))^2)<2){
    return("Method one very probably doesn't account for standard deviation")
  }
  if (((prod(mj3))^2)<2){
    return("Method one very probably doesn't account for standard deviation")
  }
  
  #below checks each method produces the right number of variates
  if (length(mj1)!=1000){
    return("Method one produces wrong number of variates")
  }
  if (length(mj2)!=1000){
    return("Method one produces wrong number of variates")
  }
  if (length(mj3)!=1000){
    return("Method one produces wrong number of variates")
  }
   
  #if no above error messages have been returned, then the test pass message is returned 
  else{
    return("All tests passed, produces the correct number of variates, mean and sd have the correct effect")
  }
}

#A test to compare the results of general.rnorm to rnorm visually
visual.test<-function(){
  par(mfrow=c(2,2)) #allows 4 plots to be shown at once
  hist(rnorm(3000)) #plots histogram of 3000 variates using rnorm
  hist(general.rnorm(n=3000,method=1)) #plots histogram of 3000 variates using method 1
  hist(general.rnorm(n=3000,method=2)) #plots histogram of 3000 variates using method 2
  hist(general.rnorm(n=3000,method=3)) #plots histogram of 3000 variates using method 3
}