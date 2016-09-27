#I confirm that the attached is my own work, except where clearly indicated in
#the text.
#Assignment 1, Question 1: Marsaglia and Bray algorithm

#The functions should be designed only to accept scalar integers. 
#Will therfore include error checks. 
errorchecks <- function(n, mu, sigma){
  #Check if input value is a vector
  if(length(n) > 1)stop("invalid arguments")
  if(length(mu) > 1) stop("invalid arguments")
  if(length(sigma) > 1)stop("invalid arguments")
  
  #Check if null
  if(is.null(n)) stop("invalid arguments")
  if(is.null(mu)) stop("invalid arguments")
  if(is.null(sigma)) stop("invalid arguments")
  
  return(NULL)
}

my.rnorm <- function(n, mu=0, sigma=1){
  #Implement the error checks to ensure scalar input
  errorchecks(n, mu, sigma)
  #This function generates pairs. Therefore, use an if...else statement to 
  #make n even if an odd n value is inputted by the user. 
  #"http://www.programiz.com/r-programming/examples/odd-even" used as a reference.
  if ((n %% 2) == 0) {
    #print("n input value is even")
  } else { 
    n = n+1
    #print("n input value is odd")
  }
  
  #Store the x values in a vector
  x <- c(1,2)  
  
  #This function generates pairs of deviates, and so a for loop is used to run 
  #until n/2. 
  
  for (i in 1:(n/2)){ 
    #Give a starting value for w that is greater than 1. This induces the below 
    #while loop to run. 
    w <- 3
    while(w>1){ 
      #Instead of creating u1 and u2, can create a vector with n=2
      u<-runif(2,0,1)
      #Transform the generated deviates so that they are distributed over the
      #unit square
      u<-2*u-1
      #Define w, thereby only using a pair of random deviates from within the unit
      #circle
      w<-(u[1])^2+(u[2])^2
    }
    #Define the variable v
    v<-(-2*log(w)/w)^(1/2)
    
    #Define x1 and x2, the pair of normally distributed variates 
    x[i] <- u[1]*v
    x[i+(n/2)] <- u[2]*v
    
  }
  x <- x*sigma+mu
  return(x)
}

#Question 2
#Box-Mueller algorithm

boxmueller.rnorm <- function(n, mu=0, sigma=1){
  #As with the Marsaglia and Bray method above, the Box Mueller method generates
  #pairs and so only needs to run for n/2.
  for (i in 1:(n/2)){
  
    #Generate a pair of uniformly distributed random deviates
    u <- runif(2*n,mu,sigma)
    
    #COnvert the pair to polar coordinates
    x[i] <- ((((-2)*log(u[1]))^(1/2))*cos(2*u[2]*pi))
    x[i +(n/2)] <- ((((-2)*log(u[1]))^(1/2))*sin(2*u[2]*pi))
  }
  
  return(x)

}

#Central Limit Theorem algorithm
CLT <- function(n){
  #Create a matrix to store the generated deviates
  CLTmatrix <- matrix(runif(n), ncol = n, nrow = n)
  #Apply numerical algorithm 
  x <- ((rowSums(CLTmatrix) - 8)*sqrt(12/16))
  return(x)
  
}  

#Combine the above three methods into one function
#Change algorithm choice input from a number to a string for improved user 
#interaction
#Choice of 3 methods
algorithm <- c("1", "2", "3")
algorithm <- toString(algorithm) 
#Create general.rnorm function which allows the user to pick which algorithm 
#should be used 
general.rnorm <- function(n,mu=0,sigma=1,algorithm=1){
  #Include error checks for input values
  errorchecks(n, mu, sigma)
  #Use the switch function to provide the method opionns. Default is Marsaglia 
  #and Bray (option 1)
  switch(algorithm,
       '1' <- (x=my.rnorm(n,mu,sigma)),
       '2' <- (x=boxmueller.rnorm(n,mu,sigma)),
       '3' <- (x=CLT(n)))
  x <- (x*sigma) + mu 
  
  return(x)
  return(general.rnorm)
  
}
