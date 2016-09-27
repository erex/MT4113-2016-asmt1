# I can confirm that the attached is my own work, except where clearly indicated in the text.


# Implementation of the Marsaglia and Bray's method for generating n normal random deviates
# with mean mu and and standard deviation sigma.

my.rnorm<-function(n,mu=0,sigma=1){
  if(n<=0 || n%%1!=0 || sigma<0 || is.numeric(mu)==FALSE){
    stop("Invalid arguments")
    
  }
  rd<-c() # initialise vector to hold the random deviates.
  for(i in 1:ceiling((n/2))){
    u<-2*runif(2,0,1)-1 # Generate two uniform deviates and transform to the unit sqaure.
    while(sum(u^2)>1){  # While loop carries out the rejection step.
      u<-2*runif(2,0,1)-1
      
    }
    # Carry out the transformation of the deviates and populate the vector rd.
    w<-sum(u^2)         
    v<-(-2*log(w)/w)^(1/2)
    rd<-c(rd,u*v)
    
  }
  # if n was odd, then the for loop has generated one extra deviate. If so, remove one deviate
  # from rd.
  if(length(rd)>n){   
    rd<-rd[-1]        
  }
  
  return(rd*sigma+mu) #Transformation applied to the random deviates so they have the specified mean and standard deviation.
}

# implementation of the Box-Mueller algorithm for generating n random normal deviates with
# mean mu and standard deviation sigma.

BM<-function(n,mu=0,sigma=1){
  if(n<=0 || n%%1!=0 || sigma<0 || is.numeric(mu)==FALSE){
    stop("Invalid arguments")
  }
  rd<-c() #initialise vector to hold the random deviates.
  for(i in 1:ceiling((n/2))){
    u<-runif(2,0,1)
    rd<-c(rd,sin(2*pi*u[1])*sqrt(-2*log(u[2])),cos(2*pi*u[1])*sqrt(-2*log(u[2]))) #Transform the deviates so they are normal and populate rd.
  }
  # if n was odd, the for loop has generated one extra deviate. If so, remove one deviate
  # from rd.
  if(length(rd)>n){
    rd<-rd[-1]
  }
  
  return(rd*sigma+mu) #Transformation applied so the random deviates have the specified mean and standard deviation.
}

# Implementation of a function using the central limit theorem to generate n random
# normal deviates with mean mu and standard deviation sigma.
# Sample size of uniform deviates is choosen to be 16.
CL<-function(n,mu=0,sigma=1){
  if(n<=0 || n%%1!=0 || sigma<0 || is.numeric(mu)==FALSE){
    stop("invalid arguments")
  }
  rd<-c() #Initialise vector to store random deviates.
  for(i in 1:n){
    u<-runif(16,0,1)
    x<-(sum(u)-8)*sqrt(3/4)
    rd<-c(rd,x)
  }
  # if n was odd, the for loop has generated one extra deviate. If so, remove one deviate
  # from rd.
  if(length(rd)>n){
    rd<-rd[-1]
  }
  
  return(rd*sigma+mu) # Transformation applied to the random deviates so they have the specified mean and standard deviation.
  
}

# Implementaion of a function to generate n normal random deviates with mean mu
# and standard deviation sigma. The Argument method determines which of the three algorithms is used:
# 1=Marsaglia and Bray's method
# 2=Box-Mueller method.
# 3=Central limit theorem method

general.rnorm<-function(n,mu=0,sigma=1,method=1){
  if(is.element(method, c(1,2,3))==FALSE || n<=0 || n%%1!=0 || sigma<0 || is.numeric(mu)==FALSE){
    stop("Invalid arguments")
  }
  
  switch(method,my.rnorm(n,mu,sigma),BM(n,mu,sigma),CL(n,mu,sigma))
  
}  




# Function tests if each function returns vectors with the correct length and data type.

test.function1<-function(){
  x1<-sapply(c(10,100,1000),function(x){my.rnorm(x,2,5)})
  x2<-sapply(c(10,100,1000),function(x){BM(x)})
  x3<-sapply(c(10,100,1000),function(x){CL(x,-2,3)})
  #run general.rnorm for algorithms 1, 2 and 3 with vector lengths 10, 100, and 1000 respectively. 
  x4<-mapply(function(x,y){general.rnorm(x,1,2,y)},c(10,100,1000),1:3)
  
  # Display the structure of each list. Then check if each element of the list
  # is a vector of the correct length, and is of data type numeric.
  str(x1)
  str(x2)
  str(x3)
  str(x4)
  
}

# Functions below test if input errors are detected.

test.function2<-function(){
  my.rnorm(-100,1,2)
  
}

test.function3<-function(){
  general.rnorm(100,1,2,5)
}

# Function generates data from each algorithm and plots histograms to assess
# normality of the data.
test.function4<-function(){
  par(mfrow=c(1,3))
  hist(my.rnorm(10000,3,2))
  hist(BM(10000,10,1))
  hist(CL(10000,-5,2))
  
  
}




