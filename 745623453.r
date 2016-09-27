#MT4113 - COMPUTING IN STATISTICS 
#ASSIGNMENT 1
#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n,mean=0,sd=1){
  #Purpose 
  # returning a vector of pseudo-random normally-distributed deviates using Marsaglia and Bray algorithm.
  #
  # Input:
  #     n - number of value to return
  #     means - defult = 0
  #     standard deviation - defult = 1 
  #
  #  Output:
  #     n number of random variables that are from a Standard Normal Distribution (N(0,1))
  #
  x=c()  
  if ((is.numeric(n)==FALSE) | (is.numeric(mean)==FALSE) | (is.numeric(sd)==FALSE)){
    stop('invalid arguments')    
  }
  else if( sd < 0 | length(n) !=1 | length(mean) !=1 |length(sd) !=1 | n%%1!=0){ #if sd is -ve OR any of the input is a vector&
    stop('invalid arguments')                                     # then exit the loop and give a warning message
  }
  else if (n==0){         #else if ve want 0 output 
    return(0)
  }
  else{       
   for (i in 1:ceiling(n/2)){    #for i form 1 to n/2( round up) pairs 
     w <- 1.1                    #setting w to above 1 so we can run the while loop
     while (w > 1.0) {           
        u <- 2*runif(2, min=0, max=1)-1    #transform the pair of unform distributions to unit square
        w <- sum(u^2)                      #calculate the rejection value
      } 
     v <- sqrt((-2*log(w))/w)             
     x <- append(x,c(u[1]*v,u[2]*v))       #add the calculate pair of uniform distribution to vector x
    }
   if (n%%2!=0){          #if the input of n is odd then we remove first element form the vector x
     x <- x[-1]
  }
   return(x)       #return the value of x
 }
}





general.rnorm <- function(n, mean=0, sd=1, method=1){
  #Purpose 
  # returning a vector of pseudo-random normally-distributed deviates using 3 different methods
  #
  # Input:
  #     n - number of value to return
  #     means - defult = 0
  #     standard deviation - defult = 1 
  #     method- 1 (defult) corresponding to Marsaglia and Bray algorithm, 2 corresponding to Box and Mueller algorithm &
  #             3 corresponding to central-limit theorem algorithm
  #
  #  Output:
  #     n number of random variables that are from a Standard Normal Distribution (N(0,1) default)
  x=c()
  if ((is.numeric(n)==FALSE) | (is.numeric(mean)==FALSE) | (is.numeric(sd)==FALSE) | (is.numeric(method)==FALSE)){
    stop('invalid arguments')    
  }
  else if( sd < 0 | length(n) !=1 | length(mean) !=1 |length(sd) !=1){ #if sd is -ve OR any of the input is a vector&
    stop('invalid arguments')                                     # then exit the loop and give a warning message
  }
  else if((method>3 | method <1) | method%%1 != 0){     #methods can only take integer [1,2,3] and it has to be integer
    stop("Invalid arguments")
  }  
  else if(n==0){
    return(0)
  }
  else if (method == 1){          
    x<-my.rnorm(n,mean,sd)          #calling the function that use Marsaglia and Bray algorithm from before
  }
  else if (method == 2){            #carrying out the calculation for Box and Mueller algorithm
    for (i in 1:ceiling(n/2)){           
      u <- runif(2, min=0,max=1)
      x1 <- sin(2*pi*u[1])*sqrt(-2*log(u[2]))
      x2 <- cos(2*pi*u[1])*sqrt(-2*log(u[2]))
      x <- append(x,c(x1,x2))
    } 
    if (n%%2!=0){                   #same as before if we have a odd n we remove a element for the vector x
      x <- x[-1]
    }
  }
  else if (method == 3){     
    for (i in 1:n){                #Carrying out the calculation for central limit theorem
      u <- runif(16, min=0,max=1)
      xi <- (sum(u)-8)*sqrt(12/16)
      x <- append(x,xi)
    } 
  }
  return(x)     #return the vector that contains the n number of random vaiables 
}

k <-general.rnorm(1000,method=1)






#Testing the function general.rnorm and my.rnorm

source('~/Documents/MT4113-COMPUTING IN STATS/ASSIGNMENT/asmt1.r')

test.output <- function(n, mean=0,sd=1, method=1){
   #testing if our output is numeric and gives n number of random variables 
   x <- my.rnorm(n)
   y <- general.rnorm(n)
   pass.test <- (length(x)==n & is.numeric(x))
   pass.test1 <- (length(y)==n & is.numeric(y))   
   return(list(pass.test,pass.test1))
}

test.output(2000)

 norm.test <- function(n, mean=0,sd=1, method=1) {
  #Purpose: to test if our funcions produce normally distributed random variable using Shapiro-Wilk test
  #and display then visually using histogram and qqplots
  answer1 <-my.rnorm(n,mean,sd)
  answer2 <-general.rnorm(n,mean,sd,method)
  par(mfrow = c(2, 2))
  hist(answer1, main = "Histogram of using my.norm")
  hist(answer2, main = "Histogram of using general.rnorm")
  qqnorm(answer1)
  qqline(answer1)
  qqnorm(answer2)
  qqline(answer2)
  k1 <- shapiro.test(answer1)   #sw test: H0: the variables are normally distributed vs
  k2 <- shapiro.test(answer2)   # H1: the variables are not normally distributed
  return(list(k1,k2))           #if the returned p-value is higher than 5% we do not reject H0
}
norm.test(200)

#Refernce 
#http://webspace.ship.edu/pgmarr/Geo441/Lectures/Lec%205%20-%20Normality%20Testing.pdf
