#I confirm that the attached is my own work, except where clearly indicated in the text.
my.rnorm<-function(n,mean=0,sd=1){#my.rnorm with default mean and sd values
  if (n!=as.integer(n)){#if n is not equal to the integer part of n (n is not an integer)
    stop("invalid arguments")} #program execution stops
  if (length(c(n,mean,sd))!=3){ #if any (use of the OR operator |) of the inputs are not scalars (length 1)
    stop("invalid arguments") #program execution stops
  }
  if ((n<=0)|(sd<=0)){ #if either sd or n are negative
    stop("invalid arguments") #program execution stops
  }
  if (n%%2==0){ #This seperates cases where n is even from when n is odd
    m=n/2 #since we are outputting pairs, define a new variable for n/2 pairs
    X<-vector() #defining an empty vector X 
    for (i in 1:m){ #for loop, running m times so that we get m pairs
      w<-2 #here I have defined w as 2 so that it starts off more than 1, ready for the while loop
      while (w>1){ #this will run as long as w>1, and does not running as soon as w<=1 (pairs are inside unit circle)
        U<-runif(2) #two random numbers
        Un<-U*2-1
        w<-Un[1]**2+Un[2]**2 #redefining w to check the numbers are within the unit circle
      } 
      V<-sqrt(-2*log(w)/w)
      X<-append(X,(Un*V)) #this adds each suitable pair to the vector for X
    }
    X=X*sd+mean #transforming values to the suitable mean and sd
  }
  else{ #If the number of values is odd
    k=n+1 #new variable as n+1 ie. an even number
    X<-my.rnorm(k,mean,sd) #calling itself with a new number of values, will run the&
    #even part of the code
    X<-X[-k]  #disregarding the last value so that we have the required number of values
  }
  return(X)
}

general.rnorm<-function(n,mean=0,sd=1,method=1){#default values for mean,sd and method
  if (n!=as.integer(n)){
    stop("invalid arguments")}
  if (length(c(n,mean,sd))!=3){
    stop("invalid arguments")
  }
  if ((n<=0)|(sd<=0)){
    stop("invalid arguments")
  }
  if (method==1){ #will run the previous function for method=1 
    X<-my.rnorm(n,mean,sd)
  } else if (method==2){ 
    if (n%%2==0){#seperates code from even and odd 
      m<-n/2
      U<-matrix(runif(n),ncol=2) #here I am generating n random values and then seperating them
      #into pairs by inputting them into a two column matrix. 
      X <- vector()
      for (i in 1:m){ #running loop for m pairs
        X<-append(X,(sin(2*pi*U[i,1])*sqrt(-2*log(U[i,2]))))
        X<-append(X,(cos(2*pi*U[i,1])*sqrt(-2*log(U[i,2]))))
      }
      }else {#this part is the odd part, just like in my.rnorm
        m=n+1 
        X<-general.rnorm(m,mean,sd,method)
        X<-X[-m]
      }
    X<-X*sd+mean #adjusts X according to the mean and sd arguments
    }
  else if (method ==3){
    X<-vector(length=n) #for the third method, whether n is or even does not matter
    #since values are generated individually rather than in pairs
    for (i in 0:n){
      U<-runif(16) #generating 16 random numbers
      X[i]<-(sum(U)-8)*sqrt(12/16)
      X<-X*sd+mean
    }
  } else {
    stop("invalid arguments") #stops the code if method is not equal to 1,2,3
  }
  return(X)
}


#Test functions
#a test to check whether the function is returning the correct output
#a vector filled with numbers (numeric)
output.test<-function(n,mean=0,sd=1,method=1){
  output<-is.vector(general.rnorm(n,mean,sd,method),mode="numeric")
  print(paste('Function general.rnorm outputs numeric vector:',output))
  return(output)
}#returns a logical TRUE or FALSE

#testing the length is what we require it to be, returning logical output 
length.test<-function(n,mean=0,sd=1,method=1){
  print(paste('Length of vector is n:',length(general.rnorm(n,mean,sd,method))==n))
  return(length(general.rnorm(n,mean,sd,method))==n)
}
#This test is testing the mean of the random values
#tests to within a stated accuracy of mean
#default accuracy is 0.5*sd, within half a standard deviation. Accuracy can be changed,
#with the acc argument changing how many standard deviations
mean.test<-function(n,mean=0,sd=1,method=1,acc=0.5){
  X<-general.rnorm(n,mean,sd,method)
  sum<-sum(X)
  E<-sum/length(X)
  print(paste('Mean is',round(E,digits=2)))
  mean<-(E>mean-sd*acc) & (E<mean+sd*acc) #an AND operator is used to check both condtions
  #are satisfied
  print(paste('Mean is within the chosen accuracy of required mean:',mean ))
  return(mean) #returns logical statement
}

#this test is a visual test, without a logical output
#this will test whether 34.1% of the values lie within 1 standard deviation on either
#side of the mean. The number 34.1% comes from a normal distribution
sd.test<-function(n,mean=0,sd=1,method=1){
  lower<-mean-sd#defining lower and upper bounds for the percentile
  upper<-mean+sd
  middle<-0

  X<-general.rnorm(n,mean,sd,method)
  for (i in 1:n){#running through every value in X
    if ((X[i]>lower)&(X[i]<upper)){#if the value in X is between the upper AND lower bound
      middle<-middle+1 #add one to the variable middle
    }
  } 
  perc<-0.341*n #calculates 34.1% of n number of variables
  round(perc) #rounding to 2 dp
  return(c(middle,2*perc)) #returns number of values actually in the middle percentile,
  #next to how many there should be, for user to observe.
  #this test could be finalised if the coder had a specific degree of accuracy they were 
  #testing. #A logical statement much like the one use in mean.test would work. 
}

