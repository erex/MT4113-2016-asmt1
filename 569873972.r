# Student ID: MH278
# Module ID: MT4113
# I confirm that the attached is my own work
### Assignment Objectives###
# This project is meant to be for simulating Normal Random variables 
# using 3 different methods, namely:
# A- Marsaglia and Brayâs (Polar) method, a function being 
# B- Box-Muller Algorithm,
# C- Central Limit Theorum
# The Project has 3 Building Blocks:
# 1- Creating 3 functions, each uses one the methods mentioned above.
#    the 3 functions are: 'my.rnorm', 'my.rnorm.BM', and 'my.rnorm.CLT'
# 2- Creating a function called 'general.rnorm'. This function will be used to call 
#    one the algorithms used in first buliding block. It is also implement constraints
#    on the function arguments such n,sd,method  
# 3- A function created called test, to test the results of the above function quantitively
#    i.e. the number of simulation produced and qualitatively by checking the mean and sd 
#    whether it adheres to normal distribution or not

### FIRST Building Block: Generating Random Normal realisations###

# A- Marsaglia and Brayâs method
# creating a function my.rnorm, the function has 3 parameters
my.rnorm <- function (n,mean=0,sd=1){ #here we can change the mean and sd we desire
  # This is an if statement to determine if the argument n is even or not. 
  # If it is odd, it will return n+1 simulations, 
  # later on the number of simulations requested will be trancated to the odd number desired.
  k<-n
  is.odd <- function(n) n %% 2 != 0
  if(is.odd(n)) {
    n <- n+1 
  }
  # This is function using while statement to skip the realisation when W>1  
  internal.method.1<-function(){
    PS <- runif(2,0,1)
    U  <- PS*2-1
    W  <-(U[1])^2+(U[2])^2
    # This is the condition for this method before simulation normal realisation
    while (W>1){      # while will skip it until it satisfies the condintion  
      PS <- runif(2,0,1)
      U  <- PS*2-1
      W  <-(U[1])^2+(U[2])^2
    }
    V  <- sqrt(-2*log(W)/W)
    Z  <- U*V
    X  <- Z*sd+mean   #transforming standard normal into N(mean=Mean,sd=standard deviation)
    return(X)
  }
  XV<- replicate(n/2,internal.method.1()) # This replicates a vector of pairs simulated from N(mean,sd) 
  d<-(as.vector(XV))  # This will produces the n number of realisation from N(mean,sd)
  length(d)<- k
  return(d)
}

# B- Box and Muller Algorithm
# creating a function for the method specific calculations, returns a pair of standard normal N(0,1)
internal.method.2 <- function (mean=0,sd=1){
  PS<- runif(2,0,1)     #produces two number of uniform(0,1), i.e.pseudo random number
  X1 <- sin(2*pi*PS[1])*sqrt(-2*log(PS[2]))
  X2 <- cos(2*pi*PS[1])*sqrt(-2*log(PS[2]))
  Z  <- rep(0,2)
  Z[1]<-X1
  Z[2]<-X2
  ;return(Z)}
# This function returns a vector of normal distribution realisations 
my.rnorm.BM <-function(n,mean=0,sd=1){ #here we can change the mean and sd to what we desire
  # This is an if statement to determine if the argument n is even or not. 
  # If it is odd, it will return n+1 simulations, 
  # later on the number of simulations requested will be trancated to the odd number desired.
  k<-n
  is.odd <- function(n) {n %% 2 != 0}
  if(is.odd(n)) {
    n<-n+1 
  }
  n  <-n/2
  z1 <-rep(0,n)   #create an array to feed normal variates of the first realisation of each pair 
  z2 <-rep(0,n)   #create an array to feed normal variates of the second realisation of each pair 
  for (j in 1:n){
    a    <-internal.method.2() #calling for the function to produce a new pair of N(0,1)
    z1[j]<- a[1]
    z2[j]<- a[2]
  }
  x1 <- z1*sd+mean # transforming standard normal into N(mean=Mean,sd=standard deviation)
  x2 <- z2*sd+mean # transforming standard normal into N(mean=Mean,sd=standard deviation)
  d<-(c(x1,x2))
  length(d)<-k
  return(d)
}

# C- Central-Limit Theorem Algorithm
# creating a function for the method specific calculations, returns a realisation of standard normal N(0,1)
internal.method.3 <- function (mean=0,sd=1){
  PS<- runif(16,0,1)          #produces 16 number of uniform(0,1), i.e.pseudo random number
  a<-c(PS)
  x<-(sum(a) - 8)*sqrt(12/16) # the specific calculation required for this method
  ;return(X)}

# This function returns a vector of normal distribution realisations 
my.rnorm.CLT <-function(n,mean=0,sd=1){ #here we can change the mean and sd we desire
  z<-rep(0,n) #create an array to feed normal variates inside
  for (j in 1:n){
    a   <- internal.method.2() #calling for the function to produce a new realisation of N(0,1)
    z[j]<- a[1]
    
  }
  x   <- z*sd+mean #transforming standard normal into N(mean=Mean,sd=standard deviation)
  return(x)
}  

### Second Building Block: generates normal deviates using a specific algorith###

# The function below returns a normal deviates in addition a property to stop execution 
# incase of violation of certain constraints on standard deviation (should be>0),
# number of simulations(should be positive integers only, zero is not included),
# and the method or algorith being called in the function (should be 1 for Polar Method,
#                                                                    2 for Box-Muller,
#                                                               and  3 for Central Limit Theorum)

general.rnorm<-function(n,mean,sd,method=1){
  # a condition to stop calculation if sd<0
  if(sd<0) {return("invalid argument") 
    break
  }else{
    #No simulation number is allowed <1  
    if(n<1){
      return("invalid arguments")
      break
    }else{
      #No of simulations requested should be in non-zero positive integers   
      if(n != round(n)){
        return("invalid argument")
      }
    }
    # calling a specific algorithm using the forth argument of the function as noted above the function
    if(method==1){
      my.rnorm(n,mean,sd)
    }else{
      if(method==2){
        my.rnorm.BM(n,mean,sd)
      }else{
        if(method==3){
          my.rnorm.CLT(n,mean,sd)
        }else{
          return("invalid arguments") # if the method called is different than the predefined, 
          # calculations will be stopped. 
        }
      }
    }
  }
}

### THIRD Building Block: Testing the above functions###

#To test the general.rnorm function, the number of scenarios are created to check its apprpraiteness
# There are 32 scenarios/possibilities where this function should stop and returns "invalid argument"
# mean<-rep(0,4) ==> no possibility
# n<-c(1,-1,3.5,-3.5) ==> 4 possibilities 
# sd<-c(1,-1) ==> 2 possibilites
# method<-c(1,2,3,4) ==> 4 possibilities
# I have chosen 4 scenarios due to time available
test.general.rnorm <- function(n,mean,sd,method){
  ns <- c(-1,3.5,4,4)   # negative number of simulations
  ms <- c(0,0,0,0)      # there is no constriants on the mean
  sds <- c(1,1,-1,1)    # negative standard deviation in third scenario
  methods <- c(1,2,3,4) # in the forth scenario, the method used is not defined
  x<-rep(NA,4)
  for(i in seq(1:4)){
    res<-general.rnorm(n=ns[i],mean=ms[i],sd=sds[i],method=methods[i])
    x[i]<-res
  }
  return(x)             #it should return "invalid argument" for each failed scenario
}


# to test the output quantatively for the generated normal deviates and check whether
# the number simulated deviates is equal to the number requested by the user.
test.quantity<-function(n){
  x<-my.rnorm(n)  
  my.norm.pass.test <-(length(x)==n & is.numeric(x))
  if((my.norm.pass.test)==TRUE)
  {print("Great: my.rnorm function simulates numberof deviates as requested!!!")
  }else{
    print("unmatched number of simulations")
  }
  y<-my.rnorm.BM(n)  
  my.norm.BM.pass.test <-(length(y)==n & is.numeric(y))
  if((my.norm.BM.pass.test)==TRUE)
  {print("Great: my.rnorm.BM function simulates numberof deviates as requested!!!")
  }else{
    print("unmatched number of simulations")
  }
  z<-my.rnorm.CLT(n)  
  my.norm.CLT.pass.test <-(length(z)==n & is.numeric(z))
  if((my.norm.CLT.pass.test)==TRUE)
  {print("Great: my.rnorm.CLT function simulates number of deviates as requested!!!")
  }else{
    print("unmatched number of simulations")
  }
}

# To test the output quality and its adherence to Normal distribution.
# This will be executed by comparing the basic statistics (mean,standard deviation, and skewness 
# of the simulated data with the R embebed function of simulating normal 'rnorm'  
test.quality <- function(n){
  x<-my.rnorm(n)  
  if(((abs(mean(rnorm(n,0,1))-mean(x))<0.05)==TRUE) #checking if both means are equal 
     &((abs(median(rnorm(n,0,1))-median(x))<0.05)==TRUE) #checking if both medians are equal
     &(((sd(rnorm(n,0,1))-sd(x))<0.05)==TRUE)#checking if both standard deviations are equal
     &((abs(median(x)-mean(x))<0.05)==TRUE))#checking if the deviate distribution is skewed
  {print("Great: with a small margin of 5%, the R simulated mean, median and sd are approximately
         equal with the function created i.e. my.rnorm in this project.
         In addition, the mean and median of the simulated deviates are approximately similar.
         This refelects the deviates distribution is not skewed and therefore is symmetric which 
         satisfies one of the properties of normal  distribution")
  }else{
    print("something wronge, please check your algorithm in (my.norm).")
  }
  y<-my.rnorm.BM(n)  
  if(((abs(mean(rnorm(n,0,1))-mean(y))<0.05)==TRUE) #checking if both means are equal 
     &((abs(median(rnorm(n,0,1))-median(y))<0.05)==TRUE) #checking if both medians are equal
     &(((sd(rnorm(n,0,1))-sd(y))<0.05)==TRUE)#checking if both standard deviations are equal
     &((abs(median(y)-mean(y))<0.05)==TRUE))#checking if the deviate distribution is skewed
  {print("Great: with a small margin of 5%, the R simulated mean, median and sd are approximately
         equal with the function created i.e. my.rnorm.BM in this project.
         In addition, the mean and median of the simulated deviates are approximately similar.
         This refelects the deviates distribution is not skewed and therefore is symmetric which 
         satisfies one of the properties of normal  distribution")
  }else{
    print("something wronge, please check your algorithm in (my.norm.BM).")
  }
  z<-my.rnorm.CLT(n)  
  if(((abs(mean(rnorm(n,0,1))-mean(z))<0.05)==TRUE) #checking if both means are equal 
     &((abs(median(rnorm(n,0,1))-median(z))<0.05)==TRUE) #checking if both medians are equal
     &(((sd(rnorm(n,0,1))-sd(z))<0.05)==TRUE)#checking if both standard deviations are equal
     &((abs(median(z)-mean(z))<0.05)==TRUE))#checking if the deviate distribution is skewed
  {print("Great: with a small margin of 5%, the R simulated mean, median and sd are approximately
         equal with the function created i.e. my.rnorm.CLT in this project.
         In addition, the mean and median of the simulated deviates are approximately similar.
         This refelects the deviates distribution is not skewed and therefore is symmetric which 
         satisfies one of the properties of normal  distribution")
  }else{
    print("something wronge, please check your algorithm in (my.norm.CLT).")
  }
}  

# this function returs the basic statistics of the 3 algorithms versus the R embeded function rnorm
test.by_eye<-function(n){
  x<-my.rnorm(n)
  summary(x)
  y<-my.rnorm.BM(n)
  z<-my.rnorm.CLT(n)
  print(summary(rnorm(n,0,1))) #rnorm function is ready-made R function for generating normal deviates 
  print(summary(x))
  print(summary(y)) 
  print(summary(z))
}

# This function will test the graphical representation of the 3 algorithms used versus the normal 
# deviates generated by the R function rnorm 
test.graphical<-function(n){
  Norm.R<-rnorm(n,0,1)
  x<-my.rnorm(n=10000)
  y<-my.rnorm.BM(n=10000)
  z<-my.rnorm.CLT(n=10000)
  par(mfrow=c(2,2))
  hist(Norm.R)
  hist(x)
  hist(y)
  hist(z)
}