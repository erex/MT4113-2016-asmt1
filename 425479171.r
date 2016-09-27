#ID:140004215 
# I confirm that the attached is my own work, except where clearly indicated in the text



#Defining function my.rnorm with arguments n,mean and sd. The last two have default values 0 and 1, respectively.
my.rnorm<-function(n,mean=0,sd=1) {

#Checking if n, mean and sd are scalar numbers.
  if ( !(is.numeric(c(n,mean,sd))) | length(n)>1 | length(mean)>1 | length(sd)>1) {
    stop("invalid arguments")
  } 
#Using a different if statement to check if n is a positive integer and if sd is nonnegative, because we need to know that arguments are scalar numbers to perform these checks.
    if (n<=0 | n!=floor(n) | sd<0){
      stop("invalid arguments")
    }

#random deviates are produced in pairs, so need to check if n is odd and delete one deviate later if this is the case.
  odd<-FALSE
  if (n%%2==1){
    n<-n+1
    odd<-TRUE
  }
  
#initialising the values in the vector x, where the deviates will be stored.
  x<-c(rep(0,n))

#deviates are produced in pairs so half the repetitions are needed.  
  N<-n/2
  for (i in (1:N)) {
    #Marsaglia and Bray's algorithm:
    u<-runif(2,0,1)
    u<-2*u-1
    w<-u[1]^2+u[2]^2
    while(w>1) {
      u<-runif(2,0,1)
      u<-2*u-1
      w<-u[1]^2+u[2]^2
    }
    v<-sqrt(-2*log(w)/w)
    u<-u*v
#each time we produce a pair of deviates we store it in a new place in x
    x[2*i-1]<-u[1]
    x[2*i]<-u[2]
  }
  
#deleting the last deviate if n is odd, since deviates are produced in pairs
  if (odd){
    x<-x[-n]
  }
  
#transforming into values of mean:mean and standard deviation:sd, before returning them
  x<-x*sd+mean
  return(x)
}






#Defining function general.rnorm with arguments method,n,mean and sd. Method,mean and sd have default values 1,0 and 1, respectively.
general.rnorm<-function(method=1,n,mean=0,sd=1){

#Checking if n, mean, sd are scalar numbers and if method has an integer value between 1 and 3, inclusive.
  if ( !(is.numeric(c(n,mean,sd))) | length(n)>1 | length(mean)>1 | length(sd)>1 | (method!=1 & method!=2 & method!=3) ) {
    stop("invalid arguments")
  } 
#Checking if n ia a positive integer and if sd is nonnegative. Need a different if statement for the two checks as above.
  if (n!=floor(n) | n<=0 | sd<0){
    stop("invalid arguments")
  }
    
  if (method==1){
    return(my.rnorm(n,mean,sd))
  } else if (method==2){

#random deviates are produced in pairs so need to check if n is odd and delete one deviate later.
      odd<-FALSE
      if (n%%2==1){
        n<-n+1
        odd<-TRUE
      }
#initialising the values in the vector x where the deviates will be stored
      x<-c(rep(0,n))
#deviates are produced in pairs so half the repetitions are needed
      N<-n/2
      for (i in (1:N)) {
        #Box-Mueller algorithm:
        u<-runif(2,0,1)
        arg<-2*pi*u[1]
        u<-c(sin(arg),cos(arg))*sqrt(-2*log(u[2]))
#each time we produce a pair of deviates we store it in a new place in x
        x[2*i-1]<-u[1]
        x[2*i]<-u[2]  
      }

#deleting the last deviate if n is odd, since deviates are produced in pairs
      if (odd){
        x<-x[-n] 
      }
#transforming into values of mean:mean and standard deviation:sd, before returning them
      x<-x*sd+mean
      return(x)
  } else {

#initialising the values in the vector x where the deviates will be stored
      x<-c(rep(0,n))
     
      for (i in (1:n)){
        #Central Limit Theorem Algorithm:
        u<-runif(16,0,1)
#we store each deviate in a new place in x
        x[i]<-(sum(u)-8)*sqrt(12/16)
      }
      #If we increase the number of uniformly distributed deviates we get a better approximation to normally distributed data. 
      #The same probably will happen if instead of uniformly distributed deviates we choose deviates from the t distribution

#transforming into values of mean:mean and standard deviation:sd, before returning them
      x<-x*sd+mean
      return(x)
  } 
  
}




#tests

m<-1000
std<-50
n<-4321

#0. Testing if the two functions produce the expected results in terms of size and type
test.type.size<-function(x,n){
  if (is.numeric(x) & length(x)==n){
    print('type and size is correct')
  } else{
    print('wrong type or size')
  }
}
#for (i in (1:3)){
#  test.type.size(general.rnorm(method=i,n=n,mean=m,sd=std),n)
#}


deviates<-matrix(general.rnorm(1,n=n,mean=m,sd=std),c(general.rnorm(2,n=n,mean=m,sd=std),general.rnorm(3,n=n,mean=m,sd=std)),nrow = n,ncol = 3)


#1. testing if the mean and variance of the data have the requested value
test.mean.var<-function(x,M,sd1){
  n<-length(x[,i])
  sigma<-c(0,0,0)
  m<-c(0,0,0)
  for (i in (1:3)){
#finding the means and standard deviations of the deviates produced with the 3 methods
    sigma[i]<-sd(x[,i])
    m[i]<-mean(x[,i])
#Checking if the mean for each method lies inside the 95% Confidence Interval for unknown mean and standard deviation
    if (abs(m[i]-M)>abs(qt(0.975,df=(n-1))*sigma[i]/sqrt(n))){
      return(list('mean is not accurate',m[i]))
    }
#calculating 95% CI for standard deviations
    ci<-c(1/qchisq(0.975,df=(n-1)),1/qchisq(0.025,df=(n-1)))
    ci<-sqrt(ci*(n-1)*sigma[i]^2)
#Checking if requested standard deviation lies in the CI
    if (sd1>max(ci) | sd1<min(ci)){
      return(list('Standard Deviation is not accurate',sd1,ci))      
    }
  }
  return('methods produce deviates with correct mean and standard deviation')
}
#test.mean.var(x=deviates,M=m,sd1=std)





#2. Testing graphically if the data has the proper shape, i.e. bell shape centred at mean, with vast majority of data in the range mean +- 3sd
test.graph<-function(x){
  par(mfrow=c(3,1))
  for (i in (1:3)){
    hist(x[,i])
  }
}
#test.graph(deviates)




#3.As seen in MT2508, by looking at the QQ plots we can assess the normality of our data.
test.qq<-function(x){
  par(mfrow=c(3,1))
  for (i in (1:3)){
    qqnorm(x[,i])
    qqline(x[,i])
  }
}
#test.qq(deviates)




#Following 2 functions were found in this link: As seen in this link: http://stats.stackexchange.com/questions/3136/how-to-perform-a-test-using-r-to-see-if-data-follows-normal-distribution 
kurtosis.test <- function (x) {
  m4 <- sum((x-mean(x))^4)/length(x)
  s4 <- var(x)^2
  kurt <- (m4/s4) - 3
  sek <- sqrt(24/length(x))
  totest <- kurt/sek
  pvalue <- pt(totest,(length(x)-1))
  pvalue 
}

skew.test <- function (x) {
  m3 <- sum((x-mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  skew <- m3/s3
  ses <- sqrt(6/length(x))
  totest <- skew/ses
  pt(totest,(length(x)-1))
  pval <- pt(totest,(length(x)-1))
  pval
}

#4. Test for Kurtosis and Skewness
test.kurt.skew<-function(x){
  pval<-matrix(rep(0.99,6),nrow = 3,ncol = 2)
  for (i in (1:3)){
    pval[i,]<-c(kurtosis.test(x[,i]),skew.test(x[,i]))
  }
  pval<-2*pval
  for (i in (1:3)){
    for (j in (1:2)){
      if (pval[i,j]>=1){
        pval[i,j]<-2*(1-pval[i,j]/2)
      }
    }
  }
  if (min(pval)<0.05){
    return(min(pval))
  } else{
    return('data have the proper skew and kurtosis as expected from normally distributed data')
  }
}

#test.kurt.skew(deviates)




#5.Shapiro-Wilk test of normality can be used to assess the hypothesis that our data are normally distributed
test.shap<-function(x){
  pval<-c(0,0,0)
  for (i in (1:3)){
    pval[i]<-shapiro.test(x[,i])$p
  }
  if (min(pval)<0.05){
    return('data are not normally distributed')
  } else{
    return('we cannot reject the assumption that data are normally distributed')
  }
}
#test.shap(deviates)


#6. Checking how my functions react to invalid arguments

#invalid method
#general.rnorm(method=4,n=n)
#general.rnorm(method=-1,n=n)
#general.rnorm(method='a',n=n)
#general.rnorm(method=2.5,n=n)

##invalid number of elements
#general.rnorm(method=2,n=2.5)
#general.rnorm(method=2,n=-1)
#general.rnorm(method=2,n=0)
#general.rnorm(method=2,n='a')
#general.rnorm(method=2,n=c(0,23))
#my.rnorm(n=0)
#my.rnorm(n='a')
#my.rnorm(n=2.5)
#my.rnorm(n=-1)
#my.rnorm(n=c(0,4))

##Invalid mean
#general.rnorm(method=2,n=n,mean='a')
#general.rnorm(method=2,n=n,mean=c(0,2))
#my.rnorm(n=n,mean='a')
#my.rnorm(n=n,mean=c(45,23))

##invalid Standard Deviation
#general.rnorm(method=3,n=n,sd=-2)
#general.rnorm(method=2,n=n,sd=c(0,3))
#general.rnorm(method=2,n=n,sd='a')
#my.rnorm(n=n,sd='a')
#my.rnorm(n=n,sd=-5)
#my.rnorm(n=n,sd=c(9,8,7,6))



##NOTE: If this wasn't an assignment, I would have created different functions for methods 2 and 3 but I preferred to keep it like this because of the automated checks that will occur

