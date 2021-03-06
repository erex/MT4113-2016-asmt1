  #I confirm that the attached is my own work, except where clearly indicated in the text.
y.rnorm<-function(n,mean=0,sd=1){
  #   to generate a vector containing pseudo-random values from a standard normal distribution using the
  #   Marsaglia and Bray algorithm
  #   Inputs are: 
  #              n - numbers of values, 
  #              mean (default=0),
  #              sd - standard deviation (default=1)
  if(n<0) stop ('invalid arguments')
  if(sd<=0) stop ('invalid arguments')  #   error traps
  U2<-U1<-rep(NA,n)
  v<-rep(NA,n)
  for (i in 1:n){
    repeat{
      u1<-runif(1)
      u2<-runif(1)           #   genrate two values which are uniformly distributed in[0,1]
      u1<-2*u1-1
      u2<-2*u2-1             #   genrate two values which are uniformly distributed in[-1,1]   
      w<-(u1)^2+(u2)^2       #   check if the points(u1,u2) is in the unit circle  
      if (w<=1){
        U1[i]<-u1
        U2[i]<-u2
        denumerator<-w
        numerator<--2*log(w)
        v[i]<-sqrt(numerator/denumerator)
        #   caculate v related to u1 and u2
        break
      }
    }
  }
  
  X1=U1*v*sd+mean
  X2=U2*v*sd+mean    #   generate two vectors of pseudo-random values
  return (X1)        #   return X1 or it can be replaced by X2
}



general.rnorm <-function(n,mean=0,sd=1,method=1){
  #   to generate a vector containing pseudo-random values from a standard normal distribution
  #   Inputs are 
  #              n - numbers of values, 
  #              mean (default=0),
  #              sd - standard deviation (default=1), 
  #              method - 1 corresponding to Marsaglia and Bray algorithm
  #                       2 corresponding to Box and Mueller algorithm
  #                       3 corresponding to central-limit theorem algorithm
  #                         (default value for method is Marsaglia and Bray algorithm)
  if (method==1) {
    X<-my.rnorm(n,mean,sd)
    return(X)
  }
  
  
  
  
  if (method==2){
    if(n<0) stop ('invalid arguments')
    if(sd<=0) stop ('invalid arguments')    #   error traps
    U1<-runif(n,min=0,max=1)
    U2<-runif(n,min=0,max=1)                #   genrate two uniform distributions U1 and U2
    X1<-sin(2*pi*U1)*sqrt(-2*log(U2))*sd+mean
    X2<-cos(2*pi*U1)*sqrt(-2*log(U2))*sd+mean    #   generate two vectors of pseudo-random values
    return(X1)                                   #   return X1 or it can be replaced by X2
  }
  
  
  if (method==3){
    if(n<0) stop ('invalid arguments')
    if(sd<=0) stop ('invalid arguments')      #   error traps
    X<-rep(NA,n)
    for (j in 1:n){
      U<-runif(16,min=0,max=1)
      #   genrate 16 random uniformly distributed variables
      sum<-0
      for (i in 1:16){
        sum<-sum+U[i]
        #   sum them up
      }
      X[j]<-(sum-8)*sqrt(12/16)
    }
    X<-X*sd+mean          #   generate two vectors of pseudo-random values
    return(X)             #   return X
}
  
  
}

value.summary<-function(X){
  hist(X, probability=TRUE)
  lines(density(X))
  #   plot histogram  for data X
  plot(ecdf(X))
  #   plot cumulative distribution function
  qqnorm(X)
  qqline(X)
  #   qq plot for data X
  print(summary(X))

 }

test<-function(n) {
 
  y1<-my.rnorm(n,50,10)
  is.numeric(y1)
  y2<-general.rnorm(n,50,10,2)
  is.numeric(y2)
  y3<-general.rnorm(n,50,10,3)
  is.numeric(y3)
  print(length(y1))
  print(length(y2))
  print(length(y3))
  #   test whether vectors of pseudo-random values generated by three methods are consistent with the required size
  #   the lengths of the data generated are in the order (Marsaglia and Bray's method, Box-Mueller algorithm, Central limit theorem)
  
  par(mfrow=c(2,3))
  print("compare pseudo-random values generated by Marsaglia and Bray's method algorithm with rnorm")
  value.summary(my.rnorm(500,50,4))
  value.summary(rnorm(500,50,4))
  
  print("compare pseudo-random values generated by Box-Mueller algorithm with rnorm")
  value.summary(general.rnorm(500,50,4,2))
  value.summary(rnorm(500,50,4))
  
  print("compare pseudo-random values generated by Central limit theorem with rnorm")
  value.summary(general.rnorm(500,50,4,3))
  value.summary(rnorm(500,50,4))

  print("we set significance level alpha as 0.1")
  print(shapiro.test(general.rnorm(50,50,4,1)))
  if (shapiro.test(general.rnorm(50,50,4,1))[2]>0.1){
    print("we can not reject the hypothesis that the data are normal distributed at alpha=0.1")
  }
  else {
    print("we reject the hypothesis that the data are not normal distributed at alpha=0.1")
  }
  print(shapiro.test(general.rnorm(50,50,4,2)))
  
  if (shapiro.test(general.rnorm(50,50,4,1))[2]>0.1){
    print("we can not reject the hypothesis that the data are normal distributed at alpha=0.1")
  }
  else {
    print("we reject the hypothesis that the data are not normal distributed at alpha=0.1")
  }
  print(shapiro.test(general.rnorm(50,50,4,3)))
  
  if (shapiro.test(general.rnorm(50,50,4,1))[2]>0.1){
    print("we can not reject the hypothesis that the data are normal distributed at alpha=0.1")
  }
  else {
    print("we reject the hypothesis that the data are not normal distributed at alpha=0.1")
  }
  #   Since the distribution of small size data can be tested using shapiro test, Let n=50.
  #   then see if the p-value is greater significance level alpha (let alpha=0.1). 
  #   If so, we can not reject the hypothesis that data~N

}

  