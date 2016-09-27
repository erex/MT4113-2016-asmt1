source(astm1.r)
x<-my.rnorm(n=10)
pass.test<-(length(x)==10 & is.numeric(x))




#I confirm that the attached is my own work, except where clearly indicated in the text.
my.rnorm<-function(n,mean,sd){
  if(n<=0|!identical(round(n), n)){
    stop('Invalid inputs,The argument "n" must be positive integer')
  }
  if(missing(mean)){
    mean=0
  }#mean of values to return ¨C default 0#
  if(missing(sd)){
    sd=1
  }#standard deviation of values to return ¨C default 1#
  vector1<- rep(0,n)
  vector2<- rep(0,n)
  for(i in 1:n){
    U<-2*runif(2)-1
    W<- sum(U^2)
    V<-(-2*log(W)/W)^0.5
    X1<-V*U[1]
    X2<-V*U[2]
    while(W>=1){
      U<-2*runif(2)-1
      W<- sum(U^2)
      V<-(-2*log(W)/W)^0.5
      X1<-V*U[1]
      X2<-V*U[2]
    }#If w = U1 + U2 > 1, reject the pair and generate new pair U1, U2#
    vector1[i]<-X1[1]
    vector2[i]<-X2[1]
  }
  vector1
  vector2 
  X<-sd*vector1+mean
  return(X)
}#make the output a vector so that an odd number of deviates could be generated#






## choose integer "1" as rejection method,"2" as box-Mueller method and "3"  as the central-limit method for generating normally distributed random numbers. 
## I output all the values including X1 X2 in method 1 and , method 2
general.rnorm<-function(n,mean,sd,method){
  if(n<=0|!identical(round(n), n)){
    stop('Invalid inputs,The argument "n" must be positive integer')
  }
  if(missing(mean)){
    mean=0
  }
  if(missing(sd)){
    sd=1
  }
  if(missing(method)){
    method==1
  } #default value for method is Marsaglia and Bray algorithm (1)#
  if(method==1){vector1<- rep(0,n)
  vector2<- rep(0,n)
  for(i in 1:n){
    U<-2*runif(2)-1
    W<- sum(U^2)
    V<-(-2*log(W)/W)^0.5
    X1<-V*U[1]
    X2<-V*U[2]
    while(W>=1){
      U<-2*runif(2)-1
      W<- sum(U^2)
      V<-(-2*log(W)/W)^0.5
      X1<-V*U[1]
      X2<-V*U[2]
    }#If w = U1 + U2 > 1, reject the pair and generate new pair U1, U2#
    vector1[i]<-X1[1]
    vector2[i]<-X2[1]
  }
  vector1
  vector2 
  vector<-cbind(vector1,vector2)}#corresponding to Marsaglia and Bray algorithm#
  else if(method==2){
    vector1<- rep(0,n)
    vector2<- rep(0,n)
    for(i in 1:n){
      U<-runif(2)
      X1<-sin(2*pi*U[1])*(-2*log(U[2]))^0.5
      X2<-cos(2*pi*U[1])*(-2*log(U[2]))^0.5
      vector1[i]<-X1[1]
      vector2[i]<-X2[1]
    }
    vector1
    vector2 
    vector<-cbind(vector1,vector2)}# corresponding to Box and Mueller algorithm #
  else if(method==3){
    vector<- rep(0,n) 
    for(i in 1:n){
      X<-sqrt(0.75)*(sum(runif(16))-8)
      vector[i]<-X[1]}
    vector}# corresponding to central-limit theorem algorithm #
  X<-as.data.frame(sd*vector+mean)
  return(X)
} 

