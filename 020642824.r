#I confirm that the attached is my own work except where indicated in the text
#Q1
my.rnorm<- function(n,mean=0,sd=1){
  i=0 ##define the variable
  X<-c()
  while(i<n){
   w=2
   while(w>1){
   U1<-runif(2,mean,sd) ##obtain the variable from uniform
   U2<-2*U1-1
   w<-sum(U2^2)
   }
  v <- sqrt(-2*log(w)/w)
  X<- c(X,U1*v)
  i=i+1
  }
  return(X)
}

#Q2
general.rnorm <- function(n,mean=0,sd=1,method=1){
  if(method==1){
    i=0
    X=c() ##define the variable
    while(i<n){
      w=2
      while(w>1){
        U1<-runif(2,mean,sd) ##obtain the variable from uniform
        U2<-2*U1-1
        w<-sum(U2^2)
      }
      v<- sqrt(-2*log(w)/w)
      X<- c(X,U1*v)
      i=i+1
    }
    return(X)
  }else if(method==2){
    X=c()
    i=0##define the variable
    while(i<n){
      U1<- runif(2,mean,sd) ##obtain the variable from uniform
      X1<- sin(2*pi*U1[1])*sqrt(-2*log(U1[2]))
      X2<- cos(2*pi*U1[1])*sqrt(-2*log(U1[2])) 
      X<- c(X,X1,X2)
      i=i+1
    }
    return(X)
    }
    else if(method==3){
      X=c()
      i=0##define the variable
      while(i<n){
        U1<-runif(16,mean,sd) ##obtain the variable from uniform
        X<- c(X,(sum(U1)-8)*sqrt(12/16))
        i=i+1
      }
      return(X)
    }
}


