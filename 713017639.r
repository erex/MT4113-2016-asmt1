#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n){ #function of n length
  U<-c()                 #create vectors for each variable
          #U is uniformly distributed with mean 0 sd 1
  v<-c()
  w<-c()
  X<-c()
  
  for(i in 1:n){ #for loop to transform each generated U to unit square
    U<-runif(n,0,1)
    U<-2*U-1
    return(U)
  }
  w<-U[i]^2+U[i+1]^2    #define w as the unit circle
  while(w>1){           #while loop to reject the variates not within the unit circle
    U[i]<-runif(i,0,1)
    U<-2*U-1
    w<-U[i]^2+U[i+1]^2
    return(w)
  }
  v <- sqrt(-2*log(w)/w)
  return(v)
  for(i in 1:n){ #define normally distributed variates
    X<-U[i]*v[i]
    return(X)
  }
    
}
my.rnorm()
  

  

general.rnorm <- function(n,m=1){ #function has one more argument than my.rnorm
  x2<-c() #define variables
  x3<-c()
  P<-c()
  S<-c()
  if(m==1){ #marsaglia and bray's method
    x<-my.rnorm(n)
    return(x)
  } else if(m==2){ #box mueller algorithm
    for(i in 1:n){ #generate uniformly distributed deviates
      P[i]<-runif(1,0,1)
    }
    return(P)
     for(i in seq(1,n,2)){ #define normally distributed pair of variates
       x2[i]<- (sin(2*pi*P[i]))*sqrt(-2*log(P[i+1]))
       x2[i+1]<- (cos(2*pi*P[i]))*sqrt(-2*log(P[i+1]))
     }
  return(x2)
  } else if(m==3){ #central limit theorem
    x3<-c()
    for(i in 1:n){ #generate uniformly distributed deviates
      S[i]<-runif(1,0,1)
    }
  return(S)
    for(i in 1:16){
      sum(S[i-1])
    }
    return(S)
    for(i in seq(1,n,1)){ #define normally distributed variates
      x3[i]<-(S[i]-8)*sqrt(12/16)
    }
    return(x3)
  }
  
}
general.rnorm()
  
  
  
test<-function(n)
  f<-my.rnorm(n)
  
  