#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n,mean=0,sd=1){
  if (is.numeric(c(n,mean,sd))==FALSE){stop("invalid arguments")} #check input is numerical
  if (length(c(n,mean,sd))>3){stop("invalid arguments")} #check input are scalars
  if (n%%2 != 0){ #checking to see if n is even
    n=n+1 #if n is odd add 1
    even=FALSE
  } else{even=TRUE}
  f <- c() #set up empty vector to store output
  for(m in 1:(n/2)){
    circletest = FALSE
    count=0
    while(circletest==FALSE){
      u <- 2*(runif(2))-1
      w = u[1]^2+u[2]^2
      count=count+1 #counts number of repetitions of while loop
      if(count==1000){stop("count too high")} #prevents infinite loop
      if(w<=1){circletest=TRUE}
    }
    v=sqrt((-2*log(w))/w)
    x=(u*v)*sd+mean
    f <- append(f,x) #stores the output in the vector f
  }
  if (even==FALSE){f<-f[-n]} #if n was odd, remove one value from vector
  return(f)
}


general.rnorm <- function(n,mean=0,sd=1,method=1){
  if (is.numeric(c(n,mean,sd))==FALSE){stop("invalid arguments")} #check input is numerical
  if (length(c(n,mean,sd))>3){stop("invalid arguments")} #check input are scalars
  if (method == 1){
    return(my.rnorm(n,mean,sd))
  } else if (method==2){
    if (n%%2 != 0){ #checks if n is even
      n=n+1 #adds 1 if n is odd
      even=FALSE
    }else{even=TRUE}
    f <- c()
    for (i in 1:(n/2)){
      u <- runif(2)
      x1 = (sin(2*pi*u[1]))*sqrt(-2*log(u[2]))
      x2 = (cos(2*pi*u[1]))*sqrt(-2*log(u[2]))
      f <- append(f,c(x1,x2)) #store data
    }
    f <- sd*f+mean
    if (even==FALSE){f<-f[-n]} #if n was odd, remove final value from f so f has length n
    return(f)
  } else if (method == 3){
    f <- c()
    for (i in 1:n){
      u <- runif(16)
      x = (sum(u)-8)*sqrt(12/16)
      f <- append(f,x)
    }
    f <- sd*f+mean
    return(f)
  } else{stop("invalid arguments")} #if method equals anything other than 1,2, or 3 then error message is returned
}


#testing

test.normality <- function(n=10,mean=0,sd=1,method=1){ # uses k-s test to test for normality, and shows a q-q plot
  f <- general.rnorm(n,mean,sd,method)
  qqnorm(f) #creates q-q plot
  p.value= ks.test(f,"pnorm",mean,sd)$p.value
  if (p.value<0.01){return(FALSE) #small chance of type 1 or type 2 error
  }else{return(TRUE)}             #but should give a rough idea if function is returning normally distributed values
}

test.length <-function(n,mean=0,sd=1,method=1){ #checks that the length of the vector is equal to n
  x <- general.rnorm(n,mean,sd,method)
  if (length(x)!=n){return(FALSE)}
  else{return(TRUE)}
}


