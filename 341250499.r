#I confirm that the attached is my own work,except where clearly indicated in the text.
#creates n simulations from a normal distribution using the Marsaglia and Bray 
my.rnorm<-function(n,mean=0,sd=1) {
  if(!is.numeric(n)|!is.numeric(mean)|!is.numeric(sd)|length(n)!=1|length(mean)!=1|length(sd)!=1){                          
    stop("invalid arguments",call. = FALSE) 
  }                                                         
  if(sd<0|n<=0|n%%1!=0){
    stop("invalid arguments",call. = FALSE)
  }                                                         #incorrect inputs with error message
  x<-matrix(0,nrow = 2,ncol = ceiling(n/2))
  for (i in 1:ceiling(n/2)) {
    while(TRUE){                                            # repeatedly produce pairs of uniform deviates until w<=1
      normal<-runif(2,0,1)*2-1
      w<-normal[1]^2+normal[2]^2
      if(w<=1) break
    }
    v<-sqrt(-2*log(w)/w)
    subnormal<-normal*v
    x[1,i]<-subnormal[1]
    x[2,i]<-subnormal[2]
  }
  vofx<-c(x)
  vofx<-vofx[1:n]                                   #if it is asked to produce an odd number of deviates                        
  vectorofx<-vofx*sd+mean                           #transform them into values with mean and sd
  return(vectorofx)
}

#creates n simulations from a normal distribution using three different way
general.rnorm<-function(n,mean=0,sd=1,method=1){
  if(!is.numeric(n)|!is.numeric(mean)|!is.numeric(sd)|!is.numeric(method)|length(n)!=1|length(mean)!=1|length(sd)!=1|length(method)!=1){                          
    stop("invalid arguments",call. = FALSE) 
  } 
  if(sd<0|n<=0|n%%1!=0){
    stop("invalid arguments",call. = FALSE)
  }                                                     
  if(method!=1&method!=2&method!=3){
    stop ("invalid arguments",call. = FALSE)
  }                                                    #incorrect inputs with error message
  if(method==1){                                       #use Marsaglia and Bray algorithm
    x<-matrix(0,nrow = 2,ncol = ceiling(n/2))
    for (i in 1:ceiling(n/2)) {
      while(TRUE){                                     # repeatedly produce pairs of uniform deviates until w<=1
        normal<-runif(2,0,1)*2-1
        w<-normal[1]^2+normal[2]^2
        if(w<=1) break
      }
      v<-sqrt(-2*log(w)/w)
      subnormal<-normal*v
      x[1,i]<-subnormal[1]
      x[2,i]<-subnormal[2]
    }
    vofx<-c(x)
    vofx<-vofx[1:n]                                   #if it is asked to produce an odd number of deviates
  }
  if(method==2){                                      #use Boxand Mueller algorithm
    x<-matrix(0,nrow = 2,ncol = ceiling(n/2))
    for (i in 1:ceiling(n/2)){
      normal<-runif(2,0,1)
      x[1,i]<-sin(2*pi*normal[1])*sqrt(-2*log(normal[2]))
      x[2,i]<-cos(2*pi*normal[1])*sqrt(-2*log(normal[2]))
    }
    vofx<-c(x)
    vofx<-vofx[1:n]                                   #if it is asked to produce an odd number of deviates
  }
  if(method==3){                                      #use central-limit theorem algorithm
    vofx<-rep(0,n)
    for (i in 1:n) {
      normal<-runif(16,0,1)
      vofx[i]<-(sum(normal)-8)*sqrt(12/16)
    }
  }
  vectorofx<-vofx*sd+mean                           #transform them into values with mean and sd
  return(vectorofx)
}

#check if every argument has default value except for n
test.1<-function(n){                                 #a is the value of n
  source('asmt1.r')
  x<-my.rnorm(n)
  if(!is.null(x))
    print("Except for n,every argument in function my.rnorm has default value.")
  else{
    print("Not every argument in function my.rnorm has default value.")
  }
}

#test if the argument mean is in the 95% confidence interval of return values' mean
#Since my.rnorm is kind of a part of general.rnorm, the function below is designed to test general.rnorm
test.2<-function(n,mean=0,sd=1,method=1){ #n is the size of sample
  source('asmt1.r')
  vectorofmean<-rep(0,100)
  for (i in 1:100) {                                 #sample 100 times
    x<-general.rnorm(n,mean,sd,method)#
    vectorofmean[i]<-mean(x)
  }
  interval<-rep(0,2)
  interval[1]<-mean-2*sd/sqrt(n)
  interval[2]<-mean+2*sd/sqrt(n)
  logicalvalue<-vectorofmean>=interval[1]&vectorofmean<=interval[2]
  p<-sum(logicalvalue)
  newp<-paste(p,"% sample mean fall within 2 standard deviations of the true population mean",sep = " ")
  print("Please try to make the size of sample larger(i.e n>30) to get a better result.")
  return(newp)
}