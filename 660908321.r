#I confirm that the attached is my own work, except where clearly indicated in the text.

#We first write a function for each of the three algorithms given.

#MARSAGLIA-BRAY ALGORITHM:
#The first two 'if' statements check that the arguments given are valid.
#Arguments must be of length 1; 'n' must be >0; 'sd' must be >=0.
#The algorithm produces a pair of normally distributed rv's. We repeat the algorithm n times,
#each time randomly selecting one of the two values produced and appending it to the vector
#of values to return. This way we obtain a vector of length n regardless of whether n is 
#even or odd.
marsaglia.bray<-function(n,mean=0,sd=1){
  if (length(n)!=1|length(mean)!=1|length(sd)!=1){
    return("invalid arguments")
  }
  else if (n<=0|sd<0){
    return("invalid arguments")
  }
  else{
    values<-c()
    for (j in 1:n){
      #Generating the two Ui and transforming them to unit square.
      a<-2*runif(1,0,1)-1
      b<-2*runif(1,0,1)-1
      w<-a^2+b^2
      #Ensuring that the pair of coordinates lies within the unit circle.
      while (w>1){
        a<-2*runif(1,0,1)-1
        b<-2*runif(1,0,1)-1
        w<-a^2+b^2
      }
      #Defining v and producing x1,x2. We also transform x1,x2 so that they have
      #the desired mean and sd.
      v<-sqrt(((-2)*log(w))/w)
      x1<-((a*v)*sd)+mean
      x2<-((b*v)*sd)+mean
      #Randomly selecting one of x1 or x2; discarding the other.
      values[j]<-sample(c(x1,x2),1)
    }
    return(values)
  }
}


#BOX-MUELLER ALGORITHM:
#First two 'if' statements check that given arguments are valid.
#The algorithm produces a pair of normally distributed rv's. Again,
#we run the algorithm n times, each time randomly selecting only one of the
#two values produced. 
box.mueller<-function(n,mean=0,sd=1){
  if (length(n)!=1|length(mean)!=1|length(sd)!=1){
    return("invalid arguments")
  }
  else if (n<=0|sd<0){
    return("invalid arguments")
  }
  else{
    values<-c()
    for (j in 1:n){
      #Generating the two Ui.
      a<-runif(1,0,1)
      b<-runif(1,0,1)
      #Making the required transormation.
      x1<-sin(2*pi*a)*sqrt((-2)*log(b))
      x2<-cos(2*pi*a)*sqrt((-2)*log(b))
      #Transforming x1,x2 so that they have the required mean and sd.
      x1<-(x1*sd)+mean
      x2<-(x2*sd)+mean
      #Randomly sampling one the two values; discarding the other.
      values[j]<-sample(c(x1,x2),1)
    }
    return(values)
  }
}


#CENTRAL LIMIT THEOREM ALGORITHM:
clt.method<-function(n,mean=0,sd=1){
  if (length(n)!=1|length(mean)!=1|length(sd)!=1){
    return("invalid arguments")
  }
  if (n<=0|sd<0){
    return("invalid arguments")
  }
  else{
    values<-c()
    for (i in 1:n){
      #Creating a vector of 16 random observations from a uniform ditribution.
      a<-runif(16,0,1)
      #Producing the random variable X by applying the CLT.
      #Note: the function sum(), gives the cumulative sum of all the entries of a vector
      x<-(sum(a)-8)*sqrt(12/16)
      #Transforming the X so that it has the required mean and sd.
      x<-(x*sd)+mean
      values[i]<-x
    }
    return(values)
  }
}

#Both my.rnorm and general.rnorm make use of one or all of the functions written above.

#MY.RNORM:
#The function is just using the Marsaglia-Bray algorithm to return a vector of
#n values from a normally distributed rv with a given mean and sd.
#We do not need to check the conditions of the arguments as this is done already in the 
#function implementing the M-B algorithm.
my.rnorm<-function(n,mean=0,sd=1){
  return(marsaglia.bray(n,mean,sd))
}


#GENERAL.RNORM:
#Here we only need to ensure that the argument given for 'method' is one of 1,2, or 3.
#Based on the value of 'method' the function utilises one of the three algorithms.
#Any other value given as 'method' will result in 'invalid argument'.
general.rnorm<-function(method,n,mean=0,sd=1){
  if (method==1){
    return(marsaglia.bray(n,mean,sd))
  }
  else if (method==2){
    return(box.mueller(n,mean,sd))
  }
  else if (method==3){
    return(clt.method(n,mean,sd))
  }
  else{
    return("invalid arguments")
  }
}


#TESTING THE CODE:
#Want to check that the code does in fact produce normally distributed random deviates.
#One way to test whether the function is producing the desired output is to check that
#the produced deviates have a mean and sd sufficiently close to the one that we had
#specified.
#The function test.mean.variance below calculates the sample mean and sample sd
#from a sample of n observations produced using our general.rnorm function.
#We set the default for n to be 500 because we want to ensure that the number of 
#observations is sufficient.
test.mean.variance<-function(method,n=500,mean=0,sd=1){
  a<-general.rnorm(method,n,mean,sd)
  est.mean=sum(a)/n
  b<-(a-est.mean)^2
  est.var=sum(b)/(n-1)
  est.sd=sqrt(est.var)
  return(c(est.mean,est.sd))
}
#Suppose we want to check that the general.rnrom function is producing
#deviates of mean=10, sd=2, using the Box-Mueller algorithm. A typical result gives:
# > test.mean.variance(2,n=500,mean=10,sd=2)
# [1] 10.073465  1.954917
#First value gives sample mean; second gives sample sd. Both are fairly close to 
#the values that we had specified. 
#We should also replicate this test many times, say 100, and observe the distribution of the
#sample means and sample sds obtained. This is a much stronger way to test the code. 
#For example, we could produce a histogram of the 100 sample means obtained.
repeated.test<-function(test,method,n=500,mean=0,sd=1){
  if (test==1){
    sample.means<-c()
    for (i in 1:100){
      sample.means[i]<-test.mean.variance(method,n,mean,sd)[1]
    }
    return(hist(sample.means))
  }
  else if (test==2){
    sample.sds<-c()
    for (i in 1:100){
      sample.sds[i]<-test.mean.variance(method,n,mean,sd)[2]
    }
    return(hist(sample.sds))
  }
  else{
    return('invalid arguments')
  }
}

#Finally, we should check the observations appear to come from a normal distribution. 
#An informal way to do this is to produce a QQ plot. 
qq.plot<-function(method,n=500,mean=0,sd=1){
  return(qqplot(rnorm(n,mean,sd),general.rnorm(method,n,mean,sd)))
}
#Sticking to the previous example, we could produce the following:
#qq.plot(method=2,n=500,mean=10,sd=2)
#and then visually evaluate whether the observations seem to come from a normal distribution.