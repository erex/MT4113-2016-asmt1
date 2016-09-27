#I confirm that the attached is my own work, except where clearly indicated in the text.

# Function my.rnorm generates n normally distributed random variables, with a desired mean and sd.
# The mean and sd by default equal to 0 and 1 respectively.
# Function use Marsaglia and Bray¡¯s method (Maindonald 1984).
my.rnorm<-function(n,mean=0,sd=1){
  
  #Boolean variable used to record whether input n is a positive whole number or not 
  is.positive.whole<-FALSE
  #Boolean variable used to record whether input sd is positive or not 
  is.positive<-FALSE
  #First check whether n is a numeric scaler or not
  cond1<-(is.numeric(n))&&(is.vector(n))&&(length(n)==1)
  #If n is a numeric scaler, check whether it is a positive whole number and record the result
  if(cond1) is.positive.whole<-(n%%1==0)&&(n>0)
  #Check whether mean and sd is a numeric scaler or not 
  cond2<-(is.numeric(mean))&&(is.vector(mean))&&(length(mean)==1)
  cond3<-(is.numeric(sd))&&(is.vector(sd))&&(length(sd)==1)
  #If sd is a numeric scaler, check whether it is positive
  if(cond3) is.positive<-(sd>0)
  
  #If all the input arguments are valid, generate the random variables
  if(is.positive.whole&&cond2&&is.positive){
    
    #Boolean variable used to record whether the required n is odd or even
    isodd<-FALSE
    #If n is odd, record it and add 1 to n in order to generate pairs of normal random variables
    if(n%%2==1){
      n<-n+1
      isodd<-TRUE
    }
    #Using matrix result to hold pairs of normal random variables
    result<-matrix(nrow=2,ncol=n/2)
    #Using for loop to generate pairs of standard normal random variables, number of iteration is n/2
    for(i in 1:n/2){
      #Initiate vector X to hold the pair of standard normal random variables in each iteration 
      X<-rep(NA,2)
      #Initiate the vector U to hold the pair of uniform random variables
      #Let each value of the vector to be 5, then sum(U^2)>1 is true. 
      #The following while loop can be executed
      U<-rep(5,2)
      #Generate pair of uniform random varibles until U is within the unit circle
      while(sum(U^2)>1){
        U<-runif(2)
        #Transfrom U to let U within the range of [-1,1]
        U<-U*2-1
      }
      #If U is within the unit circle, follow the algorithm to transform U into X
      w<-sum(U^2)
      v<-sqrt(-2*log(w)/w)
      X<-U*v
      #Store the value of pair of standard normally distributed variables in each iteration 
      result[,i]<-X
    }
    #transform the result matrix into a result vector to return it in the form of a vector
    result<-as.vector(result)
    #transform the standard normally distributed variables into the desired normally distributed variables  
    result<-result*sd+mean
    #If the input n is odd, delete the last variables as n is increased by one previously to generate pairs
    if(isodd) result<-result[-n]
    return(result)
    
  }else{
    #If input arguments are invalid, stop function execution and give error message invalid arguments
    stop("invalid arguments")
  }
}

# Function general.rnorm generates n normally distributed random variables, with a desired mean and sd 
# by choosing a desired algorithm.
# The mean and sd by default equal to 0 and 1 respectively. The method is one by default.
general.rnorm<-function(n,mean=0,sd=1,method=1){
  
  #Boolean variable used to record whether input n is a positive whole number or not
  is.positive.whole<-FALSE
  #Boolean variable used to record whether input sd is positive or not 
  is.positive<-FALSE
  #First check whether n is a numeric scaler or not
  cond1<-(is.numeric(n))&&(is.vector(n))&&(length(n)==1)
  #If n is a numeric scaler, check whether it is a positive whole number and record the result
  if(cond1) is.positive.whole<-(n%%1==0)&&(n>0)
  #Check whether mean and sd is a numeric scaler or not 
  cond2<-(is.numeric(mean))&&(is.vector(mean))&&(length(mean)==1)
  cond3<-(is.numeric(sd))&&(is.vector(sd))&&(length(sd)==1)
  #If sd is a numeric scaler, check whether it is positive
  if(cond3) is.positive<-(sd>0)
  #Check whether the input argument method is a numeric scaler or not 
  cond4<-(is.numeric(method))&&(is.vector(method))&&(length(method)==1)
  
  #If all the input arguments are valid, generate the random variables
  if(is.positive.whole&&cond2&&is.positive&&cond4){
    #If user choose method 1,use my.rnorm to generate desired normal random variables
    if(method==1){
      my.rnorm(n,mean,sd)
      
    #If user choose method2, use Box-Mueller algorithm. Jones et al(2009) to generate.
    }else if(method==2){
      #Boolean variable used to record whether the required n is odd or even
      isodd<-FALSE
      #If n is odd, record it and add 1 to n in order to generate pairs of normal random variables
      if(n%%2==1){
        n<-n+1
        isodd<-TRUE
      }
      #Using matrix result to hold pairs of normal random variables
      result<-matrix(nrow=2,ncol=n/2)
      #Using for loop to generate pairs of standard normal random variables, number of iteration is n/2
      for(i in 1:n/2){
        #Initiate vector X to hold the pair of standard normal random variables in each iteration 
        X<-rep(NA,2)
        #Generate a pair of uniform deviates and transform them into standard normal deviates
        U<-runif(2)
        X[1]<-sin(2*pi*U[1])*sqrt(-2*log(U[2]))
        X[2]<-cos(2*pi*U[1])*sqrt(-2*log(U[2]))
        #Store the value of pair of standard normally distributed variables in each iteration 
        result[,i]<-X
      }
      #Transform the result matrix into a result vector to return it in the form of a vector
      result<-as.vector(result)
      #Transform the standard normally distributed variables into the desired normally distributed variables 
      result<-result*sd+mean
      #If the input n is odd, delete the last variables as n is increased by one previously to generate pairs
      if(isodd) result<-result[-n]
      return(result)
      
    #If user choose method3, use Central limit theorem. Mood et al. (1974) to generate.
    }else if(method==3){
      #Using vector result to store value of normal random variables generated
      result<-rep(NA,n)
      #Using for loop to generate standard normal random variables, number of iteration is n
      for(i in 1:n){
        #Generate 16 uniform deviates
        U<-runif(16)
        #Transform them into a standard normal variable and store it 
        result[i]<-(sum(U)-8)*sqrt(12/16)
      }
      #Transform the standard normally distributed variables into the desired normally distributed variables 
      result<-result*sd+mean
      return(result)
      
    #If input methods is not 1 or 2 or 3, stop function execution and give error message invalid arguments
    }else stop("invalid arguments")
  
  #If input arguments are invalid, stop function execution and give error message invalid arguments
  }else{
    stop("invalid arguments")
  }
}

# Function test.function is used to test whether function general.rnorm and my.rnorm can work well
# If method equal to one , this function can test the my.rnorm function 
# The arguments used are the same as the input arguments for the general.rnorm
# This function test whether the output of general.rnorm gives desired number of random variables with 
# correct mean and sd using required method
# In addition, it tests whether the distribution is indeed normal by giving graphical as well as
# statistical tests.
# This function can also test whether general.rnorm include appropriate error traps on the inputs
test.function<-function(n,mean=0,sd=1,method=1){
  
  #Call general.rnorm to generate normal deviates
  result.random<-general.rnorm(n=n,mean=mean,sd=sd,method=method)
  
  #If the input arguments are correct, check whether the function can work well
  if(is.numeric(result.random)==TRUE){
    #Test whether the resulting numbers has the requied length
    num<-length(result.random)
    #Test whether the resulting numbers has the requied mean
    result.mean<-mean(result.random)
    #Test whether the resulting numbers has the requied standard deviation
    result.sd<-sd(result.random)
    #Test whether the resulting numbers indeed follow a normal distribution or not
    #The shapiro test is invented by Shapiro S. S. and Wilk M.(1965)
    #If p value greater than required level of significance(eg. 0.05), the numbers are 
    #normally distributed
    normal.test<-shapiro.test(result.random)
    
    #Plot the histogram to check the normality of results graphically
    #Compare the histogram with a normal density curve
    histgram<-hist(result.random,prob=TRUE,xlim=c((mean-4*sd),(mean+4*sd)))
    #Add a verticle line in the histogram, the location of the line is the mean of the resulting numbers
    abline(v=mean(result.random),col="red")
    #Create a sequence used to plot the normal density curve 
    x<-seq((mean-4*sd),(mean+4*sd),length=10000)
    #Use the sequence to calculate the normal density value
    yfit<-dnorm(x,mean=mean,sd=sd) 
    #Add the normal density curve to the histogram to compare whether the histogram roughly follows
    #normal distribution. 
    lines(x, yfit, col="blue")
    
    #Plot normal qq to check the normality of results graphically
    #The qqnorm test is developed by Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
    #If the plot roughly is a line of equation y=x, the numbers are normally distributed
    qqplot<-qqnorm(result.random)
    return(list(number=num,mean=result.mean,sd=result.sd,shpiro.test=normal.test))
  }
}


