#I confirm that the attached is my own work, except where clearly indicated in the test.
####################################################################################################

#The function my.rnorm contains an algorithm that returns a vector of pseudo-random values 
#from a normal distribution using the Marsaglia and Bray's method.

#The function has various input-error traps, and can be applied to both odd and even scenario.


my.rnorm<-function(n,mean=0,sd=1){
  if (is.numeric(mean) ==FALSE || is.numeric(sd) ==FALSE ||is.numeric(n) ==FALSE) {
    stop("invalid arguments")      #First input trap: this if statement will only allow numeric 
  }                                #inputs to go through.                                 
  if (sd <= 0 || n<=0){
    stop("invalid arguments")      #This if statement will stop negative input for standard deviation  
  }                                #or vector length.    
  if ( length(n)!=1 || length(mean)!=1 ||length(sd)!=1){
    stop("invalid arguments")      #This if statement is ensuing that the inputs are single scalar.
  }
  if (n != floor(n)){              #This if statement is guarding the fact that the input n must 
    stop("invalid arguments")      #be a integer.
  }
  if (n %% 2 == 0 ){               #This if statement decides the length of the empty vectors which will be used to collect values.
    a.1=numeric(n/2)               #Vector of length n for even or (n+1) for odd, they both are divided by 2 as I am using two  
    a.2=numeric(n/2)               #empty vectors to collect values.  
  } else (a.1=a.2=numeric((n+1)/2)) 
  for (i in 1:length(a.1)){        #looping through the pairs    
    w=1.1                          #set w to be bigger than 1 to start the while loop.
    while (w > 1) {              
      U=runif(2,0,1)               #while w is bigger than 1, the loop will keep generating new pairs,
      U1=2*U[1]-1                  #until the condition w=U1^2+U2^2 <1 is met.
      U2=2*U[2]-1
      w=U1^2+U2^2
    }
    v=sqrt((-2*log(w))/w)          #After the while loop ended, a transformation is done on w to get v.
    X1=U1*v                       
    X2=U2*v                        #X1 and X2 are then formed by multiplying the pair of U with the v.  
    a.1[i]=X1                    
    a.2[i]=X2                      #add X1 and X2 to the previous defined empty vectors.      
  }
  a=c(a.1,a.2)                     #combining a.1 and a.2 into one vector 
  if (n %% 2 == 0 ){             
    EVENas=sd*(a)+mean             #if even n is entered, we will take all the values from a. 
    return(EVENas)
  } else(return(sd*(a[1:n])+mean)) #if odd n is entered, we will take the first n elements from a.
  
  
}


#######################################################################################
#######################################################################################

#The function general.rnorm also returns a vector of pseudo-random values from a normal
#distribution. However, it has one extra argument(method) compared to my.rnorm, and this 
#argument allows us to get the normal random deviates with three different methods.

#The methods being applied are: Marsaglia and Bray algorithm 
#                               Box and Mueller algorithm
#                               central-limit theorem algorithm
#with the codes 1,2 and 3 respectively.

#The function has the same input-error traps as above, and all with one extra argument "method".



general.rnorm=function(n,mean=0,sd=1,method=1){      
  if (is.numeric(mean) ==FALSE || is.numeric(sd) ==FALSE ||is.numeric(n) ==FALSE || is.numeric(method)==FALSE) {
    stop("invalid arguments")
  }
  if (sd <= 0 || n<=0 ){
    stop("invalid arguments")
  }
  if ( length(n)!=1 || length(mean)!=1 ||length(sd)!=1|| length(method)!=1){
    stop("invalid arguments")
  }
  if (n != floor(n)|| method!=floor(method)){
    stop("invalid arguments") 
  }
  if (method==1){                               #When method 1 is entered, the algorithm will trigger my.rnorm function
    return(my.rnorm(n,mean,sd))
  } 
  if ( method==2){                              #when method 2 is entered
    if (n%%2==0){                               
      b.1=numeric(n/2)                          #
      b.2=numeric(n/2)                          #  
    }else(b.1=b.2=numeric((n+1)/2))             # 
    for (i in 1:length(b.1)){                   #              
       U=runif(2,0,1)                           #
       X1=(sin(2*pi*U[1]))*sqrt(-2*log(U[2]))   #same logic as my.rnorm function,but just with different formula and without the while loop
       X2=(cos(2*pi*U[1]))*sqrt(-2*log(U[2]))   #
       b.1[i]=X1                                #                                       
       b.2[i]=X2                                #  
    }                                           #  
    b=c(b.1,b.2)                                #
    if (n %% 2 == 0 ){                          # 
      EVENas=sd*(b)+mean                        #
      return(EVENas)
    } else(return(sd*(b[1:n])+mean))
    
  } 
  if (method==3){                               #When method 3 is entered 
    d=numeric(n)
    for (i in 1:n){
      U=runif(16,0,1)
      X=(sum(U)-8)*sqrt(12/16)
      d[i]=X
    }
    d=sd*d+mean
    return(d)
  } else ( stop("invalid arguments"))
}



############################################################################################################################
############################################################################################################################

#This test function will print "true", if the output vector has n numbers.

#It has the same arguments as general.rnorm, which means it can be used to test all three methods.

testfun=function(n,mean=0,sd=1,method=1){
  x=general.rnorm(n,mean,sd,method)
  if(length(x)==n & is.numeric(x)){
    return("true")
  }else(return("false"))
}



#After we have the correct number of values, it is sensible to test whether these are the correct values.



############################################################################################################################
############################################################################################################################

#The main goal of this testing function is to assess how accurate the generated values are compared to the original rnorm
#function, in other words, we are trying to test whether or not the generated values are actually from a normal distribution. 

#There are four main parts in this function: Visual Checks (histograms and quantile-quantile normal plots)
#                                            Difference between the mean of rnorm and our functions(t.test:hypothesis testing)
#                                            Skewness and Kurtosis of the generated distribution(if statements)   
#                                            Shapiro wilk normality test(hypothesis testing)

#Note the input arguments in this function are the same as the general.rnorm function, thus, it allows us to investigate
#the normality of all three different methods.

#Function will return messages when generated values do not pass the test.


testfun2=function(n,mean=0,sd=1,method=1){
  #visual check
  #########################################################################################################################
  #########################################################################################################################
  par(mfrow=c(2,2))
  GeneratedMethod=general.rnorm(n,mean,sd,method)   #GeneratedMethod is a set of values, generated by the corresponding method, which will be tested. 
  RNORM=rnorm(n,mean,sd)                            #RNORM is a set of normal distributed values which will be used to test GeneratedMethod
  hist(GeneratedMethod,main=c("by using method",method), xlab=c(n,"normal random deviates"))
  hist(RNORM,main="by using rnorm",xlab=c(n,"normal random deviates"))
  qqnorm(GeneratedMethod,main=c("quantile-quantile plot for method",method))
  qqnorm(RNORM,main="quantile-quantile plot for rnorm")
  par(mfrow=c(1,1))
  #Difference between the mean of rnorm and our function
  #########################################################################################################################
  #########################################################################################################################
  
  #Ho:Urm=Ugm   vs   H1:Urm!=Ugm                    #Urm mean of RNORM, Ugm mean of GeneratedMethod.
  TT=t.test(RNORM,GeneratedMethod)
  print(TT)
  
  if (TT$p.value<0.05 ){
    print(paste("difference between mean of rnorm and method",method,"is",abs(mean(RNORM)-mean(GeneratedMethod))))
    return(paste("There is a difference between the mean of method",method, "and rnorm as 0 is not within the 95% CI"))
  }
  
  #skewness and kurtosis
  ##########################################################################################################################
  ##########################################################################################################################
  if (!require(moments)){
    install.packages("moments")
    library(moments)
  }else (library(moments))
  print(paste("skewness of the distribution generated by method",method,"is",skewness(GeneratedMethod)))
  print(paste("kurtosis of the distribution generated by method",method,"is",kurtosis(GeneratedMethod)))
  
  #Skewness is a measure of the symmetry of a distribution. Normal distribution has skewness 0.
  #We can assess whether or not GeneratedMethod has the same skewness as normal distribution(rnorm).
  #First, we simulate 1000 skewness(es) of rnorm. By doing so, we will have a distribution of
  #the "true" skewness of the normal distribution
  l=numeric(1000)
  for(i in 1:1000){
    l[i]=skewness(rnorm(n,mean,sd))
  }
  #Since we have a distribution, it is obvious that we can just work out the 95% CI using quantile command.
  Up=quantile(l,0.975)
  Do=quantile(l,0.025)
  
  #Finally, we simply compare the skewness of GeneratedMethod with the 95% CI.
  #if skewness of GeneratedMethod is out of the 95% CI, then it is clear that it is quite far away from 0, 
  #meaning that GeneratedMethod is not as symmetric as the normal distribution.
  
  if (skewness(GeneratedMethod)>Up || skewness(GeneratedMethod)<Do){
    return(paste("Distribution generated by method",method,"is not as symmetric as compared to normal distribution"))
  }                         
  
  
  #We can do the exact same thing for kurtosis.
  #Kurtosis is a measure of the tail-heaviness of the distribution. Typically, normal distribuion has kurtosis=3.
  H=numeric(1000)
  for(i in 1:1000){
    H[i]=kurtosis(rnorm(n,mean,sd))
  }
  Up=quantile(H,0.975)
  Do=quantile(H,0.025)
  
  if (kurtosis(GeneratedMethod)<Do || kurtosis(GeneratedMethod)>Up){
    return(paste("Distribution generated by method",method,"seems to have a lighter or heavier tail compared to the normal distribution"))
  }
 
  #formal test
  ######################################################################################################################
  ######################################################################################################################
   
  #One of the most popular tests for normality : Shapiro-wilk normality test 
  #H0:Generated values are from normal distribution.
  #H1:Generated values are not from normal distribution. 
  
  if(n<5000){                                       #As the limit on shapiro test is 5000.
    print(shapiro.test(GeneratedMethod))
    #We set the significant level to be 5% in this case. 
    if ((shapiro.test(GeneratedMethod)$p.value)<0.05){
      return(paste("Despite the fact that GeneratedMethod has roughly the same mean,skewness and kurtosis 
compared to normal distribution, however, the shapiro test informs us that the distribution
generated by method",method, "is in fact not a normal distribtion as it has a p-value less than our 
significance level(5%)"))
    }else (return(paste("From the visual check, the generated distribution does look normal and the difference between
the means is neglectable, and it also has roughly the same skewness and kurtosis as the normal
distribution. Furthermore, the p-value from the shapiro test is greater than the significance level(5%).
Thus, we have evidence to believe the data generated by method",method,"are from a normal distribution")))
  }else(return("please make sure n is less than 5000 before the shapiro test will be executed"))
  
}


  
