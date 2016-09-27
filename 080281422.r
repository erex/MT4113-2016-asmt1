#I confirm that the attached is my own work, except where clearly indicated in the text.
#
#
#
#
#We first encode the three seperate algorithms
#to make the code for my.rnorm and general.rnorm neater
#
#
#
marsaglia.bray<-function(mean,sd){
#
#   Purpose:
#         generate a pair of N(mean,sd^2) independent eandom deviates using the
#         Marsaglia-Bray algorithm
#   Input:
#         mean - mean of the desired distribution
#         sd - standard deviation of the desired distribution
#   Output:
#         A single vector of length two containing the pair of deviates
#
#
# first generate two U(0,1) random deviates
  U<-2*(runif(2,0,1))-1
# run a while loop, checking that U1^2+U2^2<=1, and if not simulate another
# pair of random deviates
  while(U[1]^2+U[2]^2>1){
   U<-runif(2,0,1)
   U<-2*U-1
  }
# create w, v, and X as detailed in the algorithm and return 
# X which contains the final two values.
  w<-U[1]^2+U[2]^2
  v<-sqrt((-2*log(w))/w)
  X<-c(U[1]*v,U[2]*v)
# this has created two N(0,1) random deviates, so we transform 
# them to N(mean,sd^2) random deviates
  X<-X*sd+mean
  return(X)
}



#
box.mueller<-function(mean,sd){
#   Purpose:
#         generate a pair of N(mean,sd^2) independent random deviates using the
#         Box-Mueller algorithm
#   Input:
#         mean - mean of the desired distribution
#         sd - standard deviation of the desired distribution
#   Output:
#         A single vector of length two containing the pair of deviates
#
#
# create two N(0,1) random variates
  U<-runif(2,0,1)
  X<-c(sin(2*(pi)*U[1])*sqrt(-2*log(U[2])),cos(2*pi*U[1])*sqrt(-2*log(U[2])))
# now transform to two N(mean,sd^2) random variates
  X<-X*sd+mean
  return(X)
}



#
CLT<-function(mean,sd){
#   Purpose:
#         generate a N(mean,sd^2) random deviate using the
#         Moody et al CLT algorithm
#   Input:
#         mean - mean of the desired distribution
#         sd - standard deviation of the desired distribution
#   Output:
#         The single deviate
#
#
#
# create one N(0,1) random variate
  U<-runif(16,0,1)
  X<-(sum(U)-8)*sqrt(12/16)
# now transform to one N(mean,sd^2) random variate
  X<-X*sd+mean
  return(X)
}


my.rnorm<-function(n,mean=0,sd=1){
  #
  # Purpose:
  #       Generate n independent random deviates from a N(mean,sd^2) using the Marsaglia-Bray method
  #
  # Input:
  #       n - number of random deviates desired, must be non-negative integer. n=0 returns numeric(0)
  #       mean- mean of the desired Normal distribution, must be numeric scalar, if not input defaults to 0
  #       sd- standard deviation of the desired Normal distribution, if not input
  #       defaults to 1, must be numeric scalar and non-negative
  #
  # Output:
  #       vector containing n N(mean,sd^2) random deviates 
  #
  #
# check a value for n has been input
  if(missing(n)){
     stop("invalid arguments")
# check the input values for n, mean, sd are numeric
  }else if(!(all(is.numeric(n),is.numeric(mean),is.numeric(sd)))){
      stop("invalid arguments")
# check n, mean, sd are single values
  }else if(!all(length(n)==1,length(mean)==1,length(sd)==1)){
      stop("invalid arguments")
# check n is integer, non-negative, sd is non-negative
  }else if(!(all(floor(n)==n,n>=0,sd>=0))){
      stop("invalid arguments")
# now produce random deviates
# if n=0 return numeric(0)
  }else if(n==0){
      return(numeric(0))
# if n=1 produce one random deviate 
  }else if(n==1){
      X<-marsaglia.bray(mean,sd)
      return(X[1])
# If n>1,begin by creating floor(n/2) N(mean,sd^2) random variates
  }else{
    X<-c()
    for (i in 1:floor(n/2)){
      X<-append(X,marsaglia.bray(mean,sd))
    }
# if n is even, end
    if (floor(n/2)==n/2){
      return(X)
    }else{
# if n is odd add one final term
      X<-append(X,marsaglia.bray(mean,sd)[1])
      return(X)
}
}
}


general.rnorm<-function(n,mean=0,sd=1,method){
  #
  # Purpose:
  #       generate n independent N(mean,sd^2) random deviates using the Marsaglia-Bray, Box-Mueller, or CLT method
  #
  # Input:
  #       n - number of random deviates required, must be non-negative integer, n=0 returns numeric(0)
  #       mean - mean of desired N(mean,sd^2) distribution, defaults to 0 if not input, Must be numeric scalar
  #       sd - standard deviation of desired N(mean,sd^2) distribution, defaults to 1 if not input. 
  #       sd must be non-negative numeric scalar
  #       method - desired method to generate the random deviates, must be one of 1,2,3, use: 
  #               1 - Marsaglia-Bray method
  #               2 - Box-Mueller method
  #               3 - Mood method
  #
  # Output:
  #       vector containing n N(mean,sd^2) random deviates generated using the desired method
  #
#
# check values for n, method have been input
  if(!(all(missing(n)==FALSE,missing(method)==FALSE))){
    stop("invalid arguments")
# check the input values for n, mean, sd, method are numeric
  }else if(!(all(is.numeric(n),is.numeric(mean),is.numeric(sd),is.numeric(method)))){
    stop("invalid arguments")
# check n, mean, sd, method are single values
  }else if(!all(length(n)==1,length(mean)==1,length(sd)==1,length(method)==1)){
    stop("invalid arguments")
# check n is integer, non-negative, sd is non-negative, method is one of 1, 2, 3
  }else if(!(all(floor(n)==n,n>=0,sd>=0,is.element(method,c(1,2,3))))){
    stop("invalid arguments")
#
# check if we are using the marsaglia-brey method
#
  }else if(method==1){
# now produce random deviates
    X<-my.rnorm(n=n,mean=mean,sd=sd)
    return(X)
#
# check if we are using the box-meuller algorithm
#
  }else if(method==2){
# now produce random deviates
# if n=0 return numeric(0)
    if(n==0){
        return(numeric(0))
# if n=1 produce one random deviate  
    }else if(n==1){
        X<-box.mueller(mean,sd)
        return(X[1])
# If n>1,begin by creating floor(n/2) N(mean,sd^2) random variates                
    }else{
        X<-c()
        for (i in 1:floor(n/2)){
        X<-append(X,box.mueller(mean,sd))
        }
    # if n is even, end
        if (floor(n/2)==n/2){
            return(X)
        }else{
    # if n is odd add one final term
            X<-append(X,box.mueller(mean,sd)[1])
            return(X)
        }
        }
# check if we are using the Mood et al CLT method
#
    }else if(method==3){
        if(n==0){
            return(numeric(0))
        }else{
            X<-c()
            for(i in 1:n){
                X<-append(X,CLT(mean,sd))
            }
            return(X)}
                    
}
}


#
#
#

# We now produce test functions
#
#
small.function.test<-function(n,function.no){
#
# Purpose:
#         test the chosen function produces numeric deviates of the correct number
#
# Input:
#         n - number of deviates function should produce, must be non-negative ineger 
#         function.no - chosen function to simulate deviates, must be one of 0,1,2,3. Use:
#               0 - use my.rnorm function
#               1 - use general.rnorm with method=1
#               2 - use general.rnorm with method=2
#               3 - use general,rnorm with method=3
#
#
# Output:
#       Returns TRUE if correct number of numeric deviates are produced, FALSE otherwise
# 
  # check a value for n, function.no has been input
  #
  if(!(all(missing(n)==FALSE,missing(function.no)==FALSE))){
    stop("invalid arguments")
 }else if(!all(is.numeric(n),is.numeric(function.no))){
    stop("invalid arguments")
  #
  #function.no is 0,1,2,3
 }else if(is.element(function.no,c(0,1,2,3))==FALSE){
    stop("invalid arguments")
  #
  }else{
  # randomly simulate a mean and standard deviation
  Mean<-runif(1,-100,100)
  SD<-runif(1,0,100)
  #
  # create the sample
  #
  if(function.no==0){
    test<-my.rnorm(n,mean=Mean,sd=SD)
  }else{
    test<-general.rnorm(n=n,mean=Mean,sd=SD,method=function.no)
  }
  #
  # check the sample is the correct length
  #
  if(length(test)!=n){
    print("ERROR:does not produce correct number of deviates")
    return(FALSE)
  #
  #check the sample is numeric
  #
  }else if(is.numeric(test)==FALSE){
    print("ERROR:non numeric output")
  return(FALSE)
  }else{
    return(TRUE)
  }
}
}




function.test<-function(my.rnorm,general.rnorm){
#function.test
#
# Purpose:
#     To check that the functions my.rnorm, general.rnorm are working
#
# Input:
#     my.rnorm 
#     general.rnorm
#
# Output:
#     Produces a normal qq plot for a a set of random deviates produced by each 
#     use of the function, my.rnorm and general.rnorm with method = 1, 2, 3
#     ,with random number of deviates, mean, and standard
#     deviation. It returns a list of the randomly generated desired sample lengths, means, and sds, as well
#     the sample lengths, means, sds.
#     We also apply the Shapiro-Wilks test for normality to each of the
#     generated samples and return the test statistic and p-value for each sample.
#     As well as this it returns for each use of the function (see above)
#     a p-value and test statistic for a t-test of mean of each sample (under hypothesis= to mean input to 
#     function), as well as a p-value and test statistic for a chi squared test
#     of sample variance for each sample( under null hpothesis variance = sd^2.)
#     Note that the variance test is equivalent to testing standard deviation.
#
# check functions are entered
#
if(!(all(missing(my.rnorm)==FALSE,missing(general.rnorm)==FALSE))){
  stop("invalid arguments")
}
par(mfrow=c(2,2))
#
# simulate n, mean, sd for each function
#
N<-round(runif(4,1000,5000))
Mean<-runif(4,-100,100)
SD<-runif(4,0,100)
#
# set up variables to record values
#
shapiro.p.value.store<-c(0,0,0,0)
shapiro.test.stat.store<-c(0,0,0,0)
mean.p.value<-c(0,0,0,0)
mean.test.final<-c(0,0,0,0)
var.p.value<-c(0,0,0,0)
var.test.final<-c(0,0,0,0)
sample.sd<-c(0,0,0,0)
sample.mean<-c(0,0,0,0)
sample.length<-c(0,0,0,0)

#
# Apply tests to my.rnorm
# simulate the data
#
  Y<-my.rnorm(n=N[1],mean=Mean[1],sd=SD[1])
  sample.length[1]<-length(Y)
  sample.sd[1]<-sd(Y)
  sample.mean[1]<-mean(Y)
#
# Now perform the Shapiro-Wilk Normality test, and store
# the test statistic and p-value
  shw<-shapiro.test(Y)
  shapiro.p.value.store[1]<-shw$p.value
  shapiro.test.stat.store[1]<-shw$statistic
#
# test the variance of the sample
# under the null hypotheses that the variation of the distribution
# is the same as the specified variance, Q=(n-1)s^2/sigma^2~chisquared on n-1
# degrees of freedom.
# calculate the test statistic
  Q<-(N[1]-1)*var(Y)/(SD[1])^2
  var.test.final[1]<-Q
#decide which side of the distribution the test statistic is on
#and calculate the p-value for the two-sided test
  if(Q<=qchisq(0.5,N[1]-1)){
    var.p.value[1]<-2*pchisq(Q,N[1]-1)
  }else{
    var.p.value[1]<-2*(1-pchisq(Q,N[1]-1))
  }
#
#
#repeat for the mean test; one sample t-test
#calculate the test statistic for the mean test, and apply the test
# under the null hypotheses that the mean of the distribution
# is the same as the specified mean, T=(mean(sample)-mu)/(s/sqrt(n))~tn-1
# since the t distribution is symmetric about the distribution, we can simply
# calculate P(A<=-|T|), or equivalently P(A>=|T|), and then multiply by two
# since this is a two-sided test.
#
  t<-(mean(Y)-Mean[1])/(sqrt(var(Y)/N[1]))
  mean.test.final[1]<-t
  mean.p.value[1]<-2*pt(-abs(t),N[1]-1)
#
#now plot the qqnorm
#
  qqnorm(Y,main="Normal Q-Q plot of my.rnorm")
#
#repeat the tests with general.rnorm and methods 1, 2, 3
#
for (i in (2:4)){
  Y<-general.rnorm(n=N[i],mean=Mean[i],sd=SD[i],method=(i-1))
  sample.length[i]<-length(Y)
  sample.sd[i]<-sd(Y)
  sample.mean[i]<-mean(Y)
  shw<-shapiro.test(Y)
  shapiro.p.value.store[i]<-shw$p.value
  shapiro.test.stat.store[i]<-shw$statistic
  Q<-(N[i]-1)*(var(Y))/(SD[i])^2
  var.test.final[i]<-Q
  if(Q<=qchisq(0.5,N[i]-1)){
    var.p.value[i]<-2*pchisq(Q,N[i]-1)
  }else{
    var.p.value[i]<-2*(1-pchisq(Q,N[i]-1))
  }
  t<-(mean(Y)-Mean[i])*sqrt(N[i]/var(Y))
  mean.test.final[i]<-t
  mean.p.value[i]<-2*pt(-abs(t),N[i]-1)
  qq.name<-c("Normal Q-Q plot of general.rnorm method=",i)
  qqnorm(Y,main=qq.name)
 }
#
#create lists to output and return them
#
method.list<-list("my.rnorm, general.rnorm method 1, general.rnorm method 2, general.rnorm method 3")
function.test.lengths<-list("desired sample sizes"=N)
function.test.mean<-list("desired means"=Mean)
function.test.sd<-list("desired sd"=SD)
function.test.deviate.lengths<-list("actual sample sizes"=sample.length)
function.test.deviate.mean<-list('actual sample means'=sample.mean)
function.test.deviate.sd<-list("actual sample sd"=sample.sd)
function.test.output.swstat<-list("Shapiro-Wilks test statistic"=shapiro.test.stat.store)
function.test.output.swpvalue<-list("Shapiro-Wilks p-value"=shapiro.p.value.store)
function.test.output.tstat<-list("test statistic from t test"=mean.test.final)
function.test.output.tpvalue<-list("p-value from t test for mean of the desired distribution"=mean.p.value)
function.test.output.chitest<-list("test statistic from Chi Squared test for variance of the desired distribution"=var.test.final)
function.test.output.chipvalue<-list("p-value from Chi Squared test for variance of the desired distribution"=var.p.value)
observed.list<-list(method.list,function.test.lengths,function.test.deviate.lengths,function.test.mean,function.test.deviate.mean,function.test.sd,function.test.deviate.sd)
test.list<-list(function.test.output.swstat,function.test.output.swpvalue,function.test.output.tstat, function.test.output.tpvalue,function.test.output.chitest,function.test.output.chipvalue)
final.output<-list(observed.list,test.list)
return(final.output)
}


