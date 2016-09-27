#I confirm that the attached is my own work, except where clearly indicated in the text.
################################################################################################
#First my.rnorm function to transform uniform random numbers to the normal distribution
#specify the function and set arguments, some are default. n is the number of wanted normal random values with mean and sd as desired

my.rnorm <- function(n, mean=0, sd=1){

  #Set some conditions (error traps) on the inputs, like ensuring the numbers are real, positive, finite and scalar
  if (n < 0) stop("invalid arguments")
  if (is.complex(n)==TRUE) stop("invalid arguments")
  if (is.complex(mean)==TRUE) stop("invalid arguments")
  if (is.complex(sd)==TRUE) stop("invalid arguments")
  if (n == Inf) stop("invalid arguments")
  if (mean == Inf) stop("invalid arguments")
  if (sd == Inf) stop("invalid arguments")
  if (sd < 0) stop("invalid arguments")
  if (length(n) > 1) stop("invalid arguments")
  if (length(mean) > 1) stop("invalid arguments")
  if (length(sd) > 1) stop("invalid arguments")

  #Construct a function that checks whether an input is a whole number 
  #(as per reference https://stat.ethz.ch/R-manual/R-devel/library/base/html/Round.html). 
  #Since we are generating pairs of variables, if the user wants, say, n=10, then five such pairs are needed. 
  #For n being odd we cannot generate half a pair, so will later round up if n is odd and then delete one element of the pair at the end

  is.whole <- function(a) { floor(a)==a }

  #Another condition using the above is.whole function
  if(is.whole(n)==FALSE) stop("invalid arguments")

  #Set k for the number of pairs we want to generate
  k<- if(is.whole(n/2)==FALSE) (n+1) else n

  #Build matrix X as the output and generate pairs of uniform rvs
  X<-matrix(0, ncol=2, nrow=(k/2)) 
  for(i in 1:k/2){ 
    U<-runif(2,0,1) 
    U<-2*U-1 
    w<-sum(U^2) 

    #Condition on the sum of uniform pairs to fall inside the unit disk
    while(w>1){ 
      U<-runif(2,0,1) 
      U<-2*U-1 
      w<-sum(U^2) 
    }

    #Transform the uniform variables and feed them into the matrix X, then transform into a vector, picking the first n elements (covers the case when user specifies odd number)
    v<-sqrt(-2*log(w)/w) 
    X[i,]<-v*U 
  } 
  return(head(as.vector((X*sd)+mean),n))
}

################################################################################################

#Second function, general.rnorm
  
  #Define the Box Mueller method, output matrix X and impose error traps like before

  box<-function(n, mean=0, sd=1) {

    if (n < 0) stop("invalid arguments")
    if (is.complex(n)==TRUE) stop("invalid arguments")
    if (is.complex(mean)==TRUE) stop("invalid arguments")
    if (is.complex(sd)==TRUE) stop("invalid arguments")
    if (n == Inf) stop("invalid arguments")
    if (mean == Inf) stop("invalid arguments")
    if (sd == Inf) stop("invalid arguments")
    if (sd < 0) stop("invalid arguments")
    if (length(n) > 1) stop("invalid arguments")
    if (length(mean) > 1) stop("invalid arguments")
    if (length(sd) > 1) stop("invalid arguments")
    is.whole <- function(a) { floor(a)==a }
    k<- if(is.whole(n/2)==FALSE) {n+1} else {n}
  
    X<-matrix(0, ncol=2, nrow=(k/2))
    
    #generate random uniform variables
    U<-matrix(0, ncol=2, nrow=k/2)
    U[,1]<-runif(k/2,0,1)
    U[,2]<-runif(k/2,0,1) 
 
    #transform the uniform vars and inject matrix elements; turn into vector
    X[,1]<-sin(2*pi*U[,1])*sqrt(-2*log(U[,2]))
    X[,2]<-cos(2*pi*U[,1])*sqrt(-2*log(U[,2]))

    return(head(as.vector((X*sd)+mean),n))      
  }

  #Define Central Limit Theorem function with similar error traps and conditions

  clt<- function(n, mean=0, sd=1) {

    if (n < 0) stop("invalid arguments")
    if (is.complex(n)==TRUE) stop("invalid arguments")
    if (is.complex(mean)==TRUE) stop("invalid arguments")
    if (is.complex(sd)==TRUE) stop("invalid arguments")
    if (n == Inf) stop("invalid arguments")
    if (mean == Inf) stop("invalid arguments")
    if (sd == Inf) stop("invalid arguments")
    if (sd < 0) stop("invalid arguments")
    if (length(n) > 1) stop("invalid arguments")
    if (length(mean) > 1) stop("invalid arguments")
    if (length(sd) > 1) stop("invalid arguments")
    is.whole <- function(a) { floor(a)==a }
    if(is.whole(n)==FALSE) stop("invalid arguments") 
    
    #Define output matrix X and generate it by a transformation of a sum of 16 uniformly generated random variables. Output X as vector

    X<-matrix(0, ncol=1, nrow=n) 
    for(i in 1:n) { 
      U<-runif(16,0,1) 
      X[i,]<-(sum(U)-8)*sqrt(12/16)}
      return(as.vector((X*sd)+mean)) 
    }

#Finally, combine the three functions above into one

general.rnorm <- function(n, mean=0, sd=1, method=1){
  #Set conditions on the method argument, as the other arguments are covered already. Method 1 calls my.rnorm from before

  if(method==1) {my.rnorm(n, mean, sd)} else
  if(method==2){box(n, mean, sd)} else
  if(method==3){clt(n, mean, sd)} else stop("invalid arguments")
}


  
################################################################################################

#I understood this testing section not to mean testing of input traps/syntax errors/if functions add or transform like they're supposed to etc, 
#as this was done during the course of writing previous functions...
#and not testing for finite machine computational time, although this depends on the number of values the user wants generated
#which in any case this will depend on the hardware used. Rather, the testing here will concern how the functions perform as a pseudo-random value
#generator, assuming the uniform generator underlying the whole task is good enough. Hence, randomness of uniform samples will not be tested.

#This first test will investigate autocorrelation (as per reference http://www.mathcs.duq.edu/larget/math496/random.html) for level specified by the user, 
#default is five percent - seeing if there is dependence between elements in the preudo-random numbers X we generate. 
#Naturally, the bigger the sample size, the better the reliability of the figures.

autotest<-function(n, level=0.05, mean=0, sd=1, method=1){ 
  g<-general.rnorm(n, mean, sd, method) 
  if (cor(g[-1],g[-n])<level) {
  print("Pass")
}

#Second test is to eyeball the random values we generated (via histogram) and the mean, that they *look like* the normal distribution. 

visualtest<-function(n, mean=0, sd=1, method=1) {
  hist(general.rnorm(n, mean, sd, method) )
  return(mean(general.rnorm(n, mean, sd, method)))
}

#We can extend this to a more quantitative procedure, to investigate 'normality' using the Shapiro-Wilk 
#normality test (reference https://stat.ethz.ch/R-manual/R-devel/library/stats/html/shapiro.test.html) 
#for p-value greater than 0.05 we do not reject the null hypothesis that the data follow the normal distribution
#However, this does not imply that the data *are* normal.

shaptest<-function(n,mean=0,sd=1,method=1) {
  shapiro.test(general.rnorm(n, mean,sd,method))
}

#We can also check how our function general.rnorm compares with the built-in normal generator in R. Gives plot and correlation coeff
#of course, if we repeat these tests, we get different results due to randomness of the data

normcomparetest1<-function(n,mean=0,sd=1,method=1) {
  plot(rnorm(n)-general.rnorm(n, mean, sd, method))
}
normcomparetest2<-function(n,mean=0,sd=1, method=1) {
  cor(rnorm(n),general.rnorm(n, mean, sd, method))
}


#We can utilise some written R packages in order to help us investigate further the randomness of data
#Derived from the Diehard tests (reference http://stat.fsu.edu/pub/diehard/), there is a whole catalogue of tests for randomness.
#Let us try the runs test (reference page 13 of https://cran.r-project.org/web/packages/randtests/randtests.pdf)
#The runs test which again tests the IID assumption underlying the generated data.
#Runs test is explained well here (reference https://onlinecourses.science.psu.edu/stat414/book/export/html/233)
#For p-value less than 0.05 we can reject the null hypothesis of randomness in the data

install.packages("randtests")
library(randtests)
runstest<-function(n, mean=0,sd=1, method=1) {
  runs.test(general.rnorm(n ,mean, sd, method))
}


#A neat test from a different package is the Poker Test, which considers the frequency with which particular digits are repeated
#(reference http://www.iiserpune.ac.in/~cathale/lects/IDC102-2011/20110404randomno02.pdf)
#this is again an independence investigation.

install.packages("randtoolbox")
library(randtoolbox)
pokertest<-function(n,mean=0,sd=1, method=1) {
  poker.test(general.rnorm(n, mean,sd, method))
}
}