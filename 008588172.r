#130012320 
#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n, mean=0, sd=1){ #we define my.rnorm as a function with default mean and standard deviations of 0 and 1 respectively
  
  if ((typeof(n))=="character" | (typeof(mean))=="character" | (typeof(sd))=="character" | abs(n - round(n)) > 0 | (sd) < (0) | length(sd) > 1 | length(n) > 1 | n < 1 | n > 10000 ) { #condition for ensuring integer values for all arguments (n, mean and sd), condition for ensuring non-negative standard deviations, condition for only one sd value (no lists or matrices), condition for one n value (no lists), condition for positive n values, condition for reasonable length of deviates list (computing power)
    stop ("invalid arguments")
  } else {
    o<-n  #define variable o the value of n, number of deviates we want
    if ((n %% 2) != 0) #is it odd, if so we make it even...
     o<-n+1  #we add one to make it even
  }
    udeviates <- rep(0,o)   #define a list called udeviates of length o with all elements initially 0
    wdeviates <- rep(0,(o/2)) #define another list called wdeviates of length o/2 (which is ok since o is even from previous commands!) with all elements initially 0
    for (i in 1:o){ 
      udeviates[i]<-(2*runif(1)-1) #put a random deviate from uniform (0,1) distribution in every place on the list udeviates and transform each random deviate to unit square
    }
    for (i in 1:(o/2)){
        wdeviates[i]<-((udeviates[2*i-1])^2+(udeviates[2*i])^2) #we calculate each w value for each pair of deviates on our list udeviates
    }      
    for (i in 1:(o/2)){  
        while (wdeviates[i]>1){ #since we want to use values for the pairs of deviates inside the unit circle, we check to make sure the w value corresponding to each pair of deviates is not greater than one
             udeviates[2*i-1]<-(2*runif(1)-1) #if w is greater than 1 then we generate a new pair of deviates that have been transformed to the unit square
             udeviates[2*i]<-(2*runif(1)-1) #the 2nd deviate in the pair is generated here
             wdeviates[i]<-((udeviates[2*i-1])^2+(udeviates[2*i])^2) #we recalculate w for the new pair and the process is repeated for the new pair in case they also fall outside the unit circle
        }
    }
    #we could print wdeviates to check that each w is less than one but our while loop ensures that this can never be the case
    vdeviates <- rep(0,(o/2))  #define a list called vdeviates of length o with all elements initially 0
    for (i in 1:(o/2)){
      vdeviates[i]<-sqrt(-2*((log(wdeviates[i])))/(wdeviates[i])) #we generate values of v for each w and associated pair of deviates as per the formula in our algorithm
    }
    rdeviates <- rep(0,(o)) #define a list called rdeviates of length o with all elements initially 0 to store our output random normally distributed deviates
    for (i in 1:(o/2)){
      rdeviates[2*i]<-(udeviates[2*i]*vdeviates[i]) #using the formula in our algorithm, we multiply our 1st initially created deviate in the i-th pair with its corresponding v value
      rdeviates[2*i-1]<-(udeviates[2*i-1]*vdeviates[i]) #using the formula in our algorithm, we multiply our 2nd initially created deviate in the i-th pair with its corresponding v value
    } 
    rdev<-((sd*rdeviates[1:n])+mean) #call desired out rdev
    return(rdev) #we return the first n elements of our list corresponding to the n requested at the start
 }

general.rnorm <- function(method, n, mean=0, sd=1) {
   if (!(method %in% c(1,2,3) )) {  #conditional for ensuring method=1,2 or 3
    stop ("invalid arguments")
 } else if ((typeof(n))=="character" | (typeof(mean))=="character" | (typeof(sd))=="character" | abs(n - round(n)) > 0 | (sd) < (0) | length(sd) > 1 | length(n) > 1 | n < 1 | n > 10000) {  #condition for ensuring integer values for all arguments (n, mean and sd), condition for ensuring non-negative standard deviations, condition for only one sd value (no lists or matrices), condition for one n value (no lists), condition for positive n values, condition for reasonable length of deviates list (computing power)
    stop ("invalid arguments")
 } else if (method==1) {               #if method is 1, equivalent to the Marsaglia and Bray method, we compute random deviates using our my.rnorm function
    my.rnorm(n, mean, sd)      
 } else if (method==2) {               #if method is 2, equivalent to the Box-Mueller method, we compute random deviates using the following algorithm
    o<-n      #define variable o the value of n, number of deviates we want
    if ((n %% 2) != 0) #is it odd, if so we make it even...
     o<-n+1  #we add 1 to it to make it even
    udeviates <- rep(0,o) #define a list called udeviates of length o with all elements initially 0
    xdeviates <- rep(0,o) #define a list called xdeviates of length o with all elements initially 0
    for (i in 1:o){
     udeviates[i]<-runif(1) #put a random deviates from uniform (0,1) distribution in every place in the list udeviates
    }
    for (i in 1:(o/2)){ 
     xdeviates[2*i-1] <- (sin(2*pi*udeviates[2*i-1])*sqrt((-2*log(udeviates[2*i])))) #according to the algorithm, we define the first value in each i-th pair of x values according the algorithm
     xdeviates[2*i] <- (cos(2*pi*udeviates[2*i-1])*sqrt((-2*log(udeviates[2*i])))) #according to the algorithm, we define the second value in each i-th pair of x values according the algorithm
    }
    return((sd*xdeviates[1:n])+mean) #we return the first n values of the list of random deviates corresponding to the n requested
   }
   else if (method==3) {  #if method is 3, equivalent to the Box-Mueller method, we use the central limit theorem 
    xdeviates <- rep(0,n)   #define a list called xdeviates of length m with all elements initially 0
    for (i in 1:n){   #we generate a list of 17 random deviates from uniform (0,1) distribution n times, i.e for each random deviate we require 
     udeviates <- rep(0,16) #define a list called udeviates of length 17 with all elements initially 0
     for (j in 1:16){
       udeviates[j]<-runif(1) #put random deviates from uniform (0,1) distribution in every place in the list udeviates
     }
     xdeviates[i]<-((sum(udeviates)-8)*sqrt(12/16)) #we calculate random deviate according to algorithm 
    }
    return((sd*xdeviates)+mean) #we return the list of random deviates 
   }
}  

testfunction.norm <- function() {
  matrix<- matrix(rep(0, 3000),3,1000) #create 3x1000 matrix
  typeoflist<-rep(0,3) #define a list called xdeviates of length m with all elements initially 0
  tests<-rep(0,18)
  j<-1
  for (m in 1:3) {
   tests[j]<-is.numeric(general.rnorm(n=10,method=m)) #make sure output of every test is numeric for (n=10) output in list is 1 if true and 0 if false 
   j<-j+1     #make sure next input in the list is entered in the next position               
   for (n in 1:5) {           #make sure for all n from 1 to 5, number of random deviates requested equals number of random deviates given
    tests[j]<-(length(general.rnorm(n,method=m))==n) #make sure number of deviates given in every test and for every n from 1 to 5 is equal to the n input. Output in list is 1 if true and 0 if false 
    j<-j+1    #make sure next input in the list is entered in the next position
   } #finish 1st for loop
  } #finish 2nd for loop
   print(tests) #all elements should be 1 (equivalent to TRUE)
   if (length(tests)==sum(tests)) { #tests to see that no elements equal 0, which would cause the sum to be less than the length
    print('all initial tests have passed') #prints confirmation
   }  #finish if statement
   
   means<-rep(0,3) #corresponds to list of the means
   sds<-rep(0,3) #corresponds to list of the sds
   CIs<-matrix(rep(0, 6),3,2) #corresponds to list of 95% confidence intervals of the means
   for (m in 1:3){   
    matrix[m,] <- general.rnorm(n=1000,method=m) #for each row, put in 1000 random deviates from each method
    means[m]<-mean(matrix[m,]) #corresponds to list of the means for each deviate list
    sds[m]<-sd(matrix[m,]) #standard deviation for each list of deviates
    CIs[m,1]<-(means[m]-((2*sds[m])/(sqrt(1000))))    #generate 95% lower confidence interval bound of the samples
    CIs[m,2]<-(means[m]+((2*sds[m])/(sqrt(1000))))    #generate 95% upper confidence interval bound of the samples
    #generate 95% confidence interval of the mean
   }

   if(0<(CIs[1,1])| 0>(CIs[1,2])) {  #we test to see if all confidence intervals contain mean 0.
     a<-('desired mean outside of sample confidence internval for method 1')
 } else {
     a<-('desired mean within sample confidence internval for method 1')
 }  

   if(0<(CIs[2,1])| 0>(CIs[2,2])) {
     b<-('desired mean outside of sample confidence internval for method 2')
 } else {
     b<-('desired mean within sample confidence internval for method 2')
 }  
   if(0<(CIs[3,1])| 0>(CIs[3,2])) {
     c<-('desired mean outside of sample confidence internval for method 3')
 } else {
     c<-('desired mean within sample confidence internval for method 3')
    #We looked at simple calculations but can use a formal test to see if key assumptions of randomness and normality hold
 }  #Kolmogorov Smirnov, Runs Test, Shapiro Wilks,...
   print(c(a,b,c)) #In most cases we see that method 1 and 3 do not contain the true mean in their confidence intervals.
  #We can start to test our functions using a variety of methods.
  #The first we want to use is a runs test for detecting non-randomness
  #This will flag two instances: either if like follows like or if there is a large tendency to alternate. Both would indicate a problem in our algorithm.
  #A high p-value would indicate the former and a low p-value the latter.
  #We use the median of our series as the cutting point
   install.packages("TSA") #install Time Series Analysis packages
   library(TSA) #add to our library
   print("p value of runs test for method 1,2 and 3:")
   print(c(runs(matrix[1,],median(matrix[1,]))$pvalue,runs(matrix[2,],median(matrix[2,]))$pvalue,runs(matrix[3,],median(matrix[3,]))$pvalue)) #conduct a runs test using the median as our cutting point
  #We see method 1 maintaining the least randomness (usually with a low p-value indicating a tendency for values in the sequence to alternate) but it is usually not statistically significant. It can also be easily solved by randomizing the sequence of numbers
  
  #We have now analysed the standard deviation and mean of our deviates, we should now look at two other characteristics including skewness and kurtosis. 
  #If we want to assume normality, we need no skewness (i.e. close to 0) to be present and kurtosis to be close to 0 as well.
   print("kurtosis of deviates from methods 1,2 and 3")
   print(c(kurtosis(matrix[1,]),kurtosis(matrix[2,]),kurtosis(matrix[3,])))
   print("skewness of deviates from methods 1,2 and 3")
   print(c(skewness(matrix[1,]),skewness(matrix[2,]),skewness(matrix[3,])))
   #We see that all are very near zero. This shows that the characteristics of our deviates are very similar to a normal distribution
  #We now want a more exact test with more power and complexity to test the most important aspect, the assumption of normality
  
  #We can firstly conduct a two-sample Kolmogorov-Smirnov test to compare the random deviates produced by rnorm and by each of the 3 methods. 
   
   ks.test(rnorm(1000),matrix[1,],alternative="tw")
   ks.test(rnorm(1000),matrix[2,],alternative="tw")
   ks.test(rnorm(1000),matrix[3,],alternative="tw")
  #Here we see that for all methods there is no evidence against the assumption of normality. Our p value is usually greater than 0.2 indicating no evidence.
   #skewness and kurtosis
  
  #We also conduct a Shapiro-Wilks test for normality 
   shapiro.test(matrix[1,])
   shapiro.test(matrix[2,])
   shapiro.test(matrix[3,])
  #Once again, we see no evidence against normality
   
  #We can also create qq plots. These are plots of the quantiles of each data set. By normality, we would expect to see a straight line which implies that the data is normally distributed.
   par(mfrow=c(2,2))
   qqnorm(matrix[1,])
   qqnorm(matrix[2,])
   qqnorm(matrix[3,])
  #From these plots we see Q-Q plots satisfying the assumptions of normality since there 
  #Overall we see that all 3 methods produce deviates which satisfy conditions of normality
}
   