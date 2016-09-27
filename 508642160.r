#I confirm that the attached is my own work, except where clearly indicated in the text.


my.rnorm <- function(n,mean=0,sd=1) {           #Defaults mean to 0 and sd to 1, no 
                                                #default for n so if no value is given
                                                #for n an error message is delivered
                                                #automatically.
  if (length(n) > 1){                           #Checks for more than one value of n,
    stop("invalid argument")                    #and delivers appropriate message. 
  }
  if (n != as.integer(n)) {                     #Checks that n is an integer, and
    stop("invalid arguments")                   #delivers appropriate message.
  }
  if (n < 1) {                                  #Checks that n >= 1, and delivers
    stop("invalid arguments")                   #appropriate message.
  }
  if (length(mean) != 1) {                      #Checks for more than one mean value
    stop("invalid arguments")                   #and delivers appropriate message.
  }
  if (length(sd) != 1) {                        #Check for more than one sd value
    stop("invalid arguments")                   #and delivers appropriate message.
  }
  if (is.numeric(mean) == FALSE) {              #Check that mean value is a number.
    stop("invalid arguments")
  }
  if (is.numeric(sd) == FALSE) {                #Check that sd value is a number.
    stop("invalid arguments")
  }
  if (sd < 0) {                                 #Check that sd i non-negative.
    stop("invalid arguments")
  }
  Xs <- array(data=0,dim=0)                     #0-array with 0 dimension made to
                                                #store the Xi values.
  i <- 0                                        #Set up of count for storing purposes.
  j <- 0                                        #Set up count to stop infinite loop.
  while (length(Xs) < n) {                      #While loop which stops as soon as
                                                #Xs has at least n elements, for odd
                                                #values of n this means there is an
                                                #extra Xi value, but this is
                                                #accounted for later.
    j <- j+1                                    #Count number of iterations.
    if (j == (100*n)) {                         #Halts an infinite/very large loop.
      return("ERROR: infinite loop likely")
    }
    U1 <- runif(1,min=0,max=1)                  #Generates random Ui~uniform(0,1) for
    U2 <- runif(1,min=0,max=1)                  #i=1,2
    U1 <- (2*U1)-1                                #Transform U1 and U2 to unit square.
    U2 <- (2*U2)-1
    w <- (U1^2)+(U2^2)                          #Calculate w.
    if (w <= 1) {                               #If statment only containing
                                                #instructions for values of w less
                                                #than or equal to 1, hence rejecting
                                                #any U1 and U2 such that w is greater
                                                #than 1 (rejection step).
      i <- i+1                                  #Count for the number of unrejected
                                                #U1 and U2 pairs, for storing purposes.
      v <- sqrt(-2*log(w)/w)                    #Calculating v.
      X1 <- U1*v                                #Calculating X1.
      X2 <- U2*v                                #Calculating X2.
      Xs[(2*i-1)] <- X1                         #Storing X1 and X2 values in the Xs
      Xs[(2*i)] <- X2                           #array using the count.
      }
    }
  Xs <- (Xs*sd)+mean                            #Transforming Xs values into values
                                                #with the appropriate mean and sd.
  if ((n/2) == as.integer(n/2)) {               #If statement for even n value, if n
                                                #is even then Xs will contain n
                                                #values.
    return(Xs)                                  #Return Xs array.
  } else {                                      #Else statment for odd n value, if n
                                                #is odd then there will be an extra Xi
                                                #in the Xs array due to the while
                                                #statement and pair nature of the
                                                #algorithm.
    del <- sample(1:(n+1),1)                    #Selecting a random Xi in Xs.
    Xs <- Xs[-del]                              #Deleting the randomly selected Xi.
    return(Xs)                                  #Return Xs array.
  } 
}




general.rnorm <- function(n,mean=0,sd=1,algorithm=1) {   #Defaults mean to 0, sd to 1
                                                #and algorithm to 1, no default for n
                                                #so if no value is given for n an
                                                #error message is delivered
                                                #automatically.
  if (length(n) != 1){                          #Same scalar checks carried out on n,
    stop("invalid arguments")                   #mean and sd as in my.rnorm.
  }
  if (n != as.integer(n)) {
    stop("invalid arguments")
  }
  if (n < 1) {
    stop("invalid arguments")
  }
  if (length(mean) != 1) {
    stop("invalid arguments")
  }
  if (length(sd) != 1) {
    stop("invalid arguments")
  }
  if (is.numeric(mean) == FALSE) {
    stop("invalid arguments")
  }
  if (is.numeric(sd) == FALSE) {
    stop("invalid arguments")
  }
  if (sd < 0) {
    stop("invalid arguments")
  }
  if (length(algorithm) != 1) {                 #Check that only one algorithm code
    stop("invalid arguments")                   #is given.
  }
  if (is.numeric(algorithm) == FALSE) {         #Check algorithm code given is a
    stop("invalid arguments")                   #number.
  }
  if (algorithm != as.integer(algorithm)) {     #Check that algorithm code is an
    stop("invalid arguments")                   #integer.
  }
  if (algorithm > 3) {                          #Check for incorrect values inputted
    stop("invalid arguments")                   #for the algorithm code, with
  }                                             #relevant message delivered.
  if (algorithm < 1) {
    stop("invalid arguments")
  }
  if (algorithm == 1) {                         #Calls my.rnorm function for the
    return(my.rnorm(n=n,mean=mean,sd=sd))       #algorithm code.
  }
  if (algorithm == 2) {                         #Using Box and Mueller algorithm.
    Xs <- array(data=0,dim=0)                   #Create a 0 array with 0 dimension to
                                                #store the Xi values.
    for (i in 1:((n+1)/2)) {                    #Setting up a for loop for n/2 times,
                                                #for an even n and (n+1)/2 times for
                                                #odd. As Xi values are produced in
                                                #pairs, there will be n Xi values for
                                                #an even n value and n+1 Xi values 
                                                #for an odd n value, which is
                                                #accounted for later.
      U1 <- runif(1,min=0,max=1)                #Generates random Ui~uniform(0,1) for
      U2 <- runif(1,min=0,max=1)                #i=1,2.
      X1 <- (sin(2*pi*U1))*sqrt(-2*log(U2))     #Calculates Xi for i=1,2
      X2 <- (cos(2*pi*U1))*sqrt(-2*log(U2))
      Xs[((i*2)-1)] <- X1                       #Stores X1 and X2 in the Xs array.
      Xs[(i*2)] <- X2
    }
    Xs <- (Xs*sd)+mean                          #Transforming Xs values into values
                                                #with the appropriate mean and sd.
    if ((n/2) == as.integer(n/2)) {             #If n is even then Xs is returned.
      return(Xs)
    } else {                                    #If n is odd then Xs contains n+1
                                                #elements and so a random element
                                                #must be removed.
      del <- sample(1:(n+1),1)                  #Select random element of Xs array.
      Xs <- Xs[-del]                            #Delete random element from Xs array.
      return(Xs)                                #Return new Xs array.
    }
  }
  if (algorithm == 3) {                         #Using Central-Limit Theorem algorithm.
    Xs <- array(data=0,dim=0)                   #Create a 0 array with 0 dimension to
                                                #store the Xi values.
    for (i in 1:n) {                            #Setting up a for loop for n reps.
      U <- c(runif(16,min=0,max=1))             #Generates an array containing random
                                                #Ui~uniform(0,1) for i=1,...,16.
      X <- (sum(U)-8)*sqrt(12/16)               #Calculate X.
      Xs[i] <- X                                #Stores X in Xs array.
    }
    Xs <- (Xs*sd)+mean                          #Transforms Xs values into values with
                                                #appropriate mean and sd.
    return(Xs)                                  #Return Xs array.
  }
}




#The following function tests a previously obtained and stored set of values, where
#x are those values, n is the length, mean is the mean and sd is the sd specified when
#generating x from my.rnorm/general.rnorm. As mean and sd and defaulted as 0 and 1
#respectively in my.rnorm and general.rnorm the same has been done here.
test.function <- function(x,n,mean=0,sd=1) {    
  pass.test.length <- length(x) == n                    #Test for correct number of
  print(c("Lenth test", pass.test.length))              #values having been delivered.
  pass.test.numeric <- is.numeric(x)                    #Test that numbers were
  print(c("Numeric test", pass.test.numeric))           #delivered obtaining x.
  #The following code produces a Q-Q plot for the x values, lack of deviation from
  #the line indicates that the x values are normally distributed.
  qqnorm(x)
  qqline(x)
  #The following code carries out a Kolmogorov-Smirnov test to a 5% significance
  #level to assess to normality of the x values, as a Q-Q plot might not always be
  #conclusive. First the values have to be transformed back to a N(0,1) distribution.
  #The null hypothesis is that the x values are normally distributed. The alternative
  #hypothesis is that the x values are not normally distributed.
  y <- (x-mean)/sd
  ks.test(y,pnorm)
  ks.test.p.value <- ks.test(y,pnorm)[2]
  if (ks.test.p.value < 0.975 & ks.test.p.value > 0.025) {
    print("Do not reject the null hypothesis that the x values are normally distributed, at a 5% significance level.")
  } else {
    print("Reject the null hypothesis that the x values are normally distributed, at a 5% significance level.")
  }
  #The following code is for an F-test for equality of variance, testing the x values
  #generated by my.rnorm/general.rnorm againist a hypothetical set of normally
  #distributed values of size n, with a sd equal to the sd specified when generating
  #x, where n is the n specified when generating x. The null hypothesis is that the
  #sd of the x values is equal to the sd specificed when generating x. The 
  #alternative hypothesis is that these two sd's are not equal.
  sd.f.test.stat <- (sd(x)^2)/(sd^2)
  sd.f.test.lower.bound <- qf(0.025,(n-1),(n-1))
  sd.f.test.upper.bound <- qf(0.975,(n-1),(n-1))
  if ( sd.f.test.stat < sd.f.test.upper.bound & sd.f.test.stat > sd.f.test.lower.bound) {
    print("Do not reject null hypothesis that the sd of the x values is equal to the specified sd used to generate x, at a 5% significance level.")
  } else {
    print("Reject null hypothesis that the sd of the x values is equal to the specified sd used to generate x, at a 5% significance level.")
  }
  #The following code is a t-test concerning the mean of x where the sd of the
  #distribution is unknown, so no assumptions about the sd of x are made in the test.
  #The null hypothesis is that the mean of the x values is equal to the mean speficied
  #when generating x. The alternative hypothesis is that these two means are not equal.
  mean.t.test.stat <- (mean(x)-mean)/(sd(x)/sqrt(n))
  mean.t.test.value <- qt(0.975,(n-1))
  if (mean.t.test.stat < 0) {
    mean.t.test.stat <- mean.t.test.stat * (-1)
  }
  if (mean.t.test.stat >= mean.t.test.value) {
    print("Reject null hypothesis that the mean of x is equal to the specified mean used to generate x, at a 5% significance level.")
  } else {
    print("Do not reject null hypothesis that the mean of x is equal to the specified mean used to generate x, at a 5% significance level.")
  }
}