#I confrim that the attached is my own work, except where clearly indicated in the text


my.rnorm <- function(n,mean=0,sd=1) {                       #create the function my.rnorm with number of values to return (n), mean of values to return (with default 0) and sd of values to return (with default 1) as its arguments
  U <- runif(n)                                             #generate n random deviates from the Uniform Distribution and place them in a vector named U
  for (i in 1:n) {                                          #for each element in U
    U[i] <- (2*U[i])-1                                      #multiply by 2 and minus 1 to transfrom them into the range [-1,+1] (i.e the unit square)
  }
  w <- vector(length=(n/2))                                 #create a vector named w of length n/2
  for (i in 1:(n/2)) {                                      #for loop calculates the values of U[2*i-1]^2 + U[2*i]^2 for i from 1 to n/2 and places them in the vector w
    w[i] <- (U[2*i-1])^2 + (U[2*i])^2
  }
  for (i in 1:length(w)) {                                  #loop over each value in w
    while (w[i]>1) {                                        #while the element in position i is greater than 1
      U[2*i-1] <- runif(1); U[2*i] <- runif(1)              #the pair U[2*i-1], U[2*i] are rejected and a new pair are generated
      U[2*i-1] <- (2*U[2*i-1])-1; U[2*i] <- (2*U[2*i])-1    #this pair are then transformed to the unit square
      w[i] <- (U[2*i-1])^2 + (U[2*i])^2                     #and used to re-calculate the element w[i]
    }
  }                                                                                                                                                                                                                                                                              
  v <- vector(length=(n/2))                                 #create a vector named v of length n/2
  for (i in 1:length(v)) {                                  
    v[i] <- sqrt(-2*log(w[i])/w[i])                         #for loop calculates v=sqrt(-2log(w)/w) for each value in the vector w and places this in the corresponding position in the vector v
  }
  X <- vector(length=n)                                     #create a vector named X of length n
  for (i in 1:length(v)) {                                  #loop over each value in v
    X[2*i-1] <- U[2*i-1]*v[i]                               #the X values are calculated by multiplying the corresponding U value by the corresponding v value
    X[2*i] <- U[2*i]*v[i]                                   #and these values are placed into the vector X
  } 
  X <- X*sd +mean                                           #transform X values to have correct mean and standard deviation
  return(X)                                                 #deliver the vector X of pseudo-random values from a normal distribution
}





general.rnorm <- function(method=1,n,mean=0,sd=1) {                 #create the function general.rnorm with method used (with default 1), number of values to return (n), mean (with default 0) and sd (with default 1) as its arguments
  if (method==1) {                                                  #if the method used is the Marsaglia and Bray algorithm
    return(my.rnorm(n,mean,sd))                                     #then the function returns the result of the my.rnorm function
  }
  else if (method==2) {                                             #else if the method used is the Box and Mueller algorithm
    U <-runif(n)                                                    #generate n random deviates from the Uniform Distribution and place them in a vector, U
    X <- vector(length=n)                                           #create a vector X of length n
    for (i in 1:(n/2)) {                                            
      X[2*i-1] <- sin(2*pi*U[2*i-1])*sqrt(-2*log(U[2*i]))           #for loop replaces the elements in odd positions of X with this
      X[2*i] <- cos(2*pi*U[2*i-1])*sqrt(-2*log(U[2*i]))             #and the elements in even positions of X with this
    }
    return(X)                                                       #deliver the vector X of pseudo-random normally-distributed deviates
  }
  else if (method==3) {                                             #else if the method used is the central-limit theorem algorithm
    Y <- replicate(n, {                                             #replicate the following process n times
      U <- runif(16)                                                #create a vector called U which contains 16 Uniformally distributed random deviates
      X <- (sum(U)-8) *sqrt(12/16)                                  #calculate X by summing the elements in the vector U, subtracting 8, then multiplying by sqrt(12/16)
    })
    return(Y)                                                       #deliver the vector Y which contains n pseudo-random normally-distributed deviates
  }
}





#Is ouput a vector
vector.test <- function(method=1,n,mean=0,sd=1) {                    #define a function named vector.test whose arguments are the same as for the general.rnorm function
  return(is.vector(general.rnorm(method,n,mean,sd)))                 #if the output of general.rnorm is a vector then the test will return TRUE, else the test will return FALSE
}





#Shapiro-Wilk normality test
is.normal <- function(method=1,n,mean=0,sd=1) {                      #define a function named is.normal whose arguments are the same as for the function general.rnorm 
  y <- shapiro.test(general.rnorm(method,n,mean,sd))$p.value         #run a Shapiro_Wilk normality test on the random deviates generated by general.rnorm and assign y the p-value of this test
  if (y > 0.05) {                                                    #if the p-value is greater than our chosen alpha level 0.05
    return("We can assume that the data is Normal")                  #then the function will return "We can assume that the data is Normal"
  }
  else {                                                             #else if the p-value is smaller than our chosen alpha level 0.05
    return("We must reject the assumption that the data is Normal")  #the function will return "We must reject the assumption that the data is Normal"
  }
}





#Kolmogorov-Smirnov test
comparison.test <- function(method=1,n,mean=0,sd=1) {                       #define a function named comparison.test whose arguments are the same as for the function general.rnorm
  z <- ks.test(general.rnorm(method,n,mean,sd),rnorm(n,mean,sd))$p.value    #run a Kolmogorov-Smirnov test comparing the random deviates generated by the general.rnorm function to the ones generated by the rnorm function and assign z the p-value of this test
  if (z > 0.05) {                                                           #if the p-value is greater than 0.05
    return("We can assume that the data is Normal")                         #then the function will return "We can assume that the data is Normal
  }
  else {                                                                    #else if the p-value is smaller than 0.05
    return("The data is not from a Normal Distribution")                    #the function will return "The data is not from a Normal Distribution"
  }
}


#Test for randomness - NOTE TEST WORKS BEST FOR LARGE VALUES OF N AND MAY FAIL FOR SMALLER VALUES
randomness.test <- function(method=1,n,mean=0,sd=1) {                       #define a function named randomness.test whose arguments are the same as for the function general.rnorm
  random_deviates <- general.rnorm(method,n,mean,sd)                        #assign the vector random_deviates the random deviates generated by the general.rnorm function
  x<-random_deviates[1:(n/2)]                                               #assign the vector x the elements in the first half of the random_deviates vector
  y<-random_deviates[((n+2)/2):n]                                           #assign the vector y the elements in the second half of the random_deviates vector
  spearmans_rank <- cor(x,y,method="spearman")                              #calculate the spearmans rank correlation coefficient for x and y
  if (spearmans_rank<0.4) {                                                 #if spearmans rank is less than 0.4 (suggesting little correlation and implying randomness)
    return("Data appears random")                                           #the function returns "Data appears random"
  }
  else {                                                                    #else if spearmans rank is greater than 0.4
    return("Data does not appear random")                                   #the function returns "Data does not appear random"
  }
}
