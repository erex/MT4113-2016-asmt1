#Assignment 1 MT4113

my.rnorm <- function(n, mean = 0, sd = 1){
  #
  #Purpose: Function to return a vector of pseudo-random values from a normal distribution
  #         using the Marsaglia and Bray algorithm
  #Arguments: n - number of values to return
  #           mean - mean of values to return (default 0)
  #           sd - standard deviation of values to return (default 1)
  #Output: vector length n of pairs of pseudo-random values from N(0,1)
  #
  #Error Checking: 1. checking n is not a vector, just a scalar, no negative n
  if (length(n)>1) { 
    stop("Invalid Argument")
  }
  if(n<0) {
  	stop("Invalid Argument")
  }
  #for n odd, change to n->n+1(caculate even pairs and then delete last value in the returning vector)
  if (n%%2 != 0){
    number= n+1
  } else {
    number = n 
  }
  #initalizing vector to retun at end of function 
  normal.dev.pairs <- NULL
  pairs <- number / 2 #get the number of how many times you need to run the algorithm
  
  for(i in 1:pairs){
      repeat{ #repeat statement simpler that while loop
        u <- runif(2)
        for (i in 1:2){
          u[i] <- 2*u[i] - 1
        }
        w <- (u[1])^2 + (u[2])^2
        if (w<=1) { 
          break #condition is met so exit the repeat loop
        }
      }
      v <- sqrt((-2*log(w))/w)
      x<- NULL
      for (i in 1:2){
        x[i] <- u[i]*v
      }
      normal.dev.pairs<-append(normal.dev.pairs, x) #adding PAIR of random normal deviates to previous list
  }
  #transforming for mean and sd (if not 0 or 1)
  normal.dev.pairs<-(normal.dev.pairs)*sd + mean
  #chopping of last value if n is odd
  if (n%%2 != 0){
      return(normal.dev.pairs[1:n]) 
  } else {
      return(normal.dev.pairs) 
  } #returns a vector of pseudo random normal deviates length n
}

general.rnorm <- function(n, mean = 0, sd = 1, method = 1) {
  #
  #Purpose: Function to return a vector of pseudo-random values from a normal distribution
  #         with user choice of which method to use.
  #         Default value for method is marsaglia bray algorithm if nothing is specified
  #
  #Arguments: n - number of values to return
  #           x - which method to use( 1= marsaglia and bray, 2= box and mueller
  #               3= central-limit theorem) Default  = 1
  #           mean - mean of values to return (default 0)
  #           sd - standard deviation of values to return (default 1)
  #Output: vector length n of pairs of pseudo-random values from N(0,1)
  #
  #Error Checking: checking n is not a vector, just a scalar; no negative n's
  
  if (length(n)>1) { 
    stop("Invalid Argument")
  }
  if(n<0) {
  	stop("Invalid Argument")
  }
  #for n odd, change to n->n+1(caculate even pairs and then delete last value in vector)
  if (n%%2 != 0){
    number= n+1
  } else {
    number = n 
  }
  
  #initalizing vector to retun at end of function 
  random.pairs <- NULL
  pairs <- number / 2 #get the number of how many times you need to run the algorithm
                      #not used for CLT method since return single variable
  
  if (method == 1) {
      random.pairs <- my.rnorm(n,mean,sd) #call the my.rnorm function
  } else if (method == 2) {
     for(i in 1:pairs){ #box.mueller method:
       u<-runif(2) # 2 random unif deviates
       x<-NULL
       x[1] <- ((sin(2*pi*u[1]))*(sqrt(-2*log(u[2]))))
       x[2] <- ((cos(2*pi*u[1]))*(sqrt(-2*log(u[2]))))
       random.pairs<-append(random.pairs,x) #adding PAIR of random normal deviates to previous list of them
      }
  } else if (method == 3) {
      for(i in 1:number){ #not pairs in loop since central limit only returns 1 variable, using number not n, incase n=odd
        u<-runif(16) #16 random unif deviates
        sum.unif<-0
        for (i in 1:16){
          sum.unif<-sum.unif + u[i] #summation step in theorem
        }
        x <- (sum.unif-8)*(sqrt(12/16))
        random.pairs<-c(random.pairs, x) # adding ONE random normal deviate to previous list
      }
  } else {
      stop("Invalid Argument") #user imputed method # not equal to Null, 1,2,3
  } 
  #transforming for mean and sd (if not 0 or 1)
  random.pairs<-(random.pairs)*sd + mean
  #chopping of last value if n is odd
  if (n%%2 != 0){
    return(random.pairs[1:n]) 
  } else {
    return(random.pairs) 
  }
  # returns vector of random normal pairs, length n
}


#----------------------------------
# Testing Functions to make sure code works
#----------------------------------

test.function.1 <- function(x) {
  #
  #Purpose: to test general.rnorm and my.rnorm works by looking at 
  #         the Q-Q plot and histogram to see if normality holds
  #             - holds if Q-Q linear and histogram bell shaped
  #Input: vector of supposed pseudo-random normal deviates
  #
  old <- par(mfrow=c(1, 2)) #to display 2 graphs on same screen
  hist(x, main="Histogram")
  qqnorm(x, main="Q-Q Plot")
  par(old)
}

test.function.2 <- function(x, mean =0, sd =1){
  #
  #Purpose: To test general.rnorm and my.rnorm works by using a
  #         Kolomogrov-Smirnoff Test to see if the vector of random numbers follows
  #         a normal distribution
  #Input: Vector of supposed pseudo-random normal deviates
  #       Mean (default 0) if changing the mean in my.rnorm,general.rnorm
  #       sd (default 1) if changing sd in "  "
  # if pvalue < 0.05 then reject the null Hypothesis that the variables follow a 
  #   Normal Distibution
  result<- ks.test(x,'pnorm',mean,sd,alternative="two.sided")
  if (result$p.value >0.05) {
    print('data follows a Normal distribution')
    cat('p-value: ', result$p.value)
  }
  else {
    print ('data doesnt follow a Normal distribution')
    cat('p-value: ', result$p.value)
  }
}


test.function.3 <- function(x){
  #
  #Purpose: To test general.rnorm and my.rnorm works by using a shapiro-wilk test 
  #         of normality
  #
  #Input: Vector of supposed pseudo-random normal deviates
  #
  result<-shapiro.test(x)
  if (result$p.value >0.05) {
    print('data follows a Normal distribution')
    cat('p-value: ', result$p.value)
  }
  else {
    print ('data doesnt follow a Normal distribution')
    cat('p-value: ', result$p.value)
  }
}

