# I confirm that the attached work is my own work, except where clearly indidcated in the text.

my.rnorm <- function(n, mean = 0, sd = 1) {
  if (length(n) > 1 | n <= 0 | is.character(n) == TRUE |                                    ## To ensure that 'n' is a scalar, non-negative,
      n != as.integer(n)) stop ("invalid arguments")                                        # numeric and integer value.
  if (length(mean) > 1 | is.character(mean) == TRUE) stop  ("invalid arguments")            ## To ensure that 'mean' is scalar and numeric.                                                                             
  if (length(sd) > 1 | sd < 0 | is.character(sd) == TRUE) stop ("invalid arguments")        ## To ensure 'sd' is scalar, non-negative and numeric.         
  else if (n %% 2 == 0) {                                                                   ## To check if 'n' is an even number.
    x <- matrix(nrow = n/2, ncol = 2)                                                       ## If yes - Creating a matrix in which results of the algorithm 
    for (i in 1:(n/2)) {                                                                    # will be stored; where the rows indicate number of iterations and 
      while(TRUE) {                                                                         # columns indicate the deviates X1 & X2 per iteration of the for-loop.
        u <- 2*(runif(2, 0, 1))-1                                                           ## Running the while-loop till 'w' meets the condition.
        w <- sum(u^2)
        if (w <= 1 ) break } 
      v <- sqrt((-2*log(w))/w)
      x[i,] <- u*v                                                                          ## Populating the matrix with normally distributed deviates X1 & X2. 
    }                                                                                       ## Converting the matrix containing pairs of normally distributed deviates
    x <- as.vector((x*sd) + mean)                                                           # into a vector after adjusting their 'mean' and 'sd' as required.
  } else {                                                                                  ## If 'n' is an odd value - creating a matrix with (n+1)/2 rows for                             
    x <- matrix(nrow = (n+1)/2, ncol = 2)                                                   # n+1 iterations of the for-loop.
    for (i in 1:((n+1)/2)) {                                                                ## Running the for and nested while-loop again, as above, in order to
      while(TRUE) {                                                                         # implement the Marsaglia & Bray's method of generating pairs of normally-
        u <- 2*(runif(2, 0, 1))-1                                                           # distributed deviates from pairs of uniformly-distributed deviates.
        w <- sum(u^2)
        if (w <= 1) break } 
      v <- sqrt((-2*log(w))/w)
      x[i,] <- u*v                                                                          ## Populating the matrix with (n+1)/2 pairs of normal deviates; which 
    }                                                                                       # leaves us in excess of one value of the the required number 'n'. 
    x <- (x*sd) + mean
    x <- (sample(as.vector(x), size = n))                                                   ## Rejecting one randomly selected value after converting the matrix into a vector.
  }                                                                                         
  return(x)
}



general.rnorm <- function(n, mean = 0, sd = 1, method = 1) {
  if (method != 1 & method != 2 & method != 3) stop ("invalid arguments")                   ## To ensure only 1, 2 & 3 are acceptable values for 'method'.
  if (length(n) > 1 | n <= 0 | is.character(n) == TRUE |                                    ## If-statements with stop() to ensure that values for 'n', 'mean' and 'sd' 
      n != as.integer(n)) stop ("invalid arguments")                                        # are acceptable.
  if (length(mean) > 1 | is.character(mean) == TRUE) stop  ("invalid arguments")                                                                                         
  if (length(sd) > 1 | sd < 0 | is.character(sd) == TRUE) stop ("invalid arguments")
  else if (method == 1) {                                                                   ## The algorithm to apply if 'method' = 1;
    if (n %% 2 == 0) {                                                                      # code and comments in this case: same as provided for 'my.rnorm'.
      x <- matrix(nrow = n/2, ncol = 2)
      for (i in 1:(n/2)) {
        while(TRUE) {
          u <- 2*(runif(2, 0, 1))-1
          w <- sum(u^2)
          if (w <= 1) break } 
        v <- sqrt((-2*log(w))/w)
        x[i,] <- u*v
      }
      x <- (x*sd) + mean
      x <- as.vector(x)
    } else {
      x <- matrix(nrow = (n+1)/2, ncol = 2)
      for (i in 1:((n+1)/2)) {
        while(TRUE) {
          u <- 2*(runif(2, 0, 1))-1
          w <- sum(u^2)
          if (w <= 1) break } 
        v <- sqrt((-2*log(w))/w)
        x[i,] <- u*v
      }
      x <- (x*sd) + mean
      x <- (sample(as.vector(x), size = n))
    }
  }
  else if (method == 2) {                                                             ## Implementation of the Box-Mueller algorithm, if 'method' = 2.
    if (n %% 2 == 0) {                                                                ## If 'n' is an even number, creating two vectors of length n/2 each: 
      x1 <- vector("numeric", length = n/2)                                           # one for storing deviates X1 and the other for X2.
      x2 <- vector("numeric", length = n/2)
      for (i in 1:(n/2)) {                                                            ## Setting up n/2 iterations as we will get a pair of values per iteration.
        u <- runif(2, 0, 1)                                                           ## Generating a pair of uniform random deviates u[1] & u[2].
        x1[i] <- sin(2*pi*u[1])*sqrt(-2*log(u[2]))                                    ## Converting them into normal random deviates X1 & X2 by applying the algorithm.
        x2[i] <- cos(2*pi*u[1])*sqrt(-2*log(u[2]))
      }
      x <- c(x1, x2)*sd + mean                                                        ## Concatenating X1 and X2 into single vector and adjusting their 'mean'
    } else {                                                                          # and 'sd', as required.
      x1 <- vector("numeric", length = (n+1)/2)                                       ## If 'n' is an odd number, will set up a loop to iterate (n+1)/2 times 
      x2 <- vector("numeric", length = (n+1)/2)                                       # to give us n+1 values.
      for (i in 1:((n+1)/2)) {                                                        ## The algorithm is applied, same as above.
        u <- runif(2, 0, 1)
        x1[i] <- sin(2*pi*u[1])*sqrt(-2*log(u[2]))
        x2[i] <- cos(2*pi*u[1])*sqrt(-2*log(u[2]))
      }
      x <- c(x1, x2)*sd + mean                                                        ## We now have a concatenated vector containing n+1 normal deviates with adjusted 'mean' & 'sd'.
      x <- (sample(x, size = n))                                                      ## Randomly rejecting one value, to get a vector of length n.
    }
  }
  else if (method == 3) {                                                             ## If 'method' = 3, will apply the Central Limit Theorem to generate
    x <- vector("numeric", length = n)                                                # and convert 16 uniform random deviates into one normal deviate.
    for (i in 1:n) {                                                                  ## Here, we will set up a loop to perform 'n' iterations of the algorithm
      u <- runif(16, 0, 1)                                                            # to give us 'n' normally distributed deviates.
      x[i] <- (sum(u)-8)*sqrt(3/4)
    }
    x <- (x*sd) + mean
  }
  return(x)
}



my.test <- function(x, mu=0) {                                                        ## This is a simple function which takes the arguments 'x', which is
  t <- (mean(x)-mu)/(sd(x)/sqrt(length(x)))                                           # a vector obtained using any of the functions above; and 'mu' (default value 0), 
  p <- 2*pt(-abs(t), df=length(x)-1)                                                  # is the mean of our null hypothesis.
  oldpars <- par(no.readonly = TRUE)                                                  ## We then go on to arrive at the test statistic and P-value of our distribution.
  par(mfrow = c(1,2))                                                                 ## This test function also splits the graphics window into two to provide a visual summary
  qqnorm(x)                                                                           # of the distribution of our deviates by (i) plotting them against "theoretical" 
  hist(x)                                                                             # normal quantiles to idicate their congruence; & (ii) producing a histogram of our deviates.
  print(list(class=class(x),vector=is.vector(x), mean=mean(x),                        # The test function also prints a named list of useful information for checking if our 
             "number of deviates"=length (x), "standard deviation"=sd(x),             # vector of deviates possesses properties that our functions were created to inject.
             "test statistic"=t, "p-value"=p))                                        ## On exit, the previous graphical parameters are restored.
  on.exit(par(oldpars))
  
}



my.test.df <- function(n, mean=0, sd=1, method=1) {                           ## This is a more advanced test function for deeper analysis of the properties
  mean.x <- vector("numeric", length = n)                                     # of our normally distributed deviates at different levels of 'n'.
  sd.x <- vector("numeric", length = n)                                       ## Here, argument 'n' indicates the maximum number of values we want to simulate, 
  t <- vector("numeric", length = n)                                          # using general.rnorm. 
  p <- vector("numeric", length = n)                                          ## The result of this function is a dataframe where each row indicates the number of 
  N <- vector("numeric", length = n)                                          # deviates produced from 1:n with variables like mean, sd, t statistic, P-value etc for that simulation.
  for (i in 1:n) {                                                            ## The summary() of the resultant dataframe helps us check if the all the above statistics
    x <- general.rnorm(n=i, mean=mean, sd=sd, method=method)                  # lie within in acceptable range and quantiles to ensure that general.rnorm is producing normally 
    N[i] <- length(x)                                                         # distributed deviates at all levels of 'n'. We can apply other graphical and tabular analyses;
    mean.x[i] <- mean(x)                                                      # for example, boxplot of P-values, plot of 'N' against 'mean.x', etc. The possibilities
    sd.x[i] <- sd(x)                                                          # are immense.
    t[i] <- (mean(x)-mean)/(sd(x)/sqrt(length(x)))                            ## It took about 1.5 soconds to run this test function for n=1000 simulations on my computer.
    p[i] <- 2*pt(-abs(t[i]), df=length(x)-1)
  }
  df.x <- data.frame(N, mean.x, sd.x, t, p)
  return(df.x)
}