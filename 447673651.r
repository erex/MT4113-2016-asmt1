# I confirm that the attached is my own work, except where clearly indicated in the text.

##---------------------------------------------------------------------------------------------------------------------##
##################################### ASSIGNMENT 1:  MT4113-Computing in Statistics #####################################
##---------------------------------------------------------------------------------------------------------------------##
## Part 1:  Write R function, my.rnorm that reutns a vector of pseudo-random values from a normal distribution using   ##
##          the Marsaglia and Bray method (Maindonald, 1984). Just like the R function rnorm, the function should      ##
##          have the following arguments:                                                                              ##
##            n = number of values to return - no default                                                              ##
##            mean = mean of values to return - default 0                                                              ##
##            sd = standard deviation of values to return - default 1                                                  ##
##---------------------------------------------------------------------------------------------------------------------##

my.rnorm <- function(n=n,mean=mean,sd=sd) { 
  if(!require("schoolmath")){                      # This package will deliver function is.odd().This function will check
    install.packages("schoolmath")                 # if "n" (number of Values to return) is odd (TRUE) or even (FALSE). 
    library(schoolmath)
  }

##-----------------------------------------------## This part will check the Input.
  if(is.negative(n) == TRUE) {                    # If "n" is a negative, decimal, NA or not a numeric number, there will  
    stop("invalid arguments")                     # be an error message.
  }
  if(round(n) != n) {                             
    stop("invalid arguments")                     
  }
  if(is.numeric(n) == FALSE) {                    
    stop("invalid arguments")
  }
  if(is.na(n) == TRUE) {
    stop("invalid arguments")
  }

  if(missing(mean) == T) {                        # If there is no given "mean", it will be set to 0.
    mean <- 0
  }
  
  if(missing(sd) == T) {                          # If there is no given "sd", it will be set to 1.
    sd <- 1
  }
  
##-----------------------------------------------## This part contains the Marsaglia & Bray algorithm.
  if (is.odd(n) == FALSE ){                       # This is used to check for odd and even numbers in "n".
    N <- n/2;   RES <- matrix( , nrow= N, ncol=2) # If "n" is not odd, a matrix will be created with rows for each 
                                                  # pair (N) of (to be produced) deviates.
                                                  
    for (k in 1:N) {                              # Loop to create a pair of deviates for each N.
      U <- runif(2,0,1)                           # A pair of variates is chosen from uniform distribution
      U[1] <- 2*U[1]-1                            # and transformed into unit square.
      U[2] <- 2*U[2]-1
      w <- sum(U^2)                               # "w" is calculated for every pair.
      
      while (w > 1) {                             # While loop to choose deviates until criterion ist met:
        U <- runif(2,0,1)                         # "w" is used as an indicator in a rejection step. If "w" is 
        U[1] <- 2*U[1]-1                          # greater 1, a new pair of variates is chosen and "w" is calculated.
        U[2] <- 2*U[2]-1                          # This procedure will carry on until "w" is smaller or equal to 1.
        w <- sum(U^2)                             # If this criterion is met, the code will continue in the next step.
      } 
      
      v <- sqrt((-2)*log(w)/w)                    # "v" will be defined.
      
      X1 <- U[1]*v                                # and used to define normally distributed deviates for both variates
      X2 <- U[2]*v                                # in each pair.
      RES[k,1] <- X1                              # Save the normally distributed deviates in a specified row of the
      RES[k,2] <- X2                              # RES-matrix created before.
    }

  } else if (is.odd(n) == TRUE) {                 # If the number of values to return is odd, a matrix will be created 
    N <- (n+1)/2                                  # with rows for each pair of (to be produced) deviates - including 
    RES <- matrix( , nrow= N, ncol=2)             # one additional deviate, since they come in pairs and therefore have 
                                                  # to be even in number.
    
    for (k in 1:N) {                              # Loop follows same procedure as the one above and saves the result 
      U <- runif(2,0,1)                           # into RES-martix created before.
      U[1] <- 2*U[1]-1                            
      U[2] <- 2*U[2]-1
      w <- sum(U^2)
      
      while (w > 1) {
        U <- runif(2,0,1)
        U[1] <- 2*U[1]-1
        U[2] <- 2*U[2]-1
        w <- sum(U^2)
      } 
      
      v <- sqrt((-2)*log(w)/w)
      
        X1 <- U[1]*v
        X2 <- U[2]*v
        RES[k,1] <- X1
        RES[k,2] <- X2
    }
  } else {                                        # If "n" has not met any condition stated above, an error
    stop("invalid arguments")                     # message will be returned.
  }
  
##-----------------------------------------------## This part will create & costumize the Output.
  RESV <- as.vector(RES)                          # The created RES-Matrix will be converted into a vector.
  
  for (i in 1:length(RESV)) {                     # The values contained in that vector can be transformed into 
    RESV[i] <- RESV[i] * sd + mean                # specified mean and sd. If nothing is stated the default mean is 0
  }                                               # and the default sd is 1.
  
  if (is.odd(n)==FALSE) {                         # If "n" is a even number, the whole vector will be shown as output.
    print(RESV)       
  } else {                                        # If "n" is a odd number, the vector, without the last number, 
    print(RESV[-1])                               # will be shown.
  }
}

##---------------------------------------------------------------------------------------------------------------------##
## Part 2:  Write an R function general.rnorm returning a vector of pseurdo-randomly normally distributed deviates.    ##
##          This function will have one additional argument following the arguments to my.norm, namely                 ##
##            n = number of values to return - no default                                                              ##
##            mean = mean of values to return - default 0                                                              ##
##            sd = standard deviation of values to return - default 1                                                  ##
##    and     method = integer with value                                                                              ##
##              -1 corresponding to Marsaglia and Bray algorithm                                                       ##
##              -2 corresponding to Box and Mueller algorithm                                                          ##
##              -3 corresponding to central-limit theorem algorithm                                                    ##
##              default value for method is Marsaglia and Bray algorithm (1).                                          ##
##---------------------------------------------------------------------------------------------------------------------##

general.rnorm <- function(n=n, mean=mean, sd=sd, method=method) {
  if(!require("schoolmath")){                      # This package will deliver function is.odd().This function will check
    install.packages("schoolmath")                 # if "n" (number of Values to return) is odd (TRUE) or even (FALSE). 
    library(schoolmath)
  }

################################################### This part will check the Input.
  if(is.negative(n) == TRUE) {                    # If "n" is a negative, decimal, NA or not a numeric number, there will 
    stop("invalid arguments")                     # be an error message.
  }
  if(round(n) != n) {                             
    stop("invalid arguments")                     
  }
  if(is.numeric(n) == FALSE) {                    
    stop("invalid arguments")
  }
  if(is.na(n) == TRUE) {
    stop("invalid arguments")
  }
  
  if(missing(mean) == T) {                        # If there is no given "mean", it will be set to 0.
    mean <- 0
  }
  
  if(missing(sd) == T) {                          # If there is no given "sd", it will be set to 1.
    sd <- 1
  }
  
  if(missing(method) == T) {                      # If there is no given "method", it will be set to 1.
    method <- 1
  }
  
  if(method == 1) {                               # If the method is not 1, 2 or 3, it will be set to 1.            
    method <- 1
  } else if (method == 2) {
    method <- 2
  } else if (method == 3) {
    method <- 3
  } else { 
    method <- 1}

################################################### This part contains the three algorithms used in this function.
##-----------------------------------------------## Marsaglia and Bray's method (Maindonald, 1984)
  if (method == 1) {                            
    print("Used method: Marsaglia and Bray")      # Show in Output which method is used.
    if (is.odd(n) == FALSE ){                     # This is used to check for odd and even numbers in "n".
    N <- n/2;   RES <- matrix( , nrow= N, ncol=2) # If "n" is not odd, a matrix will be created with rows for each 
                                                  # pair (N) of (to be produced) deviates.
    
    for (k in 1:N) {                              # Loop to create a pair of deviates for each N.
      U <- runif(2,0,1)                           # A pair of variates is chosen from uniform distribution
      U[1] <- 2*U[1]-1                            # and transformed into unit square.
      U[2] <- 2*U[2]-1
      w <- sum(U^2)                               # "w" is calculated for every pair.
      
      while (w > 1) {                             # While loop to choose deviates until criterion ist met:
        U <- runif(2,0,1)                         # "w" is used as an indicator in a rejection step. If "w" is 
        U[1] <- 2*U[1]-1                          # greater 1, a new pair of variates is chosen and "w" is calculated.
        U[2] <- 2*U[2]-1                          # This procedure will carry on until "w" is smaller or equal to 1.
        w <- sum(U^2)                             # If this criterion is met, the code will continue in the next step.
      } 
      
      v <- sqrt((-2)*log(w)/w)                    # "v" will be defined.
      
      X1 <- U[1]*v                                # and used to define normally distributed deviates for both variates
      X2 <- U[2]*v                                # in each pair.
      RES[k,1] <- X1                              # Save the normally distributed deviates in a specified row of the
      RES[k,2] <- X2                              # RES-matrix created before.
    }
      
  } else if (is.odd(n) == TRUE) {                 # If the number of values to return is odd, a matrix will be created 
    N <- (n+1)/2                                  # with rows for each pair of (to be produced) deviates - including 
    RES <- matrix( , nrow= N, ncol=2)             # one additional deviate, since they come in pairs and therefore have 
                                                  # to be even in number.
      
    for (k in 1:N) {                              # Loop follows same procedure as the one above and saves the result 
      U <- runif(2,0,1)                           # into RES-martix created before.
      U[1] <- 2*U[1]-1                            
      U[2] <- 2*U[2]-1
      w <- sum(U^2)
        
      while (w > 1) {
        U <- runif(2,0,1)
        U[1] <- 2*U[1]-1
        U[2] <- 2*U[2]-1
        w <- sum(U^2)
      } 
        
        v <- sqrt((-2)*log(w)/w)
        
        X1 <- U[1]*v
        X2 <- U[2]*v
        RES[k,1] <- X1
        RES[k,2] <- X2
    }

  } else {                                        # If "n" has not met any condition stated above, an error
    stop("invalid arguments")                     # message will be returned.
  }                                               # End of Loop.
    
    
  RESV <- as.vector(RES)                          # The created RES-Matrix will be converted into a vector.
    
  if (is.odd(n)==FALSE) {                         # If "n" is a even number the whole vector will be shown as output.
    RESV <- RESV       
  } else {                                        # If "n" is a odd number the vector, without the last number, 
    RESV <- RESV[-1]                              # will be shown.
 }

##-----------------------------------------------## Box and Mueller algortihm (Jones et al., 2009)  
  } else if (method == 2) {        
    print("Used method: Box and Mueller")         # Show in Output which method is used.
    
    if (is.odd(n) == FALSE ){                     
    N <- n/2;   RES <- matrix( , nrow= N, ncol=2) # If "n" is not odd, a matrix will be created with rows for each 
                                                  # pair (N) of (to be produced) deviates.
     for (k in 1:N) {
      U <- runif(2,0,1)                           # Generate pair of uniformly distributed random deviates
      U[1] <- sin(2*pi*U[1]*sqrt(-2*log(U[2])))   # and transform them into polar coordinates. 
      U[2] <- cos(2*pi*U[1]*sqrt(-2*log(U[2])))       
      RES[k,1] <- U[1]                            # Save the normally distributed deviates in a specified row of the
      RES[k,2] <- U[2]                            # RES-matrix created before.
    }
    
    } else if (is.odd(n) == TRUE){                # If the number of values to return is odd, a matrix will be created 
      N <- (n+1)/2                                # with rows for each pair of (to be produced) deviates - including one 
      RES <- matrix( , nrow= N, ncol=2)           # additional deviate, since they come in pairs and therefore have to be 
                                                  # even in number.
      for (k in 1:N) {                           
        U <- runif(2,0,1)                         # Generate pair of uniformly distributed random deviates
        U[1] <- sin(2*pi*U[1]*sqrt(-2*log(U[2]))) # and transform them into polar coordinates. 
        U[2] <- cos(2*pi*U[1]*sqrt(-2*log(U[2])))       
        RES[k,1] <- U[1]                          # Save the normally distributed deviates in a specified row of the
        RES[k,2] <- U[2]                          # RES-matrix created before.
      }
      
    } else {                                      # If "n" has not met any condition stated above, an error
      stop("invalid arguments")                   # message will be returned.
    }                                             # End of Loop.
    
  RESV <- as.vector(RES)                          # The created RES-Matrix will be converted into a vector.
  
  if(is.odd(n)==FALSE) {                          # If "n" is a even number the whole vector will be shown as output,
    RESV <- RESV                                  # otherwise the last number in the vector will be deleted to later
   } else {                                       # show exactly n numbers in the Output.
    RESV <- RESV[-1]
  }
    
##-----------------------------------------------## Central limit theorem (Mood et al., 1974)  
  } else if (method == 3) {                       
    print("Used method: Central limit theorem")   # Show in Output which method is used.
    RES <- matrix( , nrow= n, ncol=1)
    for (k in 1:n) {                              # For loop to choose "n" times from sufficient large number (here 16) 
      U <- runif(16,0,1)                          # from underlying distribution (here uniform distribution).
      X <- (sum(U)-8)*sqrt(12/16)                 # Create mean of chosen independent random variables. 
      RES[k,1] <- X
    }
    
  RESV <- as.vector(RES)                          # The created RES-Matrix will be converted into a vector.
  }
  
################################################### This part will create & costumize the Output.
  for (i in 1:length(RESV)) {                     # The values contained in that vector can be transformed into 
    RESV[i] <- RESV[i] * sd + mean                # specified mean and sd. If nothing is stated the default mean is 0
  }                                               # and the default sd is 1.
  
  print(RESV)                                     # Show results.
}

##---------------------------------------------------------------------------------------------------------------------##
## Part 3: Check my.rnorm() & general.rnorm() with testing function.                                                   ##                                                                                          ##
##---------------------------------------------------------------------------------------------------------------------##
# I used the "R Cookbook" by Teetor (2011) and "Discovering Statistics using R" by Field et al. (2012) for further help.

result <- my.rnorm(10)                           # Save result before using the testfunction!
#result <- general.rnorm(100)

## Test 1 ##
# Check if the output from my function compares to the output of rnorm() with mean=0, sd=1 and n= as in my function.
# Input:   Result of my.rnorm() or general.rnorm() and mean and sd used in the function (as input for rnorm() function)
# Output:  Graphical comparison via Histrogram
# Interpretation: If the distribution of the my function is close to the one from rnorm(), my deviates are very likely
#                 to be normally distributed. To make a good comparison a larger number of deviates is needed!

pass.test.rnorm <- function(result=result, mean=mean, sd=sd) { 
  if(!require("ggplot2")){                        # Ggplot2 gives many opportunities to upgrade plots & make them
    install.packages("ggplot2")                   # look nice. I used the R Graphics Cookbook (Chang, W.,2013) for help. 
    library(ggplot2)
  }
  
  rnorm <- rnorm(length(result),mean,sd)              # Create as many deviates from rnorm() as we did in my function.
  mynorm <- result                                # Rename my result.
  deviates <- c(rnorm,mynorm)                     # Put all deviates in one vector - beginning with those from rnorm()
  algorithm <- c(rep("rnorm",length(result)), rep("mynorm", length(result))) # and create another vector with names.
  DATA <- cbind(deviates,algorithm)               # Use both vectors to create a matrix, which I can use for ggplot().
  
  plot <- ggplot(NULL, aes(x=deviates, fill=algorithm)) +     # Create a Plot with all deviates, differentiated by the
    geom_histogram(position="identity", alpha=0.5, bins=30)   # function used (algorithm). A histogramm for each of the 
                                                              # samples will be drawn in one plot. 
  
  return(plot)                                    # Return this plot. 
}

pass.test.rnorm(result,30,20)

## Test 2 ##
# Check visually by showing the data in several plots: Histogram with density line, QQPlot & Boxplot.
# Input:   Result of my.rnorm() or general.rnorm()
# Output:  Three plots (Histogram (density), QQPlot & Boxplot)
# Interpretation: Histogramm - If my deviates are normally distributed, the histogram should be close to a normal 
#                              distribution (symmetric & with expected mean and sd).
#                 Boxplot    - Boxplot should be symmetric as well. 
#                 QQ-Plot    - If my deviates are normally distributed, x and y axis should not differ. Meaning they 
#                              should form a line. 
#                 To make a good interpretation a larger number of deviates is needed!

pass.test.plot <- function(result=result) {
  if(!require("ggplot2")){                        # Ggplot2 gives many opportunities to upgrade plots & make them
    install.packages("ggplot2")                   # look nice. I used the R Graphics Cookbook (Chang, W.,2013) for help. 
    library(ggplot2)
  }
##-----------------------------------------------## This part creates a function to plot multiple plots using ggplot.
                                                  # Since par(mfrow=c(2,2)) is not working I chose to use the function
                                                  # multiplot() provided by Winston Chung (2016). For closer look on
                                                  # this please see references.
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
        if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots==1) {
      print(plots[[1]])
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }

##-----------------------------------------------## This part is for creating plots!
  p1 <- ggplot(NULL, aes(x=result)) +             # Create Histogram with density line from given deviates.
    geom_histogram(aes(y =..density..), colour="black", fill="grey", bins=30) +
    ggtitle("Densityplot of my.rnorm") + xlab("Deviates from my.rnorm") +
    stat_function(fun=dnorm, args=list(mean=mean(result),sd=sd(result)), colour="orange", size=1)
  
  p2 <- ggplot(NULL, aes(x=1, y=result)) +        # Create Boxplot from given deviates.
    geom_boxplot(colour="black", fill="grey") +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    ggtitle("Boxplot of my.rnorm") + xlab("Deviates from my.rnorm")
  
  p3 <- qplot(sample = result) +                  # Create QQ-Plot from given deviates.
    ggtitle("QQ-plot of my.rnorm") + xlab("Deviates from my.rnorm") 
  
  plot <- multiplot(p1, p2, p3, cols=2)           # Use multiplot() function to display all plots at the same time.
  return(plot)
}

pass.test.plot(result)


## Test 3 ##
# Check how much my parameters differ from those from normal distribution.
# Input:  Result of my.rnorm() or general.rnorm(), mean and sd (used for rnorm() function - if empty it will set to N[0,1])
# Output: Mean, Standard deviation, Median, IQR, 1st Intervall, 2nd Intervall, 3rd Intervall of my function AND
#         the rnorm() function with mean=0 & sd=1. 
# Interpretation:  Use the tables to compare the parameters between the samples and the given parameters of a standard  
#                  normal distribution. 

pass.test.parameter <- function(result, mean=mean, sd=sd) {
  if(missing(mean) == T) {                        # If there is no given "mean", it will be set to 0.
    mean <- 0
  }
  
  if(missing(sd) == T) {                          # If there is no given "sd", it will be set to 1.
    sd <- 1
  }
  
  OURD <- result                                  # Save the results of my function and the rnorm function.
  ND <- rnorm(length(result),mean,sd)             # The number of deviates from rnorm equals the number from my function.
  RES <- matrix( , nrow= 5, ncol=4)               # Create a matrix where I can put my results in.
                                                  # Specifying each column and row to make the comparison easier.

  RES[1,1] <- "FUN";              RES[1,2] <- "my.norm"         ; RES[1,3] <- "r.norm"  ; RES[1,4] <- "known parameter for standard ND"
  RES[2,1] <- "mean";             RES[2,2] <-  mean(OURD)       ; RES[2,3] <- mean(ND)  ; RES[2,4] <- "0"
  RES[3,1] <- "sd";               RES[3,2] <-  sd(OURD)         ; RES[3,3] <- sd(ND)    ; RES[3,4] <- "1"
  RES[4,1] <- "IQR";              RES[4,2] <-  IQR(OURD)        ; RES[4,3] <- IQR(ND)   ; RES[4,4] <- "1.34"
  RES[5,1] <- "median";           RES[5,2] <-  median(OURD)     ; RES[5,3] <- median(ND); RES[5,4] <- "0"

  r.norm <- quantile(ND)                          # There are too many argments deliverd with quantile(). That is
  my.norm <- quantile(OURD)                       # why we create a second Matrix to compare the Quantiles as well.
  quantile <- cbind(my.norm,r.norm)
  
 return(list(first= RES, second= quantile ))      # Output will be one matrix to compare: mean, sd, IQR and median 
                                                  # and one for the quantiles.
}

pass.test.parameter(result)


## Test 4 ##
# Check by testing the sample for normality.
# Input:  Result of my.rnorm() or general.rnorm(), length of result must be between 3 and 5000
# Output: p-Value of tests (Shapiro-Wilk Normality test, Anderson-Darling test, Cramer-von Mises test, Lilliefors test,
#         Person chi-squared test and Shapiro-Francia test)
# Interpretation:  If p-value is > .05 then the H0 (sample is based on normal distribution) is assumed otherwise the
#                  the H1 is assumed.

pass.test.p <- function(result=result) {  
  if(!require("nortest")){                             # "nortest" package possesses most of the test functions.
    install.packages("nortest")                   
    library(nortest)
  }
  
  a <- shapiro.test(result)                            # Shapiro-Wilk normality test: "n" must be between 3 & 5000
  b <- ad.test(result)                                 # Anderson-Darling test: "n" must be greater than 7
  c <- cvm.test(result)                                # Cramer-von Mises test: "n" must be greater than 7
  d <- lillie.test(result)                             # Lilliefors test: "n" must be greater than 4
  e <- pearson.test(result)                            # Pearson chi-squared test: not well suited!
  f <- sf.test(result)                                 # Shapiro-Francia test: "n" must be between 5 and 5000
  return(list(Shapiro=a, Anderson= b, Cramer= c, Lilliefors=d, Pearson= e, Francia= f)) 
}

pass.test.p(result)


## Test 5 ##
# Check 
# Input:  Result of my.rnorm() or general.rnorm()
# Output: pass.test takes on the value TRUE if the object 'x' conatins 10
# Interpretation:   If "n" in my function is 10 and numeric the Output will be TRUE.

pass.test <- (length(result)==10 & is.numeric(result))

pass.test

##---------------------------------------------------------------------------------------------------------------------##
## REFERENCES:                                                                                                         ##
## Chang, W. (2013). R Graphics Cookbook. Second Edition. O'Reilly.                                                    ##
## Chang, W. (2016). [Function] http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/                ##
## Jones, O., Maillardet, R., & Robinson, A., (2009). Scientific Programming and Simulation Using R. CRC Press.        ##
## Maindonald, J. (1984). Statistical Computation. John Wiley and Sons.                                                ##
## Mood, A.M., Graybill, F.A. & Boes, D.C.(1974). Introduction to the Theory of Statistics. Third Edition. McGraw Hill.##
## Teetor, P. (2011). R Cookbook. First Edition. O'Reilly.                                                             ##
## Field, A., Miles, J., Field, Z. (2012). Discovering Statistics Using R. Sage Publications.                          ##
##---------------------------------------------------------------------------------------------------------------------##