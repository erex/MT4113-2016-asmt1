# I confirm that the attached is my own work, except where clearly indicated in the text.

# This is my function for checking that the argument n (number of samples) passed through the following
# functions meets all of the criteria it should.

argument.test <- function(n){
  
  # Checks that there are actually samples provided.
  
  if(is.null(n)){ 
    stop("Invalid Arguments - No samples provided.")
  }
  
  # Checks that the input is actually numerical.
  
  if(is.finite(n) != TRUE){ 
    stop("Invalid Arguments - Input is non-numerical or infinite.")
  }
  
  # Checks that a positive number of samples was input.
  
  if(n <= 0){ 
    stop("Invalid Arguments - Number of samples is not positive or equal to 0.")
  }
  
  # Checks that the input is a real number.
  
  if(Im(n) != 0){ 
    stop("Invalid Arguments - Input is imaginary.")
  }
  
  # Checks to ensure the value of n is an integer. Disclaimer: I used Stackoverflow to find out how to do this.
  # I provide the link to the webpage in the bottom of the code.
  
  if(n%%1 != 0){ 
    stop("Invalid Arguments - Input is not an integer.")
  }
  
  # Checks if the length is greater than 1. If so, it is a vector and it shouldn't run.
  
  if(length(n) > 1){
    stop("Invalid Arguments - The input is a vector and not a scalar and thus it cannot be run.")
  }
  
  # Checks that the input is larger than 5000 or smaller than 3. I do this so that the Shapiro-Wilk 
  # test will run because it is a requirement of the test for the number of samples to be between 3-5000. 
  # While this is not required for any of the methods, it allows us to have sufficient evidence that our data is 
  # normal so I have included it in our checks.
  
  if(n > 5000){ 
    stop("Invalid Arguments - Number of samples is too large for the Shapiro-Wilk Test and thus we will not have 
         sufficient evidence of normality.")
  }
  
  if(3 > n){ 
    stop("Invalid Arguments - Number of samples is too small for the Shapiro-Wilk Test and thus we will not have 
         sufficient evidence of normality.")
  }
  
  return(NULL)
  
  }

# Create the function, my.rnorm which will depend on n (number of samples), mu (mean), and sigma 
# (standard deviation).

my.rnorm <- function(n, mu = 0, sigma = 1){
  
  argument.test(n)
  
  # Checks if number of samples is even. This must be done because the MB method requires pairs. If it is not
  # even, I have asked the code to multiply the number of samples by 2. Disclaimer: I used Stackoverflow to
  # find out how to do this. I have provided a link as part of another disclaimer that shows where I learned
  # the trick for the modulo.
  
  if (n%%2 == 0) {
  } else { 
    n <- 2*n
  }
  
  # Here I create a an empty vector that will be filled with the normally distributed data.
  
  x.vector <- rep(0,n) 
  
  # I start by initializing a for loop which must goes from 0:(n/2). I do this so that it iterates over pairs
  # as is required for the MB method. This for loop is what will store my u[1] and my u[2] in pairs into my 
  # x.vector to later be plotted on a histogram.
  
  for (i in 0:(n/2)){ 
    
    # I chose to use a repeat compared to a while simply because I didn't want to initialize a value of w
    # before running the while loop. Both could've done the job, but I just thought it looked cleaner like
    # this. This repeat creates a vector u which is two random variates from a uniform distribution. Then
    # it modifies the u as stated in the problem to make the unit square. I then used the equation given
    # to create w which is a scalar quantity by taking the u[1] and u[2] parts of u. My if condition breaks
    # the loop to re-run if w < 1 because then the condition given has not been met. Once a value greater than
    # 1 has been acheived, the loop is ended.
    
    repeat {
      u <- runif(2,0,1)
      u <- 2*u-1
      w <- (u[1])^2+(u[2])^2
      if (w < 1){ 
        break
      }
    }
    
    # Here I create the scalar v which is jsut the equation given.
    
    v = (-2*log(w)/w)^(1/2)
    
    # This is where my for loop acts. Because of the n/2 in the loop, it stores my values in pairs into
    # x.vector. To get the pair-like strucutre, appending u[2] into x.vector had to be done in the same way
    # that the for loop acted. I chose to do it this way instead of using another loop because R is a
    # vectorized language. While this isn't particularly pretty, it gets the job done and uses the
    # vectorization to my advantage.
    
    x.vector[i] <- u[1]*v
    x.vector[i+(n/2)] <- u[2]*v
    
  }
  
  par(mfrow = c(1:2))
  
  # This histogram shows a frequency of the data from the MB method. At first glance it looks normally 
  # distributed however I have run two tests below to check that.
  
  hist(x.vector*sigma+mu, nclass = 75, col = "lightblue3", main = "Normally Distributed Data")
  
  # A Q-Q Plot compares two probability distributions, one of which can be theoretical. If the distributions are
  # similar, they will lie on top of one another and on the line y = x. From what is seen on the plot, the lines
  # are nearly identical (save a few outliers) and are of the form y = x. Thus I can say with a decent level
  # of confidence that the data produced from the MB method is normally distributed.
  
  qqnorm(x.vector*sigma+mu)
  qqline(x.vector*sigma+mu, col = "red", lwd = 2)
  
  # Next I have run a Shapiro-Wilk Normality Test. The Shapiro-Wilk Normality test produces two values, a 
  # test statistic (w) and a p value. The test statistic for when I ran the tests on my computer were 
  # consisntely around w ~ 0.998, which supports a hypothesis that my data is normally distributed. My 
  # p values were in the range of (0.25-0.80) which is much larger than 0.05, thus this implies that my test
  # statistic is consistent with my hypothesis of having normally distributed data. This applies to all three
  # tests I ran.
  
  # I ask it to print the SW Test which will display above x.vector which is the list of data.
  
  print(if(length(x.vector*sigma+mu) <= 5000)
    shapiro.test(x.vector*sigma+mu))
  
  # Returns my vector of normally distributed data created by the MBR method.
  
  return(x.vector)  
  
}


# Create a function box.muller which will perform our Box-Muller transformation.

box.muller <- function(n, mu, sigma){
  
  # Checks if number of samples is even. This must be done because I divide n by 2 later in the code so that
  # the user doesn't get double the amount of data requested.
  
  if (n%%2 == 0) {
  } else { 
    n <- 2*n
  }
  
  # Creates two vectors of length n/2 of uniform data that will be used in the equations given. The main difference
  # between this code and the MBR code is that I don't have a for loop. The reason is that for MBR I needed
  # pairs and storing the pairs in a 'pair-wise' fashion was easier done with a for loop whereas here I just
  # need some uniform data to transform into normal data.
  
  # I run it with n/2 because otherwise you would get double the amount of data requested once I combined the
  # two vectors. Because of this, I require n to be even - thus I run the same even check as I did in
  # MBR.
  
  u1 <- runif(n/2) 
  u2 <- runif(n/2) 
  
  # The equations given with our vectors of uniform data.
  
  x1 <- sqrt(-2*log(u1))*cos(2*pi*u2) 
  x2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
  
  #  I now take the transformed data which should now be normal. I combine them into one vector x.
  
  x <- c(x1,x2)
  
  # Return the normal data from the function.
  
  # As with MBR, I pass values through the function, partition the plot screen, plot a histogram of results and a
  # Q-Q Plot to test for normality. The function seems to have worked perfectly fine.
  
  par(mfrow = c(1,2))
  hist(x*sigma+mu, nclass = 75, col = "lightblue3", main = "Normally Distributed Data")
  qqnorm(x*sigma+mu)
  qqline(x*sigma+mu, col = "red", lwd = 2)
  
  # Just to be sure, I check with the Shapiro-Wilk Test. This will print above the set of data.
  
  print(if(length(x*sigma+mu) <= 5000)
    shapiro.test(x*sigma+mu))
  
  # Return the set of data.
  
  return(x*sigma+mu)
  
}

# Create a function to test the Central Limit Theorem. Pass a number of samples.rows from the uniform (n), mean (mu), 
# and standard deviation (sigma) through it.

central.limit.theorem <- function(n, mu, sigma, lambda){
  
  # To test the central limit theorem I need to create a matrix. This will have n rows and i columns. Each row
  # is one sample from our uniform distribution. This is where the repetition of samples comes in.
  # Otherwise it would be a single sample and you wouldn't get nicely transformed data.
  
  i <- 16 # As specificied in the Assignment sheet.
  
  # This produces a matrix of observations with n rows and i columns. I make the matrix of uniformly 
  # distributed data which I will use as samples.rows of the distribution.
  
  samples.rows <- matrix(runif(i*n, 0, 1),n)
  
  # Now as done in the equation, I have to sum over all the samples.rows, however this has to be done quite 
  # carefully because you want to pick out one sample at a time. Thus, I use the apply() that we learned in
  # class to pull out the rows and then both give a sum (as asked for) to demonstrate the CLT.
  # I apply the formula as asked for in the Assignment sheet.
  
  applied.sample.sums <- (apply(samples.rows, MARGIN = 1, FUN = sum) - 8)*sqrt(12/16)
  
  # As I am one of the students who hasn't taken the statistical pre-requisites for this class I did some
  # research into what exactly the purpose of the CLT is. According to the Wikipedia article the power of it
  # is that it also returns normally distributed mean and variance distributions. Thus, I was 
  # curious to see if that does happen. This was quite easy to do as the apply() function we learned in class 
  # has options for this.
  # Disclaimer: I have attached Wikipedia article at bottom.
  
  applied.sample.means <- apply(samples.rows, MARGIN = 1, FUN = mean)
  applied.sample.variances <- apply(samples.rows, MARGIN = 1, FUN = var)
  
  par(mfrow=c(2,4))
  
  # Create the sum of the uniform as asked for in the Assignment. The graph is quite normally distributed so
  # I am happy with the result.
  
  hist(applied.sample.sums*sigma+mu, col = "lightblue3", main = "Uniform Distribution 
       Transformed Sum", nclass = 35)
  
  # Now lets test the power of the CLT. Just take data stored in .means and .variances
  
  hist(applied.sample.means, col = "lightblue3", main = "Uniform Distribution 
       Transformed Mean", nclass = 35)
  
  hist(applied.sample.variances, col = "lightblue3", main = "Uniform Distribution
       Transformed Variances", nclass = 35)
  
  # Once again, apply a Q-Q Plot test.
  
  qqnorm(applied.sample.sums*sigma+mu)
  qqline(applied.sample.sums*sigma+mu, col = "red", lwd = 2)
  
  # In the Assignment it says to also explore what would happen if we had an initial distribution that wasn't
  # necessarily uniform. One of the distributions in the 'Getting Started' sheets (thus, one of the only other
  # ones I know because I haven't done the 2000 level statistics pre-reqs) is Poissonian, so I will test what
  # happens if I apply this code to a Poissonian distribution. Follow the same process as before except now 
  # all I need is a lambda.
  
  # Having not taken statistics before, this is where my lack of knowledge might hurt me. The formula given
  # on the sheet for the CLT will not actually return the mean of transformed Poissonian distribution to 0.
  # My guess is that there is a different type of formula for each type of data set? I am unsure of this
  # though even after I did some googling. Thus, I have not applied any formula and the mean is far from 0.
  # However, it does show that the data transformed is normal.
  
  samples.rows.p <- matrix(rpois(i*n,lambda),n)
  
  applied.sample.sums.p <- apply(samples.rows.p, MARGIN = 1, FUN = sum)
  
  applied.sample.means.p <- apply(samples.rows.p, MARGIN = 1, FUN = mean)
  
  applied.sample.variances.p <- apply(samples.rows.p, MARGIN = 1, FUN = var)
  
  hist(applied.sample.sums.p*sigma+mu, col = "lightblue3", main = "Poissonian Distribution 
       Transformed Sum", nclass = 35)
  
  hist(applied.sample.means.p, col = "lightblue3", main = "Poissonian Distribution 
       Transformed Mean", nclass = 35)
  
  hist(applied.sample.variances.p, col = "lightblue3", main = "Poissonian Distribution
       Transformed Variances", nclass = 35)
  
  qqnorm(applied.sample.sums.p*sigma+mu)
  qqline(applied.sample.sums.p*sigma+mu, col = "red", lwd = 2)
  
  # Run Shapiro-Wilk Normality Tests to check for normality. These will print above the set of data.
  
  print(if(length(applied.sample.sums*sigma+mu) <= 5000)
    shapiro.test(applied.sample.sums*sigma+mu))
  
  print(if(length(applied.sample.sums.p*sigma+mu) <= 5000)
    shapiro.test(applied.sample.sums.p*sigma+mu))
  
  # Return normally distributed set of data.
  
  return(applied.sample.sums*sigma+mu)
  
}

# https://en.wikipedia.org/wiki/Central_limit_theorem

# Create the general.rnorm that contains all variables. This function will contain a switch() which allows
# the user to choose which test they would like to perform. Test 1 indicates MBR, Test 2 indicates BM, 
# Test 3 indicates the CLT. For ease, I use toString on test so that the user doesn't have to put quotes
# around the number of the test they'd like to perform.

general.rnorm <- function(n, mu = 0, sigma = 1, lambda = 100, test = 1){
  argument.test(n)
  test <- toString(test)  
  switch(test,
         
         "1" = (general.results <- my.rnorm(n, mu, sigma)),
         "2" = (general.results <- box.muller(n, mu, sigma)),
         "3" = (general.results <- central.limit.theorem(n, mu, sigma, lambda)))
  
  # Return results from the specified test.
  
  return(general.results*sigma+mu)
  
}

# Below I have provided a commented-out call statement to my general.rnorm function. I have done this because
# I used the testing algorithm provided to us in the assignment sheet to test if my code would pass the tests 
# you planned to run on it. To do that, I had to change quite a lot of what I had done because originally my
# code was printing out lots of histograms, QQ Plots, and Shapiro Wilk Tests. I have altered my code in such 
# a way that if you call the function general.rnorm you will still get to see all those things, however
# it should now also pass the standardised tests that you plan to perform on it. To see all of that just
# call the function general.rnorm and assign values for n and test. From there it will produce all of the
# histograms, QQ Plots, and SW Tests I have in my functions above, which I think are the real strength of
# what I have done.

# general.rnorm(n, mu = 0, sigma = 1, lambda = 100, test)