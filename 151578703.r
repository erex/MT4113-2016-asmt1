# I confim that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n, mean = 0, sd = 1){ 
  
  # This function is used to generate random normal distributed numbers.
  # In arguments, n represent number of values to return with no default,
  # mean represent mean of values to return with default value 0,
  # sd represent standard deviation of value to return with default value 1.
  # It use Marsaglia and Bray's method to generate random numbers.
  
  if (n < 0 | sd < 0){                                 # use if function to detect if there is any invalid arguments
    stop("invalid arguments")                          # use stop function to stop execution and output error message
  }
  else if (n == 0){                                    # if the number of output values is 0
    Random_number <- vector(mode = "numeric")          # return an empty numeric vector
  }
  else{                                                # if all arguments are not invalid
    Random_number <- vector()                          # create a vector without fixed length used to receive output values
    for(m in 1:n){                                     # use for loop to generate n output values
      repeat{                                          # use repeat function to make sure U[1]^2 + U[2]^2 <= 1
        U <- runif(2)                                  # generate a pair of U~uniform(0,1)
        U <- 2*U - 1                                   # make U distributed over the range [-1,1]
        w = U[1]^2 + U[2]^2                            # create w to store the value of U[1]^2 + U[2]^2
        if (w <= 1) break                              # if w <= 1, jump out the loop
      }
      v <- (-2*log(w)/w)^0.5
      X <- U * v                                       # generate a pair of normal distributed value X
      Random_number[m] <- X[1]                         # vector of output receive the value of X[1] to make sure any n number
    }                                                  # can achieved
    Random_number <- mean + sd * Random_number         # generate random number ~ N(mean, sd^2)
  }
  return(Random_number)                                # return values
}

general.rnorm <- function(n, mean = 0, sd = 1, method = 1){
  
  # This function use three different ways to generate random normal distributed numbers
  # Different with function my.rnorm, a new argument named method is created to specify which
  # method should be used.
  
  if (n < 0 | sd < 0){                                   # use if function to detect if there is any invalid arguments
    stop("invalid arguments")                            # use stop function to stop execution and output error message
  }
  else if (n == 0){                                      # if the number of output values is 0
    Random_number <- vector(mode = "numeric")            # return an empty numeric vector
  }
  else{                                                  # if all arguments are not invalid 
    if (method == 1){                                    # if method = 1, use Marsaglia and Bray's method 
      my.rnorm(n,mean,sd)                                # use my.rnorm function with arguments n, mean, sd
    }
    else if (method == 2){                               # if method = 2, use Box-Mueller algorithm
      Random_number <- vector()                          # create a vector without fixed length used to receive output values
      for (m in 1:n){                                    # use for loop to generate n output values
        X <- c(0,0)                                      # create a pair of X 
        U <- runif(2)                                    # generate a pair of U~uniform(0,1)
        X[1] <- sin(2*pi*U[1])*((-2*log(U[2]))^0.5)      # generate X[1] 
        X[2] <- cos(2*pi*U[1])*((-2*log(U[2]))^0.5)      # generate X[2]
        Random_number[m] <- X[1]                         # vector of output receive the value of X[1] to make sure any n number
      }
      Random_number <- mean + sd * Random_number         # generate random number ~ N(mean, sd^2)
    }
    else if (method == 3){                               # if method = 3, use Central limit theorem
      Random_number <- vector()                          # create a vector without fixed length used to receive output values
      for(m in 1:n){                                     # use for loop to generate n output values
        U <- runif(16)                                   # generate 16 random uniform distributed numbers ~U(0,1)
        Random_number[m] <- (sum(U)-8)*((12/16)^0.5)     # generate n random normal distributed numbers~N(0,1)
      }
      Random_number <- mean + sd * Random_number         # generate n random normal distributed numbers~N(mean,sd^2)        
    }
    else{                                                # if input value of method is invalid
      stop("invalid arguments")                          # stop execution and output error message
    }
  }
  return(Random_number)                                  # return values
}

test.rnorm <- function(){
  
  # This function is used to test my.rnorm function and general.rnorm function
  # It has no arguments, but all arguments want to test are asked to input via a menu
  # Menu in this function will repeat again after a test finished until receive a quit command
  # It can also back from a branch level menu to the high level menu
  
  repeat{                                                            # create a repeatable menu for user to select methods
    cat("\nWhich method you want to test:\n")
    cat("1. Marsaglia and Bray's method\n")                          # input 1 for Marsaglia and Bray's method
    cat("2. Box-Mueller algorithm\n")                                # input 2 for Box-Mueller algorithm
    cat("3. Central limit theorem\n")                                # input 3 for Central limit theorem
    cat("4. Quit\nMethod:")                                          # input 4 for quiting test
    method_receive <- scan("",n=1)                                   # allow user to input command from keyboard
    if(method_receive == 4) {                                        # if input is 4
      return("Quit Test")                                            # quit test
    } 
    else if(method_receive %in% c(1,2,3)){                           # if input is 1 or 2 or 3  
      cat("Number of values to return (n):\n")                       # allow user to input n from keyboard
      n_receive <- scan("",n=1)                                      # receive value of n
      cat("Mean of values to return (mean):\n")                      # allow user to input mean from keyboard
      mean_receive <- scan("",n=1)                                   # receive value of mean
      cat("Standard deviation of values to return (sd):\n")          # allow user to input sd from keyboard
      sd_receive <- scan("",n=1)                                     # receive value of sd
      test_results <- general.rnorm(n=n_receive,mean=mean_receive,sd=sd_receive,method=method_receive)        #generate test results with arguments n, mean, sd, method
      print(test_results)                                              # print results for testing
      repeat{                                                          # create a repeatable branch menu for testing
        cat("\n1. Compare actual results with desired output\n")       # input 1 for comparing desired n, mean and sd with actual n, mean and sd
        cat("2. Computes a histogram of results\n")                    # input 2 for computes a histogram of results
        cat("3. Produces a normal Quantile-Quantile Plot\n")           # input 3 for produces a normal q-q plot
        cat("4. Use Shapiro-Wilk Normality test\n")                    # input 4 for use Shapiro-Wilk Normality test
        cat("5. Use Kolmogorov-Smirnov test\n")                        # input 5 for Use Kolmogorov-Smirnov test
        cat("6. Back to select method\n")                              # input 6 for back to select method menu
        cat("7. Quit\n")                                               # input 7 for quit test
        test_way <- scan("",n=1)                                       # allow user to input command from keyboard
        if(test_way == 6) break                                        # if input is 6, then back to select method menu
        else if(test_way %in% c(1,2,3,4,5)){                           # if input in c(1,2,3,4,5)
          if (test_way == 1){                                          # if input is 1 
            cat("Actual results are:\n")                               # compare actual results with desired output
            print(test_results)
            cat("\nDesired n is:",n_receive,"Actual n is:",length(test_results),"Difference is:",n_receive-length(test_results),"\n")
            cat("\nDesired mean is:",mean_receive,"Actual mean is:",mean(test_results),"Difference is:",mean_receive-mean(test_results),"\n")
            cat("\nDesired sd is:",sd_receive,"Actual sd is:",sd(test_results),"Difference is:",sd_receive-sd(test_results),"\n")
          }
          else if (test_way == 2){                                    # if input is 2
            hist(test_results)                                        # computes a histogram of results
          }
          else if (test_way == 3){                                    # if input is 3
            qqnorm(test_results)                                      # produces a normal Quantile-Quantile Plot
          }
          else if (test_way == 4){                                    # if input is 4
            print(shapiro.test(test_results))                         # use Shapiro-Wilk Normality test
          }
          else{                                                       # if input is 5
            print(ks.test(test_results,"pnorm",mean = mean(test_results),sd = sd(test_results)))  # use Kolmogorov-Smirnov test
          }
        }
        else if(test_way == 7){                                       # if input is 7
          return("Quit Test")                                         # quit test
        }
        else {                                                        # if input is invalid arguments
          cat("\n Invalid opinion, please re-select\n")               # repeat branch menu and ask user to reselect 
          next                                                        # jump to next loop
        }
      }
    }
    else{                                                             # if input is invalid arguments
      cat("\n Invalid opinion, please re-select\n")                   # repeat branch menu and ask user to reselect
      next                                                            # jump to next loop
    }
  }
}








