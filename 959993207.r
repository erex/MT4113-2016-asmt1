#Marsaglia and Bray's method-------------
myfunctionMB <- function(n, mu, sig){
  #xi is a vector of the size of the output wanted, made up of 0s here which will be replaced
  #number1 is a counting variable used to cycle through xi later on.
  xi <- rep(0,n)
  number1 <- 0    
  #checks if n mod 2 is 1, if it is it is odd. therefore add 1 to n (making nprime even so can be divided by 2)
  if ((n %% 2) == 1){
    nprime <- n+1
  }else{
    nprime <- n
  }
  #as algorithm makes 2 ouputs so have to run code nprime/2 times
  for (k in 1:(nprime/2)){
    #ui is a vector of length 2 so will have values of u1 and u2
    #set w greater than 1 so loop can begin, loops untill the two random numbers under the algorithm give a value < 1
    #incase w = 0, "or" condition added as later log(0) would give an error
    ui <- c(0,0)
    w <- 2
    while ((w > 1) | (w == 0)){
      w <- 0
      for (i in 1:2){
        ui[i] <- runif(1, min = 0, max = 1)
        ui[i] <- (2*ui[i])-1
        w <- w + ui[i]^2
      }
    }
    #b is used to split up the calculation into smaller parts
    b <- (-2)*log(w)
    v <- sqrt(b/w)
    #this compares number1 + 1 or 2 to n. If number1 + 1 or number1 + 2 is >n then the random value of will not be added to xi 
    #number1 + i re-places the 0 in the vector xi with the value  if there is still space in the vector
    for (i in 1:2){
      if ((number1+i)<=n){
        xi[(number1+i)] <- (ui[i]*v*sig)+mu
      }
    }
    #as a maximum of 2 elements can be added to vector xi add 2 to number1
    number1 <- number1+2
  }
  return (xi)
}

#Box-Mueller algorithm-------
myfunctionBM <- function(n, mu, sig){
  #xi is a vector of the size of the output wanted, made up of 0s which will be replaced later
  #number1 is again used as a counting variable as above.
  xi <- rep(0,n)
  number1 <- 0
  #check if n is odd or even. If odd, makes new variable nprime which is even
  if ((n %% 2) == 1){
    nprime <- n+1
  }else{
    nprime <- n
  }
  #as algorithm makes 2 ouputs so have to run code nprime/2 times
  for (k in 1:(nprime/2)){
    ui <- c(0,0)
    for (i in 1:2){
      ui[i] <- runif(1, min = 0, max = 1)
    }
    #a,b,c are used to split up the calculation into smaller parts
    a <- sin(2*pi*ui[1])
    b <- sqrt((-2)*log(ui[2]))
    c <- cos(2*pi*ui[1])
    for (i in 1:2){
      if ((number1+i)<=n){
        if (i==1){
          xi[(number1+i)] <- (a*b*sig)+mu
        }else{
          xi[(number1+i)] <- (c*b*sig)+mu
        }
      }
    }
    #as a maximum of 2 elements can be added to vector xi add 2 to number1
    number1 <- number1+2
  }  
  return (xi)
}

#Central limit theorem------
myfunctionCL <- function(n, mu, sig){
  #x is a vector of the size of the output wanted, made up of 0s which will be replaced later
  x <- rep(0,n)
  for (k in 1:(n)){
    #ui is a vector of length 16 made up of 0s which are then each replaced
    ui <- rep(0, 16)
    for (i in 1:16){
      ui[i] <- runif(1, min = 0, max = 1)
    }
    #a is used to split up the calculation into smaller parts
    a <- (sum(ui)-8)
    x[k] <- (a*sqrt(12/16)*sig)+mu
  }
  return (x)
}


#My function-------------
my.rnorm <- function(n, mean, sd){
  #missing is used to set defults for mean and sd
  if (missing(mean)){
    mean <- 0
  }
  if (missing(sd)){
    sd <- 1
  }
  #test to make sure n is an integer
  if (isinteger(pass = n)){
    #test to make sure n and sd possible values. (n and standard deviation cannot be negative)
    if ((n > 0) & (sd >= 0)){
      return(myfunctionMB(n = n,mu = mean,sig = sd))
    }else{
      return('invalid arguments for n or sd')
    } 
  }else{
    return('n is not an integer')
  }  
}

#my general function---------
general.rnorm <- function(n, mean, sd, method){
  #missing is used to set defults for mean, sd and method
  if (missing(mean)){
    mean <- 0
  }
  if (missing(sd)){
    sd <- 1
  }
  if (missing(method)){
    method <- 1
  }
  #test to make sure n is an integer
  if (isinteger(pass = n)){
    #test to make sure n and sd possible values. (n and standard deviation cannot be negative)
    if ((n > 0) & (sd >= 0)){
      if (method == 1){
        return(myfunctionMB(n = n, mu = mean, sig = sd))
      }else if (method == 2){
        return(myfunctionBM(n = n, mu = mean, sig = sd))
      }else if (method == 3){
        return(myfunctionCL(n = n, mu = mean, sig = sd))
      }else{
        return('invalid arguments')
      }
    }else{
      return('invalid arguments for n or sd')
    } 
  }else{
    return('n is not an integer')
  } 
}

#test if integer-------------
#function checks if data type. If double mod 1 = 0 then it is a integer. Returns true if it is an integer
isinteger <- function(pass){
  if (typeof(pass) == "double"){
    return(pass%%1 == 0)
  }
  return(typeof(pass) == "integer")
}

#test function---------
my_8_tests <- function(testcode){
  #test 1) first check also checks if default values for sd and mean are working
  a <- my.rnorm(n=10)
  pass.test1 <- (length(a) == 10 & is.numeric(a))
  print(c('test 1', pass.test1))
  #pass.test1 takes value TRUE if object 'a' contains 10 numeric values
  
  #test 2) check that it does not try and interprate non existent method
  b <- general.rnorm(n = 10, method = 4)
  pass.test2 <- (b == 'invalid arguments')
  print(c('test 2',pass.test2))
  #pass.test2 takes value TRUE if it stops the code and outputs 'invalid arguments' due to method being 4
  
  #test 3) check to see that non integer values of n are rejected
  c <- general.rnorm(n = 1.1)
  pass.test3 <- (!is.numeric(c))
  print(c('test 3',pass.test3))
  #pass.test3 takes value TRUE if it doesn't return a set of values due to n not being an integer
  
  #test 4) check to see what happens if n is a string (character data type)
  d <- my.rnorm(n='one')
  pass.test4 <- (!is.numeric(d))
  print(c('test 4',pass.test4))
  #pass.test4 takes the value TRUE if code returns a message of characters, therefore not producing a numeric value
  
  #test 5) check if histogram is transposed on a diffrent method and if it begins to form bell curv
  e <- general.rnorm(n=1000, mean = 20, method = 2)
  pass.test5 <- hist(e)
  #pass.test5 success if produces a histogram around a new mean, in this case 20
  
  #test 6) check to see if negative n and negative sd are rejected
  f <- general.rnorm(n = -1)
  g <- general.rnorm(n = 1, sd = -1)
  pass.test6 <- (is.numeric(f) & is.numeric(g))
  print(c('test 6',pass.test6))
  #pass.test6 success if takes value FALSE as both, n and sd, therfore are not accepted into code if less than 0
  
  #test 7) check that if n is odd produces odd number of random elements in x for first 2 methods
  h <- general.rnorm(n=11)
  i <- general.rnorm(n=15, method = 2)
  pass.test7 <- ((length(h) %% 2) == 1 & is.numeric(h) & (length(i) %% 2) == 1 & is.numeric(i))
  print(c('test 7',pass.test7))
  #pass.test7 takes value TRUE if object 'h' and 'i' contain an odd number of numeric values. 
  #Assume right length dues to test 1
  
  #test 8) check sd = 0 works
  j <- 3
  k <- general.rnorm(n = 10,mean = j, sd = 0)
  pass.test8 <- all(k == j)
  print(c('test 8',pass.test8))
  #pass.test8 takes value TRUE if object 'k' has only values equal to mean (j in ths case)

}

