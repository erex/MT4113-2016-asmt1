# I confirm that the attached is my own work, except where clearly indicated in the text


# my.rnorm

my.rnorm <- function(n) {
  u1 <- c()
  w1 <- c()
  v <- c()
  vnew <- c()
  x1 <- c()
  if(!is.numeric(n)){                  
    stop("invalid arguments")    
  } else if (n <= 0){
    stop("invalid arguments")
  } else if (n %% 2 != 0){              
    stop("invalid arguments")
  } else {
    for (i in seq(1:n)){
      u1[i] <- (runif(1,0,1)) }
    for (i in seq(1:length(u1))){     
      u1[i] <- (2*u1[i])-1 }
    for (i in 1:length(u1)){ 
      w1[i] <- (u1[i])^2 + (u1[i+1])^2 }   # this means w1[1] <- u1[1]^2 + u1[2]^2 as required but also w1[2] <- u1[2]^2 + u1[3]^2 which is not needed 
    for (i in seq(1,length(w1),2)){        # so only every second value in w1 is checked to be less than 1  
      while (w1[i] > 1) {    
        u1[i] <- runif(1,0,1)              # if value in w1 is >1 new values for the pair of values in u1 are generated
        u1[i+1] <- runif(1,0,1)
        w1[i] <- (u1[i])^2 + (u1[i+1])^2}
      }
    w <- c(w1[seq(1, length(w1), 2)])     # w is assinged to the vector of values from w1 with odd index numbers (i.e. every other value) 
    v <- sapply(w,function(w)(sqrt((-2*log(w))/w)))
    while(length(v) < 2*length(v)){        # however now we have a v vector which is half the length of the u1 vector
      for(i in seq(1,(2*length(v)),2)){    # so I duplicate every value in v (i.e if a and b are v values v now reads (a,a,b,b,...)) 
        v <- append(v,v[i],i)}
      for (i in seq(1,length(u1))){  
        x1[i] <- u1[i]* v[i]}             # making it easier to assign the x1 values
      return(x1)
    }
  }
}




# general.rnorm


general.rnorm <- function(y,z=1){
  u2 <- c()
  x2 <- c()
  u3 <- c()
  x3 <- c()
  if(!is.numeric(y)){
    stop("invalid arguments")    
  } else if (y <= 0){
    stop("invalid arguments")
  } else if (y %% 2 != 0){
    stop("invalid arguments")
  } else if (z==1){
    return (my.rnorm(y))      
  } else if (z==2){
    for (i in (1:y)){
      u2[i] <- runif(1,0,1)}
    for (i in seq(1,length(u2),2)){            # lets us deal with the x2 values in pairs
      x2[i] <- sin(2*pi*u2[i])*sqrt(-2*log(u2[i+1]))
      x2[i+1] <- cos(2*pi*u2[i])*sqrt(-2*log(u2[i+1]))}
    return (x2)
  } else if(z==3){
    for (i in seq(1:(16*y))){          # 16 uniform values are generated for each normal deviate we want to generate
      u3[i] <- runif(1,0,1)
    }
    for (i in 1:y){
      x3[i] <- (sum(u3[((16*i)-15):(16*i)])-8)*sqrt(12/16)  # the sum sums values from u3 in groups of 16
    }
    return (x3)
  }
}





# Additional Functions

# 1 (One for each of my.rnorm and general.rnorm)
    'Checking the functions produce the expected number of deviates.'

test_length_my.rnorm <- function(n){
  if(length(my.rnorm(n)) == 0){
    stop('no deviates have been generated')
    } else if(length(my.rnorm(n)) %% 2 != 0){
    stop('odd number of deviates have been generated')
      } else if(length(my.rnorm(n)) != n){
        stop('even number of deviates have been generated but an incorrect number')
        } else {
    return(TRUE)
  }
}



test_length_general.rnorm <- function(n,z=1){   # z can be set to either 1,2 or 3 (as in general.rnorm) with 1 as the default
  if(length(general.rnorm(n)) == 0){
    stop('no deviates have been generated')
  } else if(length(general.rnorm(n)) %% 2 != 0){
    stop('odd number of deviates have been generated')
  } else if(length(general.rnorm(n)) != n){
    stop('even number of deviates have been generated but an incorrect number')
  } else {
    return(TRUE)
  }
}





#2 (One for each of my.rnorm and general.rnorm)
'Checking the functions produce deviates with expected means and standard deviations.
Gives message to check values and possibly run again if values are unexpected '

test_mean_and_sd_my.rnorm <- function(n){
  if((abs(mean(my.rnorm(n)))) > 2.5){
    stop('check values and run again if visibily do not seem like they are from a normal distribution with mean 0 and variance 1')}
  if(sd(my.rnorm(n)) > 1.7){
    stop('check values and run again if visibily do not seem like they are from a normal distribution with mean 0 and variance 1')
    } else {
    return(TRUE)}
}



test_mean_and_sd_general.rnorm <- function(n,z=1){
  if((abs(mean(general.rnorm(n)))) > 2.5){
    stop('check values and run again if visibily do not seem like they are from a normal distribution with mean 0 and variance 1')}
  if(sd(general.rnorm(n)) > 1.7){
    stop('check values and run again if visibily do not seem like they are from a normal distribution with mean 0 and variance 1')
  } else {
    return(TRUE)}
}



#3
'my.rnorm is plotted against itself. I have also plotted circles to represent each standard 
deviation, using similar code to code I found online from lines 172-180 to help draw the circles.
I use estimates that in a normal distribution 68% of values are within 1 standard 
deviation, 95% are within 2 and 99.7% are within 3.
I then calculate the difference in the gaps between the circles(which is expected to be the same)
and return an error message if the difference is unexpectedly large'

'You could do a very similar thing for general.rnorm, starting the function off with
analysing_general.rnorm <- function(n,z=1)'

'I am surprised that both this function and analysing_general.rnorm (when I set z to 
1, 2 or 3) consistently produced circles of radius larger than 1, 2 and 3. Exactly the same thing 
happened when rnorm was plotted against itself, so it shows my.rnorm and general.rnorm
are doing a very similar job to rnorm, but I could not think of a reason the radii of the 
circles were not approximately 1,2 and 3 every time.'


analysing_my.rnorm <- function(n){  #using large n (i.e.>1000) is recommended
  xgraph <- my.rnorm(n)
  plot(xgraph, xgraph, asp=1)
  r <- sqrt(xgraph^2+xgraph^2)
  f1 <- 0.68
  f2 <- 0.95              
  f3 <- 0.997
  radius1 <- sort(r)[ f1 * length(r) ]
  radius2 <- sort(r)[ f2 * length(r) ]
  radius3 <- sort(r)[ f3 * length(r) ]
  xx1 <- radius1*cos( seq(0,2*pi, length.out=360) )
  yy1 <- radius1*sin( seq(0,2*pi, length.out=360) )
  xx2 <- radius2*cos( seq(0,2*pi, length.out=360) )
  yy2 <- radius2*sin( seq(0,2*pi, length.out=360) )
  xx3 <- radius3*cos( seq(0,2*pi, length.out=360) )
  yy3 <- radius3*sin( seq(0,2*pi, length.out=360) )
  lines(xx1,yy1, col='yellow')
  lines(xx2,yy2, col='red')
  lines(xx3,yy3, col='blue')
  difference <- (radius3-radius2)-(radius2-radius1)
  return(c(radius1, radius2, radius3, difference))
  if(abs(difference) > 0.4){
    stop('possible error in my.rnorm code')
  }
}



#4

'This is more of a visual check.
This function compares my.rnorm with the inbuilt function rnorm with the standard deviation 
circles drawn as above' 

'There is more variation in the numerical values of the radii as two variables are being compared 
instead of just analysing one, but it is still a useful visual tool showing my.rnorm is doing
a similar job to rnorm as the circles are approximately equally spaced'

'You could do a very similar thing for general.rnorm, starting the function off with
comparing_rnorm_and_general.rnorm <- function(n,z=1)'

comparing_rnorm_and_my.rnorm <- function(n){  #using large n (i.e.>1000) is recommended
  xgraph <- rnorm(n)
  ygraph <- my.rnorm(n)
  plot(xgraph, ygraph, asp=1)
  r <- sqrt(xgraph^2+ygraph^2)
  f1 <- 0.68
  f2 <- 0.95               
  f3 <- 0.997
  radius1 <- sort(r)[ f1 * length(r) ]
  radius2 <- sort(r)[ f2 * length(r) ]
  radius3 <- sort(r)[ f3 * length(r) ]
  xx1 <- radius1*cos( seq(0,2*pi, length.out=360) )
  yy1 <- radius1*sin( seq(0,2*pi, length.out=360) )
  xx2 <- radius2*cos( seq(0,2*pi, length.out=360) )
  yy2 <- radius2*sin( seq(0,2*pi, length.out=360) )
  xx3 <- radius3*cos( seq(0,2*pi, length.out=360) )
  yy3 <- radius3*sin( seq(0,2*pi, length.out=360) )
  lines(xx1,yy1, col='yellow')
  lines(xx2,yy2, col='red')
  lines(xx3,yy3, col='blue')
  return(c(radius1, radius2, radius3))
}





















