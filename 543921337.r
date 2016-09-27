#I confirm that the attached is my own work, except where clearly indicated in the text.
my.rnorm<-function(n,mean=0,sd=1){
  if (n<=0) stop("invalid arguments") # make sure the input mean is valid.
  if(sd<0)  stop("invalid arguments") # make sure the input stardand deviation is valid.
  if((is.numeric(mean))==FALSE) stop("invalid arguement")  #make sure the input mean is a number.
  if (n%%2==0) { # to specify the case that n is a even number by using the reminder of (n/2) is 0
    nob<-array(0,n) # create the array with length n and put all 0 in it, later, putting X values in this array
    for (j in (1:n/2)){ # generating n numbers of X means generating n/2 pairs of X
      while (TRUE){
        U<-runif(2)
        U.updated<-2*U-1
        w<-U.updated[1]^2+U.updated[2]^2
        if (w<=1) break
      }  #using while loop to generate the w that meet the unit circle requirements. 
      # assume it is true at the first, generate 2 appropriate random deviates U.updated[1] and U.updated[2].
      # this while loop only stops when the w<=1, so indeed, outside this while loop, we have the suitable w.
      v<-sqrt(-2*log(w)/w)
      X1<-U.updated[1]*v
      X2<-U.updated[2]*v  # create X1  and X2 by the defination provided.
      nob[2*j-1]<-X1      # in order to aviod the overlap(e.g.the 3rd X replaces the 2nd X in the second run of for loop.)  
      nob[2*j]<-X2        # I put the X1 in the position of the 2*j-1 in the arrary nob(e.g.for the first for loop, the position is 2*1-1,replace the 0 with this X1).
    }                     # using this way of putting X to the position in nob, we put them in pairs and avoid the overlap.
    answer<-nob[1:n]*sd+mean
                          # if the given mean and variance are not 0 and 1 respectively, we transfer them in a new array having the desirable mean and variances.
  }                       
  else {
    nob<-array(0,n+1)     # still creating a new array with the even length (n+1)
    for (j in (1:(n+1)/2)){ # creating the for loop to create (n+1)/2 pairs of X which meet the unit square requirements
      while (TRUE){
        U<-runif(2)
        U.updated<-2*U-1
        w<-U.updated[1]^2+U.updated[2]^2
        if (w<=1) break
      }
      v<-sqrt(-2*log(w)/w)
      X1<-U.updated[1]*v
      X2<-U.updated[2]*v  
      nob[2*j-1]<-X1
      nob[2*j]<-X2
    }      # same expalination applies as above
    answer<-nob[1:n]*sd+mean #because at the first we add 1 to make the array even, now we only want to have the first n elemnts(odd number) that in array nob.
  }
  return(answer) # return the vector conatining the deviates that satisfy the given parameters.
}


Box.Mueller<-function(n,mean=0,sd=1){  
  # This function corresponds to the Box and Mueller algorithm
  if (n<=0) stop("invalid arguments")
  if(sd<0)  stop("invalid arguments")
  if((is.numeric(mean))==FALSE) stop("invalid arguement")
  if (n%%2==0) { # the question does not specify whether the pairs in this case should be maintained for each time.
    # i just assume that the pair should be maintained so that the similar strategy applies here as in the function "my.rnorm".
    # again, 2 situations with even and odd n input are splited into 2 parts,also the same way to make the generated random deviates with desiable mean and variance.
    nob<-array(0,n)
    for (j in (1:n/2)){
      U<-runif(2)
      X1<-sin(2*pi*U[1])*sqrt(-2*log(U[2]))
      X2<-cos(2*pi*U[1])*sqrt(-2*log(U[2])) 
      nob[2*j-1]<-X1
      nob[2*j]<-X2
    }
     answer<-nob[1:n]*sd+mean
  }
  else {
    nob<-array(0,n+1)
    for (j in (1:(n+1)/2)){
      U<-runif(2)
      X1<-sin(2*pi*U[1])*sqrt(-2*log(U[2]))
      X2<-cos(2*pi*U[1])*sqrt(-2*log(U[2]))
      nob[2*j-1]<-X1
      nob[2*j]<-X2
    }
      answer<-nob[1:n]*sd+mean
  }
  return(answer)
}

central.limit.theorem<-function(n,mean=0,sd=1){
  if (n<=0) stop("invalid arguments") 
  if(sd<0)  stop("invalid arguments")
  if((is.numeric(mean))==FALSE) stop("invalid arguement")
  nob<-array(0,n) # for this central limit theorem,just single X generates each time, so that the for loop is just from 1 to n.
  for ( j in (1:n)){ # using the defination, suitable X is generated each time (for each run) and put into the correspounding position.
    U<-runif(16)
    X<-(sum(U)-8)*sqrt(12/16)
    nob[j]<-X
  }
    answer<-nob[1:n]*sd+mean  
  return(answer)
}

general.rnorm<-function(n,mean=0,sd=1,method=1){
  #purpose:we generate a randomly normal detributed random variables using desiable method.
  #
  #input: 
  #
  #n:the number of values to returne
  #mean:mean of the values to return (defalt 0)
  #sd:standard deviation of values to return  (defalt 1)
  #method:a number with 1,2 and 3 corrsponding to M&B algorithm,B&M and central limited theorem algorithm respectively,
  #but any positive number could be the input.
  #
  #output: the vector with length n contains normally distributed deviates generated by particular method, if the input for the method is not specificed as 3 above,then the error message with "in valid argument" will return. 
  if (method>=4) stop("invalid arguments")
  if(n<=0) stop("invalid arguments") 
  if(sd<0)  stop("invalid arguments")
  if((is.numeric(mean))==FALSE) stop("invalid arguement")
  if (method==1){
    my.rnorm(n,mean,sd) 
  } else { if (method==2) {
    Box.Mueller(n,mean,sd)
  }
    else {
      central.limit.theorem(n,mean,sd) 
    } # although I don't write the return at the end of this function, because"general.rnorm" just recalls 3 functions(three different algorithems),
    # in each function, return is included at the end of the function. So, this "general.rnorm" gives me the vector with length =n, given mean and vairances, 
    # most importantly, it uses the given method to generate the random normal deviates.
  }
}

#Below are the test functions I wrote
#1)
g1<-my.rnorm(10)
g2<-rbinom(5,5,0.1)
g3<-general.rnorm(4)
my.pass.test<-function(z){ # input of this function could be any data set, but we are more intersted in "my.rnorm" or "general.rnorm" with any n you want.
  k<-shapiro.test(z) # this build in shapiro test function is used to test the normality of a given data set.
  k$p.value          # the null hypothesis of the test is that given data is normally distributed
  if (k$p.value>0.05) # if p-value less or equal than 0.05, we reject the test and accept the althernative hypothesis that the data is not normaly distributed
  { return (TRUE)} else {  # when we put the generated normal data into this test function, it should return "true" if indeed we generate an normally distributed data.
    return (FALSE)   #if the inputs are x,y and k respectively, the output of the function should be "TRUE", "FALSE" and "TRUE" respectively.
  }                  #however, this shapiro test can only work for n from 3 to 5000, this indeed a limitation.
}
#2)#Debuging:
#for the function "my.rnorm"at first, when I run the function n=5,and using the default mean and variance, the output is always [1,1,1,1,1],i tried the different n but the problem does not solve. 
#so I checked my function, i deleted the line which is used to convert default mean and standard deviation to to any given mean and standard deviation. Then problem solved.
# so I know the bug must appear on that line, indeed, i made the wrong way. I myltiplied by mean and added the sd, so when sd=0,mean=1, what i have is alwalys 1.
# I changed, I got the correct answer.
# if i do not use the default mean and variance, i will always think everything is right.
#This function below print all the test result from n=1 to n=m for "my.rnorm".So we can see the output vistually to aviod this type of error.
my.pass.test1<-function(m){ # input m is just a number n.
  for (i in (1:m)){ 
    a<-my.rnorm(i,0,1)
    print(i)
    print(a)
  }
}

