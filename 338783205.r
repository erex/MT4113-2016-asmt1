
#I confirm that the attached is my own work, except where clearly indicated in the text


#----------------------General.rnorm-----------------------------------------------#

general.rnorm<-function(n,mu=0,sd=1,method=1){
  if(missing(n)||(length(n)!=1)){     # make sure n is not missing and its a scalar
    stop("Invalid arguments")
  }
  else if((n<0)||(n%%1!=0)){      #check if n is a positive integer               
    stop("Invalid arguments")
  }
  else if (sd<0){         #sd needs to be >0
    stop("Invalid arguments")
  }
  else if((method==1)||(method==2)){
    mb.box.function(n,mu,sd,method) ## implement either mb or box muller 
  }
  else if (method==3) {
    central.limit.algorithm.nvalues(n,mu,sd)# this is seperated as box-muller and marsiglay
    #bay produces only pairs (even values) , odd values are subsquently produced from them
    #,where as the CTL algorithm takes  n odd or even
  }
  else if((method!=1)||(method!=2)||(method!=3)){
    stop("invalid args") # when method is not 1,2 or 3
    
  }
  
}

## central limit theorem algorithm
central.limit.algorithm.nvalues<-function(n,mu=0,sd=1){
  central.limit.algorithm<-function(){
    U_i<- runif(16,0,1) ##generate 16 uniformly distributed values
    X_i<-(sum(U_i)-8)*sqrt(12/16) ## make the transformation.
    return(X_i) ##return X_i
    
  }
  
  X_n<-replicate(n,central.limit.algorithm()) ##call the function n times and return as alist
  vecX_n<-as.vector(X_n)
  tansform.vecX_n<- vecX_n*sd+mu
  #message("ctl:",vecX_n)
  return (tansform.vecX_n)
}





######marsaglia bay and Box muller algorithm###
mb.box.function<-function(n,mu=0,sd=1,method){
  if (n%%2==0){   ##if n is even
    
    evenvec<-gen.even.function(n,method) ## return as a vec as ge.even.function returns a vector
    transf.even.vec<-evenvec*sd+mu  #transform them as using sd and mu
    return(transf.even.vec)
  }
  #
  ##if n is  odd##  
  
  else{
    evenlist<-gen.even.function(n+1,method) # create n+1 no's and pick n values 
    oddlist<-sample(evenlist,n,replace = FALSE) 
    transf.oddlist<-oddlist*sd+mu
    return(transf.oddlist)
  }
}




##########define the functions we need#######
gen.even.function<-function(n,method){
  ##if n is even
  
  if(method==1){ 
    
    
    ###marsaglia and bray algorithm###
    mbalgorit<-function(){  #runs the algorithm once and generates a pair
      
      u<-runif(2,0,1) #generate n uniformly distributed numbers
      u<-2*u-1      #convert to a unit square
      
      ### #Implement the rejection step:#####
      w<-(u[1])^2+(u[2])^2
      
      while(w>1){      #reject values until the pair of values lies within the unit circle
        ####if w>1, implement the algorithm again until w<1####
        
        u<-runif(2,0,1) 
        u<-2*u-1      
        ### #Implement the rejection step:#####
        w<-(u[1])^2+(u[2])^2
      }
      # message("W<1:",(u[1])^2+(u[2])^2<1)  ## to check the values uncomment this line
      ###Define  v######
      v<-sqrt((-2*log(w))/w)
      
      ###Define pair of normally distributed values##
      x<-u*v
      # message("MB algo",w)    ## to check  , if the function is in the right loop for method==1
      
      return(x)  ### return x
      
      
    }
    a<- replicate(n/2,mbalgorit()) ##generate even values
    return(as.vector(a))
  }
  else if (method==2){ ###box mueller algorithm###
    boxmulalg<-function()
    {
      v_1<-runif(2,0,1)
      X_1<-sin(2*pi*v_1[1])*sqrt(-2*log(v_1[2])) ##  find the first transformed variate
      X_2<-cos(2*pi*v_1[1])*sqrt(-2*log(v_1[2])) 
      boxmvec<-c(X_1,X_2)     ### return transformed list
      #  message("boxalg:",X_2)
      return(boxmvec)
    }
    b<- replicate(n/2,boxmulalg()) 
    return(as.vector(b))
  }
}
#---------------END of general.rnorm()---------------------------------------------#



#------------------my.rnorm----------------------------------------------------------#
my.rnorm<-function(n,mu=0,sd=1){
  if(missing(n)||(length(n)!=1)){                                #error statement
    stop("invalid arguments")
  }
  else if ((n<0)||(n%%1!=0)){           #check for positive integers
    stop("invalid arguments")
  }
  else if (sd<0){
    stop("invalid arguments")
    
  }
  
  else if (n%%2==0){   ##if n is even call the even function 
    
    evenvec<-even.function(n) ## return as a
    transf.even.vec<-evenvec*sd+mu  #transform them as using sd and mu
    return(transf.even.vec)
  }
  
  ##if n is  odd call the even fucntion generate n+1 values and pick n values##  
  
  else{
    evenlist<-even.function(n+1) # create n+1 no's and pick n values 
    oddlist<-sample(evenlist,n,replace = FALSE) 
    transf.oddlist<-oddlist*sd+mu
    return(transf.oddlist)
  }
  
}




##define the even functions  from the Marsaglia bay and Box-muller algorithm need###
even.function<-function(n){
   mbalgorit<-function(){  #runs the algorithm once and generates a pair
    
     u<-runif(2,0,1) #generate n uniformly distributed numbers
    u<-2*u-1      #convert to a unit square
    
    ### #Implement the rejection step:#####
    w<-(u[1])^2+(u[2])^2
    
    while(w>1){      #reject values until the pair of values lies within the unit circle
      ####if w>1, implement the algorithm again until w<1####
      
      u<-runif(2,0,1) 
      u<-2*u-1      
      ### #Implement the rejection step:#####
      w<-(u[1])^2+(u[2])^2
    }
    ##message("W<1:",(u[1])^2+(u[2])^2<1)  ## check the values
    ###Define  v######
    v<-sqrt((-2*log(w))/w)
    
    ###Define pair of normally distributed values##
    x<-u*v
    
    return(x)  ### return x
    
    
  }
  
  
  ### Generate value according to user input and return as user list###
  
  a<- replicate(n/2,mbalgorit()) #n/2 as algorithm returns pairs
  return(as.vector(a))
}

#-----------------------------End of my.rnorm------------------------------------#


#--------------------------------Testfucntions-------------------------------------#
##Test func to check if functions plot a normal distribution for large values of n###

#*****testfunc1.hist ##
# plot hist of each of my functions and visually check how closely it resembles the normal
#distribution using the rnorm function
#and then check a  my.rnorm(1000);my.rnorm(1001) 
#and should resemble  testfunc1.hist(rnorm(1000));testfunc1.hist(rnorm(1001))

#similarly check for general.rnorm(1000)[method =1,2,3] and diffent translations of mu
testfunc1.hist<-function(func)
{
  hist(func)
  
} 

#---------Result of testfunc1.hist----------------#
#The testfunc1.hist produces noormally distributed historgrams
#for large values n ==(100,1000..)
#This remains true for both functions for n odd&even in 
#my.rnorm and general.rnorm(method=1,2,3)
#---------------------------------------------------#



##testfunc2.qnorm###
#draw qq plots to see if the data you produce lie along the qqline
#again check this for the my.rnorm(1000),my.rnorm(1001)
#similarly check for general.rnorm(1000)[method =1,2,3] and diffent translations of mu
testfunc2.qqnorm<-function(func)
{
  qqnorm(func)
  qqline(func,col="green")
}
#------Results for testfunc2.qqnorm----------------------##
#Having tested , for my.rnorm for odd ,even  "n" and 
#similary mu and sd , the the Q1-Q3 values lie closely on the qq line 
# This remains true for general.rnorm for method 1,2,3
#-------------------------------------------------------###

#-------------------------end of Testfuncs------------------------------------------#

