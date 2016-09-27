
###### Assignment 1 ######

#-----------------------------1ST PART(GENERATING DEVIATES)----------------------------------------------

#Generate n random deviates from the distribution~N(mean,sd^2) using methods:
#1)Marsalia and Bray algorithm
#2)Box-Mueller algorithm
#3)Central limit theorem algorithm

#my.rnorm is function for Marsalia and Bray algorithm 
#general.rnorm is function for Marsalia and Bray, Box-Mueller, Central Limit theorem algorithm


##### my.rnorm #####---------------------------------------------------------
#(Box-Mueller algorithm )

#inputs:
#1)n is number deviates to be generated 
#2)mean is the mean normal distribution (default is 0)
#3)sd is the standard deviation of normal distribution (default is 1)

#outputs: 
#1)n random deviates, each sampled from ~N(mean,sd^2)

my.rnorm<-function(n,mean=0,sd=1){
  
  #### ERROR CHECKING PART ####
  
  ##check for missing input n 
  if(missing(n)){
    stop("n is missing with no default")
  }
  
  ##check for vector or non-numeric input in argument list
  arglist<-c(as.list(environment()))
  for(j in arglist){
    #check if any argument is vector
    if(is.vector(j) & length(j)>1){
      stop("one of the inputs is a vector")
    }
    #check if argument is non-numeric eg "character"
    if(!is.numeric(j)){
      stop("one of the inputs is not a number")
    }
    
    #check if any argument is matrix
    if(is.matrix(j)){
      stop("one of the inputs is a matrix")
    }
    
  }
  
  ##check if n is non-natural number eg -10
  if(floor(n)!=n | n<=0){
    stop("n is not a natural number")
  }
  
  ##check if sd is negative or 0
  if(sd<=0){
    stop("sd is negative or 0")
  }
  
  
  #### BODY PART ####
  
  ##initiate ls and m 
  #--ls is empty list to contain deviates
  #-- m is number of pairs to be generated eg if n=3 then m=2 pairs (note: 1 extra deviate is generated)
  ls<-c()
  m<-(n+1)/2
  
  ##loop through to generate m pairs random deviates
  for(i in 1:m){
    #initiate success value
    success<-FALSE
    #generate new u1 and u2 until success criterion matched 
    while(!success){
      u1<-2*runif(1,0,1)-1
      u2<-2*runif(1,0,1)-1
      w<-u1^2+u2^2
      #check success criterion
      if(w<1){
        success<-TRUE
      }
      #create pair of random deviates if success criterion matched 
      if(success){
        v<-sqrt((-2*log(w))/(w))
        x1<-u1*v
        x2<-u2*v
      }
    }
    #append pair of random deviate to list 
    ls<-c(ls,x1,x2)
  }
  
  ##scale all random deviates 
  #--so deviates take distribution with correct mean and sd 
  ls<-ls*sd+mean
  
  ##return n deviates from list of length 2*m 
  #--eg for n=3, m=2 so 4 deviates generated. Only return first 3 deviates
  return(ls[1:n])
}


##### general.rnorm #####---------------------------------------------------------
#(Marsalia and Bray algorithm/ Box-Mueller algorithm/ Central Limit theorem algorithm )

#inputs:
#1)n is number deviates to be generated 
#2)mean is the mean normal distribution (default is 0)
#3)sd is the standard deviation of normal distribution (default is 1)
#4) type of method 1=Marsalia and Bray, 2=Box-Mueller,3=Central Limit (default is 1)

#output:
#1)n random deviates, each sampled from ~N(mean,sd^2)
#notes: 
#1)Method 1 and 2 generate pairs of deviates, whereas, Method 3 generate single deviates 

general.rnorm<-function(n,mean=0,sd=1,method=1){
  
  #### ERROR CHECKING PART ####
  
  ##check for missing input n 
  if(missing(n)){
    stop("n is missing with no default")
  }

  ##check for vector or non-numeric input in argument list
  arglist<-c(as.list(environment()))
  for(j in arglist){
    #check if any argument is vector
    if(is.vector(j) & length(j)>1){
      stop("one of the inputs is a vector")
    }
    #check if argument is non-numeric eg "character"
    if(!is.numeric(j)){
      stop("one of the inputs is not a number")
    }
  }
  
  #check if any argument is matrix
  if(is.matrix(j)){
    stop("one of the inputs is a matrix")
  }
  
  ##check if n is non-natural number eg -10
  if(floor(n)!=n | n<=0){
    stop("n is not a natural number")
  }
  
  ##check if sd is negative or 0
  if(sd<=0){
    stop("sd is negative or 0")
  }
  
  ##check method input either 1,2 or 3
  if(!(method==1|method==2|method==3)){
    stop("method is neither 1 nor 2 nor 3")
  }
  
  #### BODY PART ####
  
  ##initiate ls
  #--ls is empty list to contain deviates
  ls<-c()
  
  ##check type of method and carry out its' algorithm
  if(method==1){
    
    ### method=1 (Marsalia and Bray) ###
    
    #initiate m 
    #-- m is number of pairs to be generated eg if n=3 then m=2 pairs (note: 1 extra deviate is generated)
    m<-(n+1)/2 
    
    #loop through to generate m pairs random deviates
    for(i in 1:m){
      #initiate success value
      success<-FALSE
      #generate new u1 and u2 until success criterion matched 
      while(!success){
        #generate u1 & u2 ~ Uniform[-1,1]
        u1<-2*runif(1,0,1)-1
        u2<-2*runif(1,0,1)-1
        #create w
        w<-u1^2+u2^2
        #check success criterion
        if(w<1){
          success<-TRUE
        }
        #create pair of random deviates if success criterion matched 
        if(success){
          #create v 
          v<-sqrt((-2*log(w))/(w))
          #generate random deviates x1 & x2 ~N(0,1)
          x1<-u1*v
          x2<-u2*v
        }
      }
      #append pair of random deviate to list 
      ls<-c(ls,x1,x2)
    }
    
    
  }else if(method==2){
    
    ### method=2(Box-Mueller) ###
    
    #initiate m 
    #-- m is number of pairs to be generated eg if n=3 then m=2 pairs (note: 1 extra deviate is generated)
    m<-(n+1)/2 
 
    #loop through to generate m pairs random deviates
    for(i in 1:m){
      #generate u1 & u2 ~ Uniform[0,1]
      u1<-runif(1,0,1)
      u2<-runif(1,0,1)
      #generate random deviates x1 & x2 ~N(0,1)
      x1<-sin(2*pi*u1)*sqrt(-2*log(u2))
      x2<-cos(2*pi*u1)*sqrt(-2*log(u2))
      #append pair of random deviate to list 
      ls<-c(ls,x1,x2)
    }  
    
  } else{
    
    ### method=3 (Central Limit) ###
    
    #initiate m 
    m<-n
    
    #loop through to generate m random deviates
    for(i in 1:m){
      #generate 16 u_i's~Unif[0,1]
      u<-runif(16,0,1)
      #generate random deviate x~N(0,1)
      x<-(sum(u)-8)*sqrt(12/16)
      #append random deviate to list
      ls<-c(ls,x)
    }
    
  }
  
  ##scale all random deviates 
  #--so deviates take distribution with correct mean and sd 
  ls<-ls*sd+mean
  
  ##return n deviates from list of deviates
  return(ls[1:n])
  
}

#-----------------------------2ND PART(TESTING)----------------------------------------------

#Test my.rnorm and general.rnorm for:
#1)Error Traps working for faulty inputs (test.input)
#2)Outputs are as expected ie a numeric vector (test.output)
#3)Deviates generated are normally distributed ()
#4)Parameters of deviates do not differ from true parameters eg sample mean

##### test.input #####---------------------------------------------------------
#-- test whether error traps for inputs are working 

#inputs:
#1)FUN is the function being tested (my.rnorm or general.rnorm)
#2)method denotes type of algorithm if FUN=general.rnorm.Ignore if FUN=my.rnorm (default is my.rnorm)


#outputs: 
#1)console print stating whether all inputs are working or not

test.input<-function(FUN=my.rnorm,method=1){
  
  ##initiate n, mean & sd
  n<-3999
  mean<-10
  sd<-2
  
  ##initiate logical list
  #--TRUE states that a test is working, FALSE otherwise
  ls<-c()
  
  ##test inputs based on function ie check if error traps working
  if(identical(FUN,my.rnorm)){
    
    #test for missing input n 
    test.missing.n <- tryCatch(FUN(,mean,sd),error=function(e) return(TRUE))
    #test for vector or non-numeric input
    test.vector.numeric.1<-tryCatch(FUN("characterise me"),error=function(e) return(TRUE))
    test.vector.numeric.2<-tryCatch(FUN(n,c(1,1)),error=function(e) return(TRUE))
    test.vector.numeric.3<-tryCatch(FUN(n,mean,NA),error=function(e) return(TRUE))
    #test for matrix input
    test.matrix<-tryCatch(FUN(c(1,2,3)),error=function(e) return(TRUE))
    #test for n is non-natural number 
    test.natural.n<-tryCatch(FUN(0.1),error=function(e) return(TRUE))
    #test for sd is negative or zero
    test.sd<-tryCatch(FUN(n,mean,-2),error=function(e) return(TRUE))
    
    #append TRUE values to vector ie test returning error
    ls<-c(test.missing.n,test.vector.numeric.1,test.vector.numeric.2,test.vector.numeric.3,test.matrix,test.natural.n,test.sd)
    #check if enough test have returned errors
    if(length(ls)==7 & all(ls==TRUE)){
      return("All input tests are working")
    }else{
      return("Not all input tests are working")
    }
    
  }else if(identical(FUN,general.rnorm)){
    
    #test for missing input n 
    test.missing.n <- tryCatch(FUN(,mean,sd,method=method),error=function(e) return(TRUE))
    #test for vector or non-numeric input
    test.vector.numeric.1<-tryCatch(FUN("characterise me",method=method),error=function(e) return(TRUE))
    test.vector.numeric.2<-tryCatch(FUN(n,c(1,1),method=method),error=function(e) return(TRUE))
    test.vector.numeric.3<-tryCatch(FUN(n,mean,NA,method=method),error=function(e) return(TRUE))
    #test for matrix input
    test.matrix<-tryCatch(FUN(c(1,2,3)),error=function(e) return(TRUE))
    #test for n is non-natural number 
    test.natural.n<-tryCatch(FUN(0.1,method=method),error=function(e) return(TRUE))
    #test for sd is negative or zero
    test.sd<-tryCatch(FUN(n,mean,-2,method=method),error=function(e) return(TRUE))
    #test method input is not 1 or 2 or 3
    test.method<-tryCatch(FUN(n,method=5),error=function(e) return(TRUE))
    
    #append TRUE values to vector ie test returning error
    ls<-c(test.missing.n,test.vector.numeric.1,test.vector.numeric.2,test.vector.numeric.3,test.matrix,test.natural.n,test.sd,test.method)
    #check if enough test have returned errors
    if(length(ls)==8 & all(ls==TRUE)){
      return("All input tests are working")
    }else{
      return("Not all input tests are working")
    }
  }else{
    return("test is not identifiable for FUN input")
  }
  
}


##### test.output #####---------------------------------------------------------
#-- test output is a numeric vector

#inputs:
#1)FUN is the function being tested (my.rnorm or general.rnorm)
#2)method denotes type of algorithm if FUN=general.rnorm. Ignore if FUN=my.rnorm (default is my.rnorm)


#outputs: 
#1)numeric vector of length n

test.output<-function(FUN=my.rnorm,method=1){
  
  ##initiate n, mean & sd
  n<-3999
  mean<-10
  sd<-2
  
  ##generate deviates based on function 
  if(identical(FUN,my.rnorm)){
    
    ### my.rnorm ###
    
    #generate deviates with my.rnorm
    dev<-FUN(n,mean,sd)
    #check if output is a vector of correct length n and vector is numeric
    if(length(dev)==n & is.numeric(dev)){
      return("Output is valid")
    }else{
      return("Output is invalid")
    }
  }else if(identical(FUN,general.rnorm)){
    
    ### general.rnorm ###
    
    #generate deviates with general.rnorm
    dev<-FUN(n,mean,sd,method=method)
    #check if output is a vector of correct length n and vector is numeric
    if(length(dev)==n & is.numeric(dev)){
      return("Output is valid")
    }else{
      return("Output is invalid. Output is either not of length n or non-numeric ")
    }
    
  } else{
    return("test is not identifiable for this input")
  }
}


##### test.norm #####---------------------------------------------------------
#-- test deviates are normally distributed ~N(mean,sd^2)

#inputs:
#1)FUN is the function being tested (my.rnorm or general.rnorm)
#2)method denotes type of algorithm if FUN=general.rnorm. Ignore if FUN=my.rnorm (default is general.rnorm)


#outputs: (a named list of)
#1)shapiro wilk test (5%)
#2)kolmogorov smirnov (5%)
#3)68-95-99.7 rule 
#4)histogram 

test.norm<-function(FUN=general.rnorm,method=1){
  
  #### SET UP PART ####
  
  ##initiate n, mean & sd
  n<-3999
  mean<-10
  sd<-2
  
  ##generate deviates based on function 
  if(identical(FUN,my.rnorm)){
    dev<-FUN(n,mean,sd)
  }else if(identical(FUN,general.rnorm)){
    dev<-FUN(n,mean,sd,method=method)
  }else{
    return("test is not identifiable for FUN input")
  }
  
  #### TEST PART ####
  
  ##Shapiro wilk (5% significance)
  #--test whether deviates come from normal distributed population
  test.shapiro<-shapiro.test(dev)
  if(test.shapiro$p.value<0.05){
    test.shapiro.result<-"FAIL. Sampled deviates are not normal"  
  }else{
    test.shapiro.result<-"TRUE. Sampled deviates are normal" 
  }
  
  ##Kolmogorov Smirnov  (5% significance)
  #--test empirical CDF with CDF of normal ~N(mean,sd)
  #--aka 1 sample kolmogorov test
  test.kolmogorov<-ks.test(dev,"pnorm",mean,sd)
  if(test.kolmogorov$p.value<0.05){
    test.ks.result<-"FAIL. Sampled deviates DIFFERENT normal distribution "  
  }else{
    test.ks.result<-"TRUE. Sampled deviates NOT DIFFERENT normal distribution" 
  }
  
  #### OBSERVATION TEST PART ####
  #--test based on observation of user
  
  ##68-95-99.7 rule
  #1)expect middle 68%(16%-84%) to be 1 sd from mean 
  #2)expect middle 95%(2.5%-97.5%) to be 2 sd from mean 
  #3)expect middle 99.7%(0.15%-99.85%) to be 3 sd from mean
  
  #quantiles using percentage of data
  quants.dat<-quantile(dev,c(0.5-0.4985,0.5-0.475,0.5-0.34,0.5+0.34,0.5+0.475,0.5+0.4985))
  #true quantiles from known sd and mean
  quants.theory<-c(-3,-2,-1,1,2,3)
  quants.theory<-sd*quants.theory+mean
  #combine rows quants.dat and quants.theory 
  quants.out<-rbind(quants.dat,quants.theory)
  
  ##plot histogram
  hist(dev)
  
  #### OUTPUT PART ####
  
  ##output results
  return(list(shapiro.test=test.shapiro.result ,kolmogorov.test=test.ks.result,quantiles=quants.out))
  
}


##### test.param #####---------------------------------------------------------
#--test equality of sampled parameters with true parameters
#--these test requires deviates to have normality ie check test.norm first

#inputs:
#1)mean is the mean normal distribution (default is 0)
#2)sd is the standard deviation of normal distribution (default is 1)
#3)FUN is the function being tested (my.rnorm or general.rnorm)
#4)method denotes type of algorithm if FUN=general.rnorm. Ignore if FUN=my.rnorm (default is general.rnorm)

test.param<-function(mean=0,sd=1,FUN=general.rnorm,method=1){
  
  #### SET UP PART ####
  
  ##initiate n
  n<-3999
  
  ##generate deviates based on function 
  if(identical(FUN,my.rnorm)){
    dev<-FUN(n,mean,sd)
  }else if(identical(FUN,general.rnorm)){
    dev<-FUN(n,mean,sd,method=method)
  }else{
    return("test is not identifiable for FUN input")
  }
  
  #### TEST PART #### 
  
  ##t-test (5% significance)
  #--test difference of sample mean from true mean (true sd is unknown)
  #--assumes samples are normal
  
  #2-tail p-value
  pvalue.t<-t.test(dev,mu=mean,alternative="two.sided")$p.value
  
  #check if p-value is significant at 5% level
  if(pvalue.t<0.05){
    t.test.result<-"Sample mean is DIFFERENT from true mean "
  } else{
    t.test.result<-"Sample mean is NOT DIFFERENT from true mean "
  }
  
  ##Chi Squared(5% significance)
  #--test for difference of sample variance from true variance
  #--assumes samples are normal
  
  #create test statistic
  chi<-(n-1)*var(dev)/sd^2
  
  #2-tail p-value (assumes symmetrical distribution)
  pvalue.chi<-pchisq(chi,n-1)
  if(pvalue.chi>1-pvalue.chi){
    pvalue.chi<-2*(1-pvalue.chi)
  }else{
    pvalue.chi<-2*(pvalue.chi)
  }
  
  #check if p-value is significant at 5% level
  if(pvalue.chi<0.05){
    chi.test.result<-"Sample variance is DIFFERENT from true variance"
  } else{
    chi.test.result<-"Sample variance is NOT DIFFERENT from true variance"
  }
  
  ##F test (5% significance)
  #--test for difference of sample variance(from function) with sample variance(from rnorm)
  #--assumes both samples are normal
  
  #generate n deviates using rnorm
  test.dat<-rnorm(n,mean,sd)
  
  #p-value
  pvalue.F<-var.test(dev,test.dat,alternative="two.sided")$p.value
  
  #check if p-value is significant at 5% level
  if(pvalue.F<0.05){
    F.test.result<-"Sample variances are DIFFERENT"
  } else{
    F.test.result<-"Sample variances are NOT DIFFERENT"
  }
  
  #### OUTPUT PART ####
  
  #output tests
  return(list(t.test=t.test.result,chi.test=chi.test.result,F.test=F.test.result))
  
}


##### References #####---------------------------------------------------------
#1)http://stackoverflow.com/questions/11885207/get-all-parameters-as-list
#-- collect values of arguments as a list
#2)http://wps.prenhall.com/wps/media/objects/9431/9657451/Ch_12/levine-smume6_topic_12-07.pdf
#--test difference of sample variance from true variance


