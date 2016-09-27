# I confirm that attached is my own work, except where clearly indicated in the text

my.rnorm<-function(n, mu = 0, sd = 1) {
  
  ###### Argument error checking ######
  
  if (n>=40) warning ("Chosen n may be too large to compute within reasonable time via Marsaglia and Bray's method")
  if (n<0) stop ("Invalid arguments - number of deviates to generate cannot be negative")
  if (n==0) stop ("Invalid arguments - number of deviates to generate cannot be zero")
  if (sd<0) stop ("Invalid arguments - Standard Deviation (sd) must not be negative")
  if (sd==0) stop ("Invalid arguments - Standard Deviation (sd) is zero iff all pseudo-random deviates equil to zero")
  if (!n%%1==0) stop ("Invalid arguments - n is not in integer form")
  if (!is.numeric(n)) stop ("Invalid arguments - n input is not numeric")
  if (!is.numeric(mu)) stop ("Invalid arguments - Mean (mu) input is not numeric")
  if (!is.numeric(sd)) stop ("Invalid arguments - Standard Deviation (sd) input is not numeric")
  if (length(n)>1) stop ("Invalid arguments - n is not scalar")
  if (length(mu)>1) stop ("Invalid arguments - Mean (mu) input not in scalar form")
  if (length(sd)>1) stop ("Invalid arguments - Standard Devidation (sd) input not in scalar form")
  
  ###### Loop generating uniform distribution variates that pass the rejection step ######

  repeat { 
    vault<-as.matrix(runif(2*n, min=0, max=1)) # create a matrix with simulated uniform variates pairs
    vault<-(2*vault-1) # transform uniform distr. variates to unit square variates
    vault<-cbind(vault[1:n,1],vault[(n+1):(2*n),1]) # relocate matrix's values into 2 rows 
    w_vault<-matrix(NA, nrow=n, ncol=1) 
    for (i in 1:n) { # the loop process is trivial: 
      w_vault[i,1]<-(vault[i,1]^2+vault[i,2]^2) # 1. use uniform distr. variates to create a matrix of w values via the given formula
    }
    for (i in 1:n) {
      if (w_vault[i,1]>1) { # 2. condition for modifying specific w's in the matrix <=1
        w_vault[i,1]<--1000 # 3. assign new numeric values to improper values
      }
    }
    if (sum(w_vault)>-10) break # 4. summation condition for loop to end
  }
  v_vault<-sqrt((-2*log(w_vault))/w_vault) # create a matrix of v values via the given formula 
  x_vault<-(as.matrix(cbind(vault[1:n,1]*v_vault[1:n,1],vault[1:n,2]*v_vault[1:n,1])))*sd+mu # create a matrix of x values using v values and uni. distr. variates + tether x values to other function arguments 
  x_vault<-c(t(x_vault)) # transpose the matrix of x's and vectorise it - this method preserves indendent pairs of X when calling values from vector by order
  rem<-x_vault[1:n] # define x values to remove
  my.answer<-x_vault[! x_vault %in% rem] # remove first half of obtained x values, use 2nd half as an output of function
  return(my.answer)
}




general.rnorm<-function(n, mu = 0, sd = 1, M = 1) {

 ###### Argument error checking ######

 if (n>=40) warning ("Chosen n may be too large to compute within reasonable time via Marsaglia and Bray's method")
 if (n<0) stop ("Invalid arguments - number of deviates to generate cannot be negative")
 if (n==0) stop ("Invalid arguments - number of deviates to generate cannot be zero")
 if (sd<0) stop ("Invalid arguments - Standard Deviation (sd) must not be negative")
 if (sd==0) stop ("Invalid arguments - Standard Deviation (sd) is zero iff all random deviates equil to zero")
 if (!n%%1==0) stop ("Invalid arguments - n is not in integer form")
 if (!M%%1==0) stop ("Invalid arguments - Method (M) is not in integer form")
 if (!is.numeric(n)) stop ("Invalid arguments - n input is not numeric")
 if (!is.numeric(mu)) stop ("Invalid arguments - Mean (mu) input is not numeric")
 if (!is.numeric(sd)) stop ("Invalid arguments - Standard Deviation (sd) input is not numeric")
 if (!is.numeric(M)) stop ("Invalid arguments - Method (Method) input is not numeric")
 if (M<=0) stop ("Invalid arguments - Method (M) input cannot be negative or zero") 
 if (M>3) stop ("Invalid arguments - Method (M) input can take values 1,2,3 only")
 if (length(n)>1) stop ("Invalid arguments - n is not scalar")
 if (length(mu)>1) stop ("Invalid arguments - Mean (mu) input not in scalar form")
 if (length(sd)>1) stop ("Invalid arguments - Standard Devidation (sd) input not in scalar form")
 if (length(M)>1) stop ("Invalid arguments - Method (M) input not in scalar form")
 
 if (M==1) { # Same code for Marsaglia and Bray's algorithm as seen above
  repeat { 
    vault<-as.matrix(runif(2*n, min=0, max=1))
    vault<-cbind(vault[1:n,1],vault[(n+1):(2*n),1])
    vault<-2*vault-1
    w_vault<-matrix(NA, nrow=n, ncol=1)
    for (i in 1:n) {
      w_vault[i,1]<-(vault[i,1]^2+vault[i,2]^2)
    }
    for (i in 1:n) {
      if (w_vault[i,1]>1) {
        w_vault[i,1]<--1000000
      }
    }
    if (sum(w_vault)>-1000) break
  }
  v_vault<-sqrt((-2*log(w_vault))/w_vault)
  x_vault<-(as.matrix(cbind(vault[1:n,1]*v_vault[1:n,1],vault[1:n,2]*v_vault[1:n,1])))*sd+mu
  x_vault<-c(t(x_vault))
  rem<-x_vault[1:n]
  my.answer<-x_vault[! x_vault %in% rem]
 }
 if (M==2) { # Code for Box-Mueller algorithm 
   sack<-as.matrix(runif(2*n, min=0, max=1)) # create a matrix containing random uniform distr. variates
   sack<-cbind(sack[1:n,1],sack[(n+1):(2*n),1]) # glue equil (both nrow=n) matrix's parts = 2 columns of variates = pairs for future x values
   x_sack<-matrix(NA, nrow=n, ncol=2)
   for (i in 1:n) {
    x_sack[i,1]<-(sin(2*pi*sack[i,1])*sqrt(-2*log(sack[i,2]))) # Generate x value via the given formula
    x_sack[i,2]<-(cos(2*pi*sack[i,1])*sqrt(-2*log(sack[i,2]))) 
   }
   x_sack<-x_sack*sd+mu # tether x values to other function arguments
   x_sack<-c(t(x_sack)) # transpose and vectorise matrix to preserve pairs as in method 1 (see my.rnorm expln)
   rem<-x_sack[1:n]
   my.answer<-x_sack[! x_sack %in% rem] # remove half of the generated values
 }
 if (M==3) { # Code for Central Limit Theorem method
   stash<-matrix(NA, nrow=16, ncol=n)
   x_stash<-matrix(NA, nrow=1, ncol=n)
   for (i in 1:n)
     stash[1:16,i]<-(runif(16, min=0, max=1)) # fill up the matrix with values of uniform distributed variates
     x_stash<-colSums(stash, na.rm=FALSE, dims=1) # fill up the matrix with sums of each column in marix defind above
     x_stash<-((x_stash-8)*sqrt(12/16))*sd+mu # calculate x values via given formula
     my.answer<-as.vector(x_stash)
 }
 return(my.answer)
}


###### Function tests ######


normality.test.1<-function(mu=0, sd=1, M) { # Perform Shapiro-Wilk Normality test - Null Hypo = the data is normally distributed vs. data is normally distributed
if (M==1) {
  a<-as.list(shapiro.test(general.rnorm(39,mu,sd,1))) # create a list with values from info out of shapiro test function
}
if (M==2) {
  a<-as.list((shapiro.test(general.rnorm(5000,mu,sd,2))))
}
if (M==3) {
  a<-as.list((shapiro.test(general.rnorm(5000,mu,sd,3))))
}
ifelse((a$p.value<=0.01), b<-print("The test null hupotheis (x values are normally distributed) - rejected"), b<-print("The test null hypothesis (x values are normally distributed) - holds"))
  return (b)
}







  

