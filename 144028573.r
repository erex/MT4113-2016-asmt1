#Assignment 1
#I confirm that the attached is my own work, except where clearly indicated in the text
my.rnorm <- function (n, mean=0, sd=1) {      
      
      #Purpose:
      #Generate n number of pseudo-random normally distributed values with mean and sd (stated in arguments)
      #from pairs of uniform random variables, through the Marsaglia and Bray method
      
      #Inputs
      #N - Number of observations we want to obtain
      #mean - by default 0
      #sd - by default 1
      
      #Output
      #A vector with n number of observations distributed approximately as ~N(mean,sd)
      
      #########################################################################################
      
      #Test for input errors        
      if(n%%1 != 0) stop("Invalid arguments") 
      if(n<0){    
            stop("Invalid arguments")
      }
      if(sd < 0){
            stop("Invalid arguments")
      }      
      #########################################################################################

            pair.runif <- function(){                 
                  unif.pairs <- runif(2,0,1)          #One pair of random uniform variables with mean=0, sd=1
                  ui.square <- 2*unif.pairs-1         #Transform the unif. variables to be distributed on the range [-1,1]
                  s <- ui.square[1]^2+ui.square[2]^2  #Make unif.variables fit inside unit circle center=0, radius=1
                  
                  while (s > 1){                      #Rejection Step. if S is greater than 1. If FALSE reject the pair
                        unif.pairs <- runif(2,0,1)    #pair of uniform R values and generate a new pair 
                        ui.square <- 2*unif.pairs-1   
                        s <- ui.square[1]^2+ui.square[2]^2
                  }                                   #if the statement is TRUE, the while loop will finish
                  v<- sqrt(((-2*log(s))/s))           #define v through the algorithm formula
                  nvariables <- ui.square*v           #define the normal random values as the transformed uniform random values times v
            }
      if (n%%2==0) {                                                    #n as an even number
            n.square.rvariables <- replicate(n/2, expr=pair.runif())    #replicate function (n/2 times to generate each time 1 pair, thus n observations)
            n.square.rvariables
            r.norm.values <- (n.square.rvariables*sd)+mean              #Transform into n normally distributed values mean=mu sd=sigma
            return(r.norm.values)                                       #by multiplying by the sd and adding the mean (inputed in the rnorm function)
      }  else {
            n.square.rvariables <- replicate(n+1, expr=pair.runif())    #When inputing n as an odd number, replicate function (n+1 times to generate full pairs)
            n.square.rvariables.sample <- sample(n.square.rvariables, n)#Sample form the full pairs to select n values we want to return
            r.norm.values <- (n.square.rvariables.sample*sd)+mean       #Repeat the transformation into n normally distributed values of mean=mu and sd=sigma
            return(r.norm.values)
      }
}



general.rnorm <- function (n, mean=0, sd=1, method=1) { 
       
      #Purpose
      #Generate n number of pseudo-random normally distributed values with mean and sd (stated in arguments)
      #from pairs of uniform random variables, through one of the three methods:
            #1.the Marsgaglia and Bray algorithm
            #2.the Box-Mueller algorithm
            #3.the Central Limit Theorem algorithm
      
      #Inputs
      #N - Number of observations we want to obtain
      #mean - by default 0
      #sd - by default 1
      #method - by default 1 (Marsaglia Bray), could take values 1,2,3 depending on the algorithm used
      
      #Output
      #A vector with n number of observations distributed approximately as ~N(mean,sd)
      
      #######################################################################################
      
      #Test for input errors
      
      if(missing(n)) stop("Invalid arguments")
      if(n%%1 != 0) stop("Invalid arguments")
      if(n<0){
            stop("Invalid arguments")
      }
      if(sd < 0){
            stop("Invalid arguments")
      } 
      if(method!=1&& method!=2&& method!=3) stop("Invalid arguments")
      #########################################################################################

      #When method is 1, Marsaglia and Bray algorithm will be generated, calling the function rnorm()
      if (method==1){
            marsaglia.b <- my.rnorm(n, mean, sd)
            return(marsaglia.b)
            }
            
      
      #When method is 2, Box and Mueller algorithm will be generated
      if (method==2){
            box.random <- function(){                                               #Generate a pair of random normally dist. values
                  box.runif <- runif(2,0,1)                                         #A pair of uniform distributed values
                  box.rnorm1 <- (sin(2*pi*box.runif[1]))*sqrt(-2*log(box.runif[2])) #transform 1st value according to formula
                  box.rnorm2 <- (cos(2*pi*box.runif[1]))*sqrt(-2*log(box.runif[2])) #transform 2nd value according to formula
                  box.results <- c(box.rnorm1, box.rnorm2)                          #Return a vector with the pair transformed
            }
            if (n%%2==0){                                                     #if n is even, same reasoning as with my.rnorm function
                  box.random.n <- replicate(n/2, expr=box.random())           
                  box.random.n.tr <- (box.random.n*sd)+mean                   
                  return(box.random.n.tr)
                  } else{
                  box.random.n <- replicate(n+1, expr=box.random())           #if n is odd
                  box.random.n.smpl <- sample(box.random.n, n)                
                  box.random.n.smpl.tr <- (box.random.n.smpl*sd)+mean         
                  return(box.random.n.smpl.tr)
                  } 
      }
      #When method is 3, Central Limit Theorem algorithm will be generated 
      if (method==3){
            clt.results <- vector(mode="numeric", length=n) #Empty vector to store the result of our loops
            for (i in 1:n){                                 #for i in n observations we want to create
                  clt.runif <- runif(16,0,1)                #we generate 16 uniform random values
                  clt.sum <- sum(clt.runif)                 #summing them to create clt.sum
                  x.def <- (clt.sum-8)*(sqrt(12/16))        #applying the formula to generate x.def (generate the approximate mean of the 16 unif. random values)
                  clt.results[i] <- x.def
            }
            clt.results
            clt.results.tr <- (clt.results*sd)+mean         #transforming for mean=mu and sd=sigma
            return(clt.results.tr)
      } 
} 

#TEST FOR NORMAL RANDOM VALUES
#TEST 1 - Create a function to compare the graphs of the three algorithms used to generate the 
#normal pseudo-random values to the normal density function generated randomly
graphs.comp <- function(n, mean=0, sd=1){                   
      norm.graph <- rnorm(n,mean,sd)                        #1. For each method of creating pseudo-values,
      marsaglia.br <- my.rnorm(n, mean, sd)                 #generate a vector
      box.mueller <- general.rnorm(n, mean, sd, method=2)
      clt <- general.rnorm(n, mean, sd, method=3)
      
      
      hist(norm.graph, prob=TRUE)                           #2. Plot a histogram with the values from rnorm 
      lines(density(norm.graph), col="dark green", lwd=2)   #3. Add subsequent lines for each set of values
      lines(density(marsaglia.br), col="blue")              #generated by the previous functions
      lines(density(box.mueller), col="red")
      lines(density(clt), col="yellow")
} 

      #INTERPRETATION OF TEST 1: As N increases, we can see the lines for all distributions converge to the 
      #generated by the rnorm(dark green). The values are normally distributed

#TEST 2 - Testing with the Shapiro Test for normality 
#reference: Used help file ?Shapiro Test
shapiro.t <- function(n, mean=0, sd=1){                                 
      norm.shap <- shapiro.test(rnorm(n,mean,sd))                       #For each set of pseudo-values generated through rnorm and 
      mar.bra.shap <- shapiro.test(my.rnorm(n, mean, sd))               #each of the three methods described in general.rnorm, apply 
      box.mu.shap <- shapiro.test(general.rnorm(n, mean, sd, method=2)) #the Shapiro Test of normality, to test whether values are normal or not
      clt.shap <- shapiro.test(general.rnorm(n, mean, sd, method=3))
      compare.shap <- list(norm.shap, mar.bra.shap, box.mu.shap, clt.shap) #Include all elements inside a list
      return(compare.shap)
}
      #INTERPRETATION OF TEST 2: We are testing against the null hypothesis that the values are normally distributed
      #If the p.value is > 0.05(confidence level), it can be said that you cannot reject the hypothesis that the values
      #from the sample created follow a normally distributed function

#TEST 3 - Testing for the qqnorm graph
#reference: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/qqnorm.html 
qqnorm.compare <- function(n, mean=0, sd=1){                            
      par(mfrow=c(1,3))                                                       #Divide the plot into three
      x <- rnorm(n,mean,sd)                                                   #Generate a line which represent a normally distributed sample
      marsaglia <- qqnorm(my.rnorm(n, mean, sd)); qqline(x)                   #Fill the data for each of the three methods into the qq plot
      boxmuell <- qqnorm(general.rnorm(n, mean, sd, method=2)); qqline(x)     #adding as a line the normal values generated in x
      cltheorem <- qqnorm(general.rnorm(n, mean, sd, method=3)); qqline(x)
}
      #INTERPRETATION OF TEST 3: We are plotting each set of pseudo-normal values into the qqnorm plot, 
      #thus comparing each, in turn, to a line representing the quantiles distribution for a normal values
      #generated randomly. As N increases, the fit of the values is closer to being a straight line, 
      #following the normal distribution line.