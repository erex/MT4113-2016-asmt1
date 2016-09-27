#I confirm that the attached is my own work, except where clearly indicated in the text.

#Marsaglia and Bray's method

set.seed(seed=13)    #reproduce the same random objects to help me evaluate my tasks
U <- runif(2, min=0, max=1)    #part1  #generate a pair of uniformly distributed random deviates

#my.U function will be using for transformation of a pair of uniformly distribued random deviates to unit square #by my.U(U) 
my.U <- function(X){        #the function has one argument without default, X, number of values to return 
        Y <- 2*X-1          #part2   #transformation               
        return(Y)
}

#my.W function creates 'w' (with criteria)   #part3
my.W <- function(X){     #the function has one argument without default, X, number of values to return     
        Ui <- my.U(X)        #transformed U's will be using for w, so i called back my.U function
        w <- sum(Ui^2)       #part3   # w=U1^2+U2^2  equation of w 
        while (w > 1){                       #criteria: if w>1, 
                U2 <- runif(2, min=0, max=1)   #it will keep creating a new pair of uniformly distribued random deviates
                Ui <- my.U(U2)                 #and transform the new pair
                w <- sum(Ui^2)                 #and calculate the new w if they meet the criteria
        }
        Z <- w
        return(Z)
}

#my.v function creates 'v'   #part3
my.v <- function(X){         #the function has one argument without default, X, number of values to return
        v <- sqrt( ( -2*log(X) ) /X)   #equation of v
        return(v)
}

#my.rnorm function returns a vector of pseudo-random values from a normal distribution and has three arguments. 
        #The first argument is the number of values to return (no default).
        #The second argument is mean (default=0)
        #The third argument is standart deviation (default=1)
my.rnorm <- function (n, mu=0, sigma=1){    
        n2 <- n/2               #to generate n number of simulations, n/2 simulations will be taken because Marsaglia algorithm produces a pair
                                #if n is an odd number, R will round that odd number to the below even number. 
        my.list = list()        #create an empty list
        for(i in 1:n2){         #generate n/2 number of simulations 
                U <- runif(2, min=0, max=1) #generate a pair of uniformly distributed random deviates
                Ui <- my.U(U)               #transform the distribution from (0,1) to [-1,1] 
                W <- my.W(U)                #calling back my.W function with input of generated a pair of uniformly distributed random deviates
                v <- my.v(W)                #calling back my.v function with input of generated W
                x <- (v)*(Ui)               #part4 and #part 5   #deliver X1 and X2 by multiplying v and Ui
                Y <- (x*sigma)+mu           #multiplying the values by sigma and adding mu
                my.list=c(my.list,Y)        #generate n number of simulations with delivered Y
        }
        vec <- unlist(my.list)   #'unlist' tronsforms 'list' to 'vector'
        return(vec)
}


#Box-Mueller algorithm

set.seed(seed=13)   #reproduce the same random objects to help me evaluate my tasks

#box.rnorm function returns a vector of pseudo-random values from a normal distribution and has three arguments.  
        #The first argument is the number of values to return (no default).
        #The second argument is mean (default=0)
        #The third argument is standart deviation (default=1)
box.rnorm <- function (n, mean=0, sd=1) {  
        n2 <- n/2               #to generate n number of simulations, n/2 simulations will be taken because Box-Mueller algorithm produces a pair
                                 #if n is an odd number, R will round that odd number to the below even number. 
        box.list = list()        #create an empty list
        for(i in 1:n2){         #generate n/2 number of simulations 
                U <- runif(2, min=0, max=1)     #generate a pair of uniformly distributed random deviates
                K <- 2*pi*(U[1])                #I defined K and L to follow formula more easily
                L <- sqrt((-2)*log(U[2]))       #formula transforms a pair of uniformly distributed random deviates to polar coordinates
                X1 <- (sin(K))*L                # deliver X1      #part3 
                X2 <- (cos(K))*L                           #and X2     #part3 
                a <- c(X1,X2)                   #combine the polar coordinates
                box.list=c(box.list, a)         #generate n number of simulations with delivered polar coordinates
        }
        box.vector <- unlist(box.list)          #'unlist' tronsforms 'list' to 'vector'
        return (box.vector)
}

#Central limit theorem

set.seed(13)    #reproduce the same random objects to help me evaluate my tasks

#CLT.rnorm function returns a vector of pseudo-random values from a normal distribution and has three arguments.  
        #The first argument is the number of values to return (no default).
        #The second argument is mean (default=0)
        #The third argument is standart deviation (default=1)
CLT.rnorm <- function(n, mean=0, sd=1){  
        CLT.list = list()       #create empty list
        for(i in 1:n){          #generate n number of simulations 
                U <- runif(16, min=0, max=1)     #generate sixteen uniformly distribued random deviates, #part1
                x <- (sum(U)-8)*sqrt(12/16)      #insert central limit theorem for generated sixteen uniformly distribued random deviates #part 2 and #part3
                CLT.list=c(CLT.list,x)           #combine the list with delivered x and number of simulations
        }
        CLT.vec <- unlist(CLT.list)       #'unlist' tronsforms 'list' to 'vector'
        return(CLT.vec)                 
}


#general.rnorm
set.seed(seed=13)    #reproduce the same random objects to help me evaluate my tasks

#general.rnorm function returns a vector of pseudo-random normally-distributed daviates and has two arguments.
        #the first argument is the number of values to return (no default)
        #the second argument is method type, 3 methods are defined (default=1)
                # 1. Marsaglia and Bray, 2. Box and Mueller and 3. central-limit theorem
general.rnorm <- function(X, select = 1){     #"1" is default value for second argument, means that if the second argument is not given the function will automaticly select=1
        if(select==1){             #if select=1, general.rnorm will use Marsaglia and Bray algorithm which is my.rnorm() 
        output <- my.rnorm(X)      #my.rnorm will take the number of simulations as input and deliver the pseudo-random values from a normal distribution 
        return(output)             
        } 
        if(select==2){      #if select=2, general.rnorm will use Box and Mueller algorithm which is box.rnorm()
        output <- box.rnorm(X)  #box.rnorm will take the number of simulations as input and deliver the pseudo-random values from a normal distribution 
        return(output)
        } 
        if(select==3) {    #if select=3, general.rnorm will use central-limit theorem algorithm which is CLT.rnorm()
        output <- CLT.rnorm(X)   #CLT.rnorm will take the number of simulations as input and deliver the pseudo-random values from a normal distribution 
        return(output)
        } 
        if(select!=1) {     #if the second argument is not 1
            if(select!=2){     #and if it is not 2
                if(select!=3){    #and if it is also not 3 
                        print("invalid arguments")   #then print "invalid arguments" 
                }
            }            
        }
}



#1.test function 
#'is.while.loop.working' function tests the while loop (defined in my.W function for #Marsaglia and Bray's method)
is.while.loop.working <- function(x) {  
        new_w <- my.W(x)
        if(new_w > 1) {         # if the outcomes of my.W greater than 1, my test will fail
                print("test failed")
        }
        if(new_w <=1){   # if the outcomes of my.W smaller or equal to 1, my test will pass
                print("test passed") 
        }
}

is.while.loop.working(U)  #"test passed"


#2.test function
#'test.my.rnorm function' tests if my.rnorm produces odd numbers, it should not be producing an odd number because the algorithm generates pairs
test.my.rnorm <- function(x){      
        lengthy <- length(x)       
        OddEven <- lengthy%%2   #if you divide a number in half, you can find out is it odd or even from its remainder #modulus
        if(OddEven==1){
                print("test failed")    #if it is an odd it will have a remainder of 1
        }
        if (OddEven==0){
                print("test passed")    #if it is an even it will have a remainder of 0
        }
}
#examples
test.my.rnorm(my.rnorm(11))
test.my.rnorm(my.rnorm(4))   


#3.test function
#'test2.my.rnorm' creates histograms of my.rnorm's outputs #tests for density and distribution 
test2.my.rnorm <- function(x){   
        output.my.rnorm <- my.rnorm(x)  #generate x number of simulations for my.rnorm()
        a <- hist(output.my.rnorm)   #create histogram from output.my.rnorm
        return(a)
}
#example
test2.my.rnorm(100)


#4.test function
test3.my.rnorm <- function(x){             #to test convergence
        output.my.rnorm <- my.rnorm(x)  
        output <- cumsum(output.my.rnorm)/seq_along(output.my.rnorm)  #cumulative mean
        y <- plot(output, type="l", col="red")        #convergence can be seen on plot
        return(y)
}
#example
test3.my.rnorm(100)


#5.test function
#'test.general.rnorm' creates histograms of general.rnorm's outputs   #tests for density and distribution 
test.general.rnorm <- function(x, y){   
        output.general.rnorm <- general.rnorm(x, y)  #x for the number of values to return and y for method as defined above
        a <- hist(output.general.rnorm)    #create histogram from output.general.rnorm
        return(a)
}

#examples
test.general.rnorm(100,1)
test.general.rnorm(100,2)
test.general.rnorm(100,3)