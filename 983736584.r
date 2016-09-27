#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n, mean = 0,sd = 1)                                        #function asked to create
{	
	print ("Uniform Deviates in the range 0,1")
        ud1 <- runif(1, min = 0, max = 1)                                       #uniform random deviates are generated using built-in runif()
        print(ud1)
        ud2 <- runif(1, min = 0, max =1)
        print(ud2)

    print ("Uniform Deviates in the range -1,+1")                               #uniform random deviates are transformed to the required range
        ud1 = 2 * ud1 - 1
        print(ud1)
        ud2 = 2 * ud2 - 1
        print(ud2)
	
            w <- ((ud1^2) + (ud2^2))                                            #the rejection step condition is calculated here and stored in w
                while (w <= 1)                                                  #while loop to check the condition
    {
                        v <- (sqrt((-2*log(w))/w))                              #formula to calculate v to generate required normally distributed variates
                        break                                                   #while loop breaks and moves out after condition is satisfied
                    }
                
            x1 <- (ud1 * v)                                                     #pseudo-random variates are generated
            x2 <- (ud2 * v)
    
        print ("Random Deviates")
            print(x1)
            print(x2)
    
}


general.rnorm <- function(n,mean = 0,sd = 1,method = 1)                         #function asked to create
{
    if(method == 1)                                                             #method 1 for Marsaglia and Bray's algorithm, this method is default also
    {
        print ("Marsaglia and Bray's Method")
        
            print ("Uniform Deviates in the range 0,1")
                ud1 <- runif(1, min = 0, max = 1)                               #uniform random deviates are generated using built-in runif()
                print(ud1)
                ud2 <- runif(1, min = 0, max = 1)
                print(ud2)
        
            print ("Uniform Deviates in the range -1,+1")
                ud1 = 2 * ud1 - 1                                               #uniform random deviates are transformed to the required range
                print(ud1)
                ud2 = 2 * ud2 - 1
                print(ud2)
        
                    w <- ((ud1^2) + (ud2^2))                                    #the rejection step condition is calculated here and stored in w
                        while (w <= 1)                                          #while loop to check the condition
                            {
                                v <- sqrt((-2*log(w))/w)                        #formula to calculate v to generate required normally distributed variates
                                break                                          #while loop breaks and moves out after condition is satisfied
                            }
                x1 <- (ud1 * v)
                x2 <- (ud2 * v)
        
            print ("Random Deviates")                                           #pseudo-random variates are generated
                print(x1)
                print(x2)
      
    }
    
    else if(method == 2)
    {
        print ("Box-Mueller Algorithm")                                         #method 2 for Box-Mueller algorithm
        
            print ("Uniform Deviates in the range 0,1")
                ud1 <- runif(1, min = 0, max = 1)                               #uniform random deviates are generated using built-in runif()
                print(ud1)
                ud2 <- runif(1, min = 0, max = 1)
                print(ud2)
            
            x1 <- ((sin(2*3.14*ud1))*(sqrt(-2*log(ud1))))                       #pseudo-random variates are generated
                    x2 <- ((sin(2*3.14*ud2))*(sqrt(-2*log(ud2))))
            
            print ("Random Deviates")
                print(x1)
                print(x2)
    }
    
    else if(method == 3)
    {
        print ("Central Limit Theorem")                                         #method 3 for Central Limit Theorem,
        
        print ("Uniform Deviates in the range 0,1")
            ud <- runif(n, min = 0, max = 1)                                    #uniform random deviates are generated using built-in runif()
            print(ud)
                #for(i in 1:16)                                                 #for loop if for when sample size is 16, but faced errors in looping
                #{
                    x <- ((ud-mean(ud))*((mean(ud)+sd(ud))/n))                  #pseudo-random variates are generated
                #}
        print ("Random Deviates")
            print(x)
        
    }
    
    else
    {
    
    print("invalid arguments")                                                  #r console will throw the message invalid arguments if any other method number other than 1,2,3 is given
    
    }
}







#References
#1. Since I am new to r programming I did learn a lot of concepts of R from: https://www.coursera.org/learn/r-programming
#2. I also used a lot of R help page using the "?name_of_the_command" option to refer to different syntax
#3. I used http://www.statisticshowto.com/central-limit-theorem-examples/ to understand the formula for Central Limit Theorem

