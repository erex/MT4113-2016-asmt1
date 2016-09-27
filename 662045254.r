###Will Tun
###MT 4113: Computing in Statistics, Assignment 1
### I confirm that the attached is my own work, except where clearly indicated in the text.


##Required packages for test functions:
##library(parallel)
##library(TTR)
##library(sm)


##Marsaglia & Brays Algorithm

U <- runif(2, min=0, max=1) #generate a pair of uniform deviates distributed over the range of [0, 1]

gen.Ui <- function(X){   
#Argument 1: U    #Input a vector containing a pair of uniform deviates distributed over the range of [0,1]   
#Purpose: This function transform the input such that the pair of values are distributed over the range of [-1, +1]

Ui <- (2*X) - 1        #Transform to unit square:Ui =(2×Ui) −1
w <- sum(Ui^2)       # w = U1^2 + U2^2          
    while (w > 1) {                 #Rejection step ensures w =< 1  
        Xx <- runif(2, min=-1, max=1)  
        Ui <- (2*Xx) - 1
        w <- sum(Ui^2)
    }
  return(Ui)
}

gen.v <- function(X){
#Argument 1: Vector containing output of gen.Ui 
    W <- sum(X^2) 
    v = sqrt( (-2*log(W)) /W)
    return(v)
}


MarsAndBray <- function(X){
#Argument 1: Input a vector containing a pair of uniform deviates distributed over the range of [0,1]  
#Purpose: Transform the input into a pair of normally distributed variates  
    Ui = gen.Ui(X)
    v <- gen.v(Ui)
    output <- Ui*v
    return(output)
}

MarsAndBray(U)


######Box-Mueller algorithm 
BoxMueller <- function(X){
#Argument 1: U    #Input a vector containing a pair of uniform deviates distributed over the range of [0,1]  
#Purpose: Transform the input into polar co-ordinates for which the marginal distribution of two points in 2D space 
#          approximates a normal distribution in the x & y dimension 
    X1 = sin(2*pi*X[1]) * sqrt(-2*log(X[2]))
    X2 = cos(2*pi*X[1]) * sqrt(-2*log(X[2]))
    out <- c(X1,X2)
    return(out)
}

BoxMueller(U)

#Central limit theorem
CentLim <- function(X){
#Argument 1: U    #Input a vector containing a pair of uniform deviates distributed over the range of [0,x >= 16]
#Purpose: Since the mean of a relatively large number of independent random variables tend to be approximately 
#         normally distributed, this function aims to approximate such a distribution 
#         from 16 values sampled from a uniform distribution.   
    num_deviates <- length(X)
    if (num_deviates < 16) {
        print("invalid arguments")         # Warning, central limit theorem requires 16+ of independent random variables
    } 
    out <- (sum(X) - 8)*(sqrt(12/16))
    return(out)
} 

#my.rnorm 
my.rnorm <- function(n, mu=0, sigma=1) { 
#Argument 1 (n): Number of values to return
#Argument 2 (mu): The mean of the values to return, default = 0
#Argument 3 (sigma): The standard deviation of values to return, default = 1
#Purpose: This function simulates and returns n number of normally distributed deviates 
#         which was transformed from an original pair of deviates derived from a uniform distribution
#         over the range of [0, 1]. 
#         It does this by applying the Marsaglia and Bray method [MarsAndBray()]  
##Warning, if n = odd number, r will round down number of values to return since n/2 = int value. 
    n2 <- n/2           #Define n number of values to return + accounts for TWO uniform deviates being created each iteration
    multi <- lapply(seq.int(n2), function(iter) {   #Returns a list where each element is the result of applying iterative function to corresponding element 
        U <- runif(2, min=0, max=1)   
        mars_bra <- MarsAndBray(U)                   #Apply marsaglia and bray method via predefined function 
        trans_mars_bra <- (mars_bra*sigma) + mu      #Transform the normally distributed values where resulting distribution as mean = (mu) and standard deviation = sigma
    })
    Sim_matrix <- do.call(rbind, multi)   #rbind the returned list into a matrix
    out <- c(Sim_matrix)
    #plot(density(out), main="Distribution") #make a denstiy plot #Put into check algorithm
    output <- as.vector(out)
    return(output)
}


general.rnorm <- function(n, mu, sigma, select_param ) { 

#Default select_param = 1
#MarsAndBray + BoxMueller returns two scalars within a vector 
#So can only generate an even number of outputs 
#Warning statement 1: Informs user if they call an odd number of deviates 
    if( (n %% 2) == 1 ) {
        print("Odd number of deviates requested, please request an even number if using Box&Mueller or Marsaglia&Brays")
    }
#warning statement 2: Warns if invalid select_param argument is called. 
    if (select_param != 1) {
        if (select_param != 2) {
            if (select_param !=3) { 
                print("invalid arguments")
            }
        }
    }  
    num_sim = n/2    #number of simulations - accounts for U containing 2 scalars
    if (select_param == 1) {
        multi_call <- lapply(seq.int(num_sim), function(iter) {
            U <- runif(2, min=0, max=1)
            mars_bra <- MarsAndBray(U)
            trans_mars_bra <- (mars_bra*sigma) + mu 
        })
    }
    if (select_param == 2) {
        multi_call <- lapply(seq.int(num_sim), function(iter) {
            U <- runif(2, min=0, max=1)
            box_mule <-BoxMueller(U)
            trans_box_mule <- (box_mule*sigma) + mu
        })
    }
    if(select_param == 3) {
        num_sim = n #alter num_sim to account for clt returning 1 scalar 
        multi_call <- lapply(seq.int(num_sim), function(iter) {
        U <- runif(16, min=0, max=1)
        clt <- CentLim(U)
        trans_clt <- (clt*sigma)+ mu 
        })
    }
    Sim_matrix <- do.call(rbind, multi_call) 
    out <- c(Sim_matrix)
    output <- as.vector(out)
    return(output)
}


Test_n = 200000
Test_mean = 50 
Test_sd = 5

mars_bra_test <- my.rnorm(Test_n, Test_mean, Test_sd)
box_mule_test <- general.rnorm(Test_n, Test_mean, Test_sd,2)
cent_test <- general.rnorm(Test_n, Test_mean, Test_sd,3)
#R has a built in function which generated normally distributed random deviates
control_test <- rnorm(Test_n, Test_mean, Test_sd)  #This can be used to compare against our own functions







### CHECK 1: W must always be less than or equal to 1. 
# Transform function MUST return a pair of U values which generates w <= 1
# This check function will simulate multiple U values 
# apply the transform function to all of them 
# return a list of all w values generated 
# and flag if any is above 1 

library(parallel)
check1 <- function(num_deviates, num_sims, parallel) {   #number of simulations 
  # For loop to generate a list of W values after various simulations of transforming U into W
  # check1 takes in 3 arguments:
  # num_deviates = number of deviates sampled from a uniform distribution  
  # num_sims = number of times we wish to create sample U and simulate the transformation function 
  # parallel = ["yes"|"no"]   yes = faster multithread simulation, no = slower simulation 
    if (parallel == "no") { 
        multi_thread <- lapply(seq.int(num_sims), function(iter) {
            cat("Sim", iter, "--", format(Sys.time(), "%H:%M:%S"), "\n")  #Keep track of simulation time 
            U <- runif(num_deviates, min=0, max=1)  
            Ui <- gen.Ui(U)
            W <- sum(Ui^2)  
            return(W)
        }) 
    }
    if (parallel == "yes") {   ##Multithreading with mclapply function 
        multi_thread <- mclapply(seq.int(num_sims), function(iter) {
            cat("Sim", iter, "--", format(Sys.time(), "%H:%M:%S"), "\n")  #Keep track of simulation time 
            U <- runif(num_deviates, min=0, max=1)  
            Ui <- gen.Ui(U)
            W <- sum(Ui^2)  
            return(W)
        }) 
    }
    Sim_matrix <- do.call(rbind, multi_thread) #creates a matrix of simulated W values
    max_index <- which.max(Sim_matrix) #index of smallest value
    max_out <- Sim_matrix[max_index]   #Minimum W value 
    if (max_out <= 1) {
        cat(" SUCCESS. \n Out of",num_sims,"simulations of the transform function \n the maximum value derived for W was:",max_out)
    }
    else{
        cat(" FAILURE. \n Transform function produce W with value more than 1: ", max_out) 
    }
}

#If package "parallel" is available
#A large number of simulations of check1 can be done relatively fast
#Due to multi-threading the simulation with mclapply 
check1(2, 100000, "yes") 
#If parallel unavailable, the check1 function can take in parallel = "no"
#Which simply uses lapply instead of mclappy.
check1(2, 1000, "no")



### CHECK 2: Convergence of mean / standard deviation
### In order to successfully evaluate the transform functions
### Enough simulations must take place
### Until the sampled scalars have converged upon the expected mean/standard deviation
library(TTR)
convergence <- function(n,mu,sigma, sdORmean){
#Arg 1,2,3: number of deviates to return, expected mean, expected standard deviation   
#Arg 2: sdORmean  IF 1 = return plot of convergence to expected MEAN 
#Arg 2: sdORmean  IF 2 = return plot of convergence to expected STANDARD DEVIATION 
    for (iter in 1:3) {
        out <- general.rnorm(n, mu, sigma, iter)  #generate normally distributed deviates 
        if (sdORmean == 1) {  
            out2 <- cumsum(out)/seq_along(out)        #calculate cumulative mean
            tit <- "Convergence to expected mean across sampling simulation"
            lab <- "Mean"
        }
        if (sdORmean == 2) {
            out2 <- runSD(out, n=1, cumulative=TRUE)  #calculate cumulative standard deviation
            tit <- "Convergence to expected Standard Deviation across sampling simulation"
            lab <- "Standard Deviation"
        }
        colours = c("chartreuse", "blue", "red")  
        if (iter == 1) {
            plot(out2, type = "l", col=colours[iter], main = tit , ylab = lab, xlab = "Iteration") 
        }
        else {
            lines(out2, type = "l", col=colours[iter])
        }
    }
    legend("topright", inset=.05, title="legend",
    c("MB","BM","CLT"), fill=c("chartreuse","blue","red"), horiz=FALSE) 
}

convergence(2000, 45, 10, 2) #convergence of standard deviation
convergence(2000, 45, 10, 1) #convergence of mean


### CHECK 3: Calculate summary statistics 
sum_stats <- function(X){
#Argument 1: vector containing normally distributed values derived from chosen method 
  Mean <- mean(X)
  Standard_Deviation <- sd(X)
  Median <- median(X)
  output <- as.data.frame(t(rbind(Mean,Standard_Deviation,Median))) #rbind the sum stats, transpose the result, put it into a dataframe
}

###-- Execute check3: 
###Print out summary stats of each test
##Compare with expected values 
Test_mean  #Expected mean, in this case, 50
Test_sd  #Expected standard deviation, 5 

sum_mb <- sum_stats(mars_bra_test)
sum_mb 
sum_bm <- sum_stats(box_mule_test)
sum_bm
sum_ctl <- sum_stats(cent_test)
sum_r <- sum_stats(control_test)
sum_r



#### CHECK 4: PLOT THE SAMPLED DEVIATES INTO A GRAPH 
plotter <- function(X, plot_style = 2){
  ##Test function creates plots of deviates 
  ##To check by eye if deviates seem normally distributed
  #Arg 1: X : vector of deviates  
  #Arg 2: plot_style : 1 creates kernal density plot, 2 creates frequency histogram 
    if (plot_style == 1) {
        m <- as.matrix(X)   #convert to matrix with one column 
        den <- density(m) 
        plot(den, main="Frequency Distribution") 
        polygon(den, col="peachpuff1", border="royalblue4")
    }
    if (plot_style == 2) {
    hist(X, main = "Histogram of generated deviates", col="tomato1")
    lines(density(X))
    }
}

plotter(mars_bra_test, 1)
plotter(cent_test, 1)
plotter(mars_bra_test, 2)
plotter(box_mule_test, 2)
plotter(control_test)

### CHECK 5 : KERNAL DENSITY PLOTS OF ALL DISTRIBUTIONS 
#The sm.density.compare( ) function 
#sm package allow several kernal density plots
# to be superimposed 
#The format is sm.density.compare(x, factor) 
#where x is a numeric vector and factor is the grouping variable.

library(sm)

kernal_den <- function(a, b, c, d) {
#Argument 1: vector containing normally distributed values derived from applying Marsaglia and Bray method
#Argument 2: vector containing normally distributed values derived from applying Box Mueller algorithm
#Argument 1: vector containing normally distributed values derived from applying Central limit theorem 
#Argument 2: vector containing normally distributed values derived from applying inbuilt function rnorm. 
    aa <- merge(a,"Mars_Bra")  #"Mars_Bra"
    bb <- merge(b,"Box_Mule") #"Box_Mule"
    cc <- merge(c, "CLT")         #"CLT"
    dd <- merge(d, "R generated") #"R generated"
    data <- rbind(aa,bb,cc,dd)
    data$y
    str(data)
    kernally <- factor(data$y, levels= c("Mars_Bra", "Box_Mule", "CLT", "R generated"),
                labels = c("Mars_Bra", "Box_Mule", "CLT", "R generated")) 
    sm.density.compare(data$x, kernally, xlab="Deviates")
    title(main="Kernal Density Plots of All distributions")
    legend("topright", inset=.03, title="legend",
    c("MB","BM","CLT","R"), fill=c("red","forestgreen","blue","cyan"), horiz=FALSE)
}

kernal_den(mars_bra_test,box_mule_test,cent_test, control_test)

#Can check roughly if the mean transform does what it should
##Alter the input for mu in a different way for all algorithms
mars_bra_test2 <- my.rnorm(Test_n, 25, Test_sd)
box_mule_test2 <- general.rnorm(Test_n, 75, Test_sd,2)
cent_test2 <- general.rnorm(Test_n, 100, Test_sd,3)
kernal_den(mars_bra_test2,box_mule_test2,cent_test2, control_test)




###CHECK 6: BOX PLOTS
box_plots <- function(X, Y){
#Argument 1: vector containing normally distributed values derived from chosen method 1 
#Argument 2: vector containing normally distributed values derived from chosen method 2 

##Creates two boxplots to compare range and median of two datasets
    y <- deparse(substitute(Y))  #convert object name into a new variable containing string of original object name
    x <- deparse(substitute(X))
    name <- c(x,y)
    colours = c("seagreen2","palevioletred1")
    boxplot(X, Y, col = colours,names = name)
}

box_plots(mars_bra_test, control_test)


###CHECK 7: T-test
#Compare the means of two groups 
#under the assumption that both samples are random, independent, and come from normally distributed population 
#with unknown but equal variances

t_test <- function(X, Y){
#Argument 1: vector containing normally distributed values derived from chosen method 1 
#Argument 2: vector containing normally distributed values derived from chosen method 2 
    T <- t.test(X,Y)
    print(T)
    P.value <- T[3]
    if (P.value > 0.1){
        print("Conclusion: P value was more than 0.1, strength of evidence against T-test null hypothesis is weak")
    }
    if ((P.value <= 0.1) & (P.value >0.05)) {
        print("Conclusion: P value was between 0.05 and 0.1, strength of evidence against T-test null hypothesis is moderate")
    }
    if ((P.value <= 0.05) & (P.value > 0)) {
        print("Conclusion: P value was equal to or less than 0.05, strength of evidence against T-test null hypothesis is strong")
    }
}

t_test(mars_bra_test,control_test)




###CHECK 8: Shapiro test 
#When one takes independent randomly sampled data points taken from a normal distribution
#And plot such points on a Q-Q plot
#If the points show linearity 
#This suggests that the data is normally distributed 

mars_bra_test <- my.rnorm(5000, Test_mean, Test_sd)   
shapiro_test <- function(X) {
##  https://en.wikipedia.org/wiki/Shapiro–Wilk_test
    shapiro.test(X)
    qqnorm(X);qqline(X, col = 2)  
}
shapiro_test(mars_bra_test)


####RAN OUT OF TIME!!!

man_test <- function(X, Y){
#Argument 1: vector containing normally distributed values derived from chosen method 1 
#Argument 2: vector containing normally distributed values derived from chosen method 2 
    T <- wilcox.test(X,Y)
    print(T)
    P.value <- T[3]
    if (P.value > 0.1){
        print("Conclusion: P value was more than 0.1, strength of evidence against T-test null hypothesis is weak")
    }
    if ((P.value <= 0.1) & (P.value >0.05)) {
        print("Conclusion: P value was between 0.05 and 0.1, strength of evidence against T-test null hypothesis is moderate")
    }
    if ((P.value <= 0.05) & (P.value > 0)) {
        print("Conclusion: P value was equal to or less than 0.05, strength of evidence against T-test null hypothesis is strong")
    }
}


var_test <- function(X, Y){
#Argument 1: vector containing normally distributed values derived from chosen method 1 
#Argument 2: vector containing normally distributed values derived from chosen method 2 
    T <- var.test(X,Y)
    print(T)
    P.value <- T[3]
    if (P.value > 0.1){
        print("Conclusion: P value was more than 0.1, strength of evidence against T-test null hypothesis is weak")
    }
    if ((P.value <= 0.1) & (P.value >0.05)) {
        print("Conclusion: P value was between 0.05 and 0.1, strength of evidence against T-test null hypothesis is moderate")
    }
    if ((P.value <= 0.05) & (P.value > 0)) {
        print("Conclusion: P value was equal to or less than 0.05, strength of evidence against T-test null hypothesis is strong")
    }
}

var_test <- function(X, Y){
#Argument 1: vector containing normally distributed values derived from chosen method 1 
#Argument 2: vector containing normally distributed values derived from chosen method 2 
    T <- ks.test(X,Y)
    print(T)
    P.value <- T[3]
    if (P.value > 0.1){
        print("Conclusion: P value was more than 0.1, strength of evidence against T-test null hypothesis is weak")
    }
    if ((P.value <= 0.1) & (P.value >0.05)) {
        print("Conclusion: P value was between 0.05 and 0.1, strength of evidence against T-test null hypothesis is moderate")
    }
    if ((P.value <= 0.05) & (P.value > 0)) {
        print("Conclusion: P value was equal to or less than 0.05, strength of evidence against T-test null hypothesis is strong")
    }
}









##References 
##http://www.had2know.com/academics/test-for-normality-statistics.html
##test normal distribution deviates with P-test
##http://www.annualreviews.org/doi/pdf/10.1146/annurev.publhealth.23.100901.140546
##


###Foot notes
#result_list=list() #create empty array 
#  for (iter in 1:num_sims) { 
#    U <- runif(2, min=-1, max=1)  #create local variable U
#    Ut <- transform(U)
#    W <- sum(Ut^2)                #Generate W per simulation
#    result_list=c(result_list, W) #concatenate W onto end of list 
#  }
#  min <- max(sapply(result_list, min))  #find minimum value in list generated by above loop




