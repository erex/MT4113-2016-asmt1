
##I confirm that the attached is my own work, except where clearly indicated in the text
#copy incomplete

#Marsaglia and Bray's method(Maindonald 1984)

set.seed(-382)
ui<-runif(n=2,min=0, max=1)
ui
#^creates a pair of uniform deviates that are on the interval [0,1]

my.u<-function(x){ 
    x<-2*x-1
    return(x)
} 
    my.u(ui)
    
##^ transforms "ui" over the unit square so that they are now distributed over range [-1,+1]

outcomeofmymy.u<-my.u(ui)
y<-outcomeofmymy.u[1]^2+outcomeofmymy.u[2]^2
y

#used as a rejection critera so that only vaules inside the unit circle are produced in new distribution
w<-function(x){ while (y<1) 
  {set.seed(-382)
  x<-runif(n=2,min=0, max=1)
  my.u
  y}
  return(x)
}

w(ui)
#code incorporates rejection equation that loops the inital generation steps if variates do not meet the criterion defined by "y"

v <-sqrt(-2*(log(w)/(w))


xi<-fuction(x1,x2)
{(x1*x2)}
xi(ui,v)



####box-mueller algorithim
set.seed(897)
u.bm<-runif(n=2,min=0, max=1)
u.bm
x.bm1<-sin(2*pi*u.bm[1])*sqrt((-2*log(u.bm[2])))
x.bm2<-cos(2*pi*u.bm[1])*sqrt((-2*log(u.bm[2])))
x.bm1
x.bm2
a<-c(x.bm1,x.bm2)
a

##central limit theorem
set.seed(323)
u.clt<-runif(n=16, min=0, max=1)
    b1<-function(x){
      x1<-((sum(x[1:16])-8)*(sqrt(12/16)))
         return(x1)
     }
  b1(u.clt)
     b<-b1(u.clt)
      b

general.rnorm<-c(a,b)
general.rnorm
  


          