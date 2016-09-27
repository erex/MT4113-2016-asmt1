#Marsaglia and Bray's method

repeat { 
  U<-runif(2)
  print(U)
  U<-(2*U)-1
  print(U)
  w<-((U[1])^2)+((U[2])^2)
  print(w)
  if (w<=1) break
} 
v<-sqrt((-2*log(w))/w)
print(v)
X<-U*v
print(X)

#Box-Mueller algorithm

U<-runif(2)
print(U)
X1<-(sin(2*pi*U[1]))*(sqrt(-2*log(U[2])))
print(X1)
X2<-(cos(2*pi*U[1]))*(sqrt(-2*log(U[2])))
print(X2)
X<-c(X1,X2)
print(X)

#Central Limit Theorem

U<-runif(16)
print(U)
print(sum(U))
X<-(sum(U)-8)*sqrt(12/16)
print(X)

