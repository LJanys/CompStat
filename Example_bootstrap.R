##Example: bootstrap###
####generate a sample of realizations of Z with an unknown distribution###
#Z = c(30,37,36,43,42,43,43,46,41,42)
set.seed(1)
n=20
data.gen<-function(n,B,mu,sd)
{
  Z<-rnorm(n,mu,sd)
  sort(Z)
  n = length(Z)
  ##Calculate the sample mean
  Zbar = mean(Z)
  #####Calculate the sd#####
  sd_hat<-(n)^(-1)*sum((Z-Zbar)^2)
  ###Number of bootstrap draws.
  B = 2000
  ###Generate one bootstrap sample
  tmpdata = sample(Z,n, replace=TRUE)
  sort(tmpdata)
  ##Sample mean of the bootstrap data
  mean(tmpdata)
  ###Generate B bootstrap samples##
  tmpdata = sample(Z,n*B, replace=TRUE)
  ###Transform into matrix###
  bootstrapsample = matrix(tmpdata, nrow=n, ncol=B)
  ##################################################
  bootstrap_means<-colMeans(bootstrapsample)
 b_mean= mean(bootstrap_means)
  b_diff<-b_mean-Zbar
  n_diff<-b_mean-mu
  return(list(bootstrap_means=bootstrap_means,Zbar=Zbar,b_diff=b_diff,n_diff=n_diff))
}
results<-data.gen(n,B,0,1)
b_diff<-results$b_diff
n_diff<-results$n_diff

##Calculate consistent confidence intervals
tmp<-sort(colMeans(bootstrapsample-Zbar))
conf_int<-c(Zbar-tmp[18],Zbar-tmp[2])
conf_int
####Why not this?####
tmp2<-sort(colMeans(bootstrapsample))
conf_int2<-c(tmp2[2],tmp2[18])
conf_int2
Zbar

####Bootstrap for linear regression##############
## Two explanatory variables plus an intercept:
setwd("~/sciebo/Arbeit/Lehre/Computational_Statistics/Documents/Codes")
library(boot)
library(ISLR)
N         <- 1000 # Number of observations
X.1       <- rep(1,N)
X.2       <- rnorm(N, mean=10, sd=sqrt(1.5)) # (pseudo) random numbers form a normal distr
X.3       <- X.2^2#rnorm(N, mean=10, sd=sqrt(1.5)) # (pseudo) random numbers form a normal distr
X         <- cbind(X.1, X.2,X.3)
###Homoscedastic error term
eps       <-rnorm(N, 0,sqrt(10))# 
###Heteroscedastic error term
#eps       <-rnorm(N, 0,X.2*sqrt(10))# 
beta.vec  <- c(5,-5,0.1)
## Model
y         <- X %*% beta.vec + eps
X         <- cbind(X.1, X.2)
##Solving for beta hat###
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta.hat
#####Calculate the predicted values from the model######
y.hat<-X %*% beta.hat
##give out the dimensions of the X vector#
xx<-dim(X)
length.x<-xx[2]
###calculate the predicted values from the model
y.hat<- X %*% beta.hat
#calculate the unexplained variance
eps.hat<-y-X %*% beta.hat
#calculate the estimated standard errors
se<-(t(eps.hat)%*%(eps.hat))/(N-length.x)
cov<-se[1]*solve(t(X) %*% X)
d1<-sqrt(diag(cov))
####analytical standard errors: compare with the bootstrap####
####Bootstrapping the standard errors instead####
data<-cbind(y,X)
data<-data.frame(data)
boot.fn<-function(data,index)
{
  X<-data[,2:3]
  y<-data[,1]
  # beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
  #return(beta.hat)
  return(coef(lm(y~X[,2] ,data=data,subset=index)))
}

boot.fn(data,index=1:N)
boot(data,boot.fn,1000)



# boot(data ,boot.fn ,1000)
# 
# data<-cbind(y,X)
# data<-data.frame(data)
# 
# boot.fn=function (data ,index )
# {
#   return(coef(lm(mpg~horsepower ,data=data ,subset =index)))
# }
# boot.fn(Auto ,1:392)
# boot(Auto ,boot.fn ,1000)
# 
# 
# 
# Auto<-read.table("Auto.data.txt")











