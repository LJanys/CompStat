rm(list=ls())
## exercise 1
#(a)
set.seed(40)
beta <- c(-2,0.1,1)
Logit_data <- function(N,beta){
  x0 <- rep(1,N)
  x1 <- runif(N,18,60)
  x2 <- rbinom(N,1,0.5)
  X <- cbind(x0,x1,x2)
  pai <- exp(X%*%beta)/(1+exp(X%*%beta))
  Y <- rbinom(N,1,pai)
  data <- cbind(X,pai,Y)
}
sim <- Logit_data(1000,beta)

## (b) likelihood function
Likelihood <- function(beta){
  X <- cbind(sim[,1],sim[,2],sim[,3])
  pai <- exp(X%*%beta)/(1+exp(X%*%beta))
  L <- prod(((pai)^sim[,5])*
              ((1-pai)^(1-sim[,5])))
}
L1 <- Likelihood(beta)
# both functions require the data structure as 
# same as above simulation function
Log_likelihood <- function(beta){
  y <- sim[,5]
  X <- cbind(sim[,1],sim[,2],sim[,3])
  L <-  sum(-y*log(1 + exp(-(X%*%beta))) -
              (1-y)*log(1 + exp(X%*%beta)))
}
L2 <- Log_likelihood(beta)

## (c) 
beta1 <- seq(-0.5,0.5,by=0.025)
beta2 <- seq(-1,3,by=0.05)
## for coefficient beta1
beta <- c(-2,0.1,1)
L1 <- c()
for (i in 1:length(beta1)) {
  beta[2] <- beta1[i]
  L1[i] <- Likelihood(beta)
}
x <- beta1[match(max(L1),L1)]
y <- max(L1)
plot(beta1,L1,"l")
points(x,y,pch=18,col='red')
x

beta <- c(-2,0.1,1)
L11 <- c()
for (i in 1:length(beta1)) {
  beta[2] <- beta1[i]
  L11[i] <- Log_likelihood(beta)
}
x <- beta1[match(max(L11),L11)]
y <- max(L11)
plot(beta1,L11)
points(x,y,pch=18,col='red')
x

# beta2
beta <- c(-2,0.1,1)
L2 <- c()
for (i in 1:length(beta2)) {
  beta[3] <- beta2[i]
  L2[i] <- Likelihood(beta)
}
xb2 <- beta2[match(max(L2),L2)]
yb2 <- max(L2)
plot(beta2,L2)
points(x,y,pch=18,col='red')
xb2

beta <- c(-2,0.1,1)
L2 <- c()
for (i in 1:length(beta2)) {
  beta[3] <- beta2[i]
  L2[i] <- Log_likelihood(beta)
}
x <- beta2[match(max(L2),L2)]
y <- max(L2)
plot(beta2,L2)
points(x,y,pch=18,col='red')
x


# (d)
# different initial setting of maxBFGS leads to different 
# estimated results. 
library(miscTools)
library(maxLik)

estim<-maxBFGS(Log_likelihood,finalHessian=TRUE,start=c(1,0,0))
estim.par<-estim$estimate
estim.par
estim.hess<-estim$hessian
Cov<--(solve(estim.hess))
sde<-sqrt(diag(Cov)) 
sde
result <- matrix(NA,2,3)
result[1,] <- t(estim.par)
result[2,] <- t(sde)
result


## (e) calculate the marginal effects
# we write the marginal effect of X1, given x2 = 0
x2 <- 0
dx<-c()
x0 <- sort(sim[,2])
for(i in 1:length(sim[,1])){
  pi<-exp(estim.par[1] + estim.par[2] * x0[i]+
            estim.par[3]*x2) / 
    (1 + exp(estim.par[1] + estim.par[2] *x0[i]+
               estim.par[3]*x2))
  dx[i]<-estim.par[2]*(pi)*(1-pi) 
}
x2<-1
dx1<-c()
x1 <- sort(sim[,2]) # without order, problem arises for lines
for(i in 1:length(sim[,1])){
  pi<-exp(estim.par[1] + estim.par[2] * 
            x1[i]+estim.par[3]*x2) / 
    (1 + exp(estim.par[1] + estim.par[2] *
               x1[i]+estim.par[3]*x2))
  dx1[i]<-estim.par[2]*(pi)*(1-pi) 
}
plot(x0,dx,xlab = "x1",ylab = "dx",
     main = "Marginal Effect of x1 given x2","l")
lines(x1,dx1,col="red")
legend(50,0.0025,legend=c("x2=0","x2=1"), col=c("black", "red"),
       lty = 1:2)

## the prediction effect given x2 = 0,1
x2 <- 0
px<-c()
x0 <- sort(sim[,2])
for(i in 1:length(sim[,1])){
  px[i]<-exp(estim.par[1] + estim.par[2] * x0[i]+
            estim.par[3]*x2) / 
    (1 + exp(estim.par[1] + estim.par[2] *x0[i]+
               estim.par[3]*x2))
}
x2<-1
px1<-c()
x1 <- sort(sim[,2]) # without order, problem arises for lines
for(i in 1:length(sim[,1])){
  px1[i]<-exp(estim.par[1] + estim.par[2] * 
            x1[i]+estim.par[3]*x2) / 
    (1 + exp(estim.par[1] + estim.par[2] *
               x1[i]+estim.par[3]*x2))
}
plot(x0,px,xlab = "x1",ylab = "Prob.",
     main = "Probability of x1 given x2","l",
     ylim = c(0.4,1))
lines(x1,px1,col="red")
legend(50,0.85,legend=c("x2=0","x2=1"), col=c("black", "red"),
       lty = 1:2)

#plot(x1,px1,xlab = "x1",ylab = "Prob.",
    # main = "Probability of x1 given x2","l",
     #ylim = c(0.9,1), col="red")

## (f) plot with CI given x2=0 for the marginal effect
## according to MLE, betas follow standard normal distribution
x2 <- 0
dxlower<-c()
dxupper <- c()
z <- abs(qnorm(0.025))
beta1_lower <- estim.par[2]-z*sde[2]
beta1_upper <- estim.par[2]+z*sde[2]
for(i in 1:length(sim[,1])){
  pi<-exp(estim.par[1] + estim.par[2]* 
            x0[i]+estim.par[3]*x2) / 
    (1 + exp(estim.par[1] +  estim.par[2]*
               x0[i]+estim.par[3]*x2))
  dxlower[i]<-beta1_lower *(pi)*(1-pi) 
}
for(i in 1:length(sim[,1])){
  pi<-exp(estim.par[1] +  estim.par[2] * 
            x0[i]+estim.par[3]*x2) / 
    (1 + exp(estim.par[1] +  estim.par[2] *
               x0[i]+estim.par[3]*x2))
  dxupper[i]<-beta1_upper*(pi)*(1-pi) 
}
lines(x0,dxlower,col="blue",lty=3)
lines(x0,dxupper,col="blue",lty=3)
## assume that given pi is not influenced by different beta
# if not, our CI looks weird. 


## now plot the CI for probability (Wald CI)
N <- 1000
X0 <- cbind(sim[,1],sort(sim[,2]),rep(0,N))
X1 <- cbind(sim[,1],sort(sim[,2]),rep(1,N))
var_x0 <- X0%*%Cov%*%t(X0)
var_x1 <- X1%*%Cov%*%t(X1)
z <- abs(qnorm(0.025))

pi_lower <- c()
x2 <- 0
for (i in 1:N) {
  pi_lower[i]<-exp(estim.par[1] +  estim.par[2] * 
            X0[i,2]+estim.par[3]*x2 - z*sqrt(var_x0[i,i])) / 
    (1 + exp(estim.par[1] +  estim.par[2] *
               X0[i,2]+estim.par[3]*x2- z*sqrt(var_x0[i,i])))
}
pi_upper <- c()
for (i in 1:N) {
  pi_upper[i]<-exp(estim.par[1] +  estim.par[2] * 
                     X0[i,2]+estim.par[3]*x2 + z*sqrt(var_x0[i,i])) / 
    (1 + exp(estim.par[1] +  estim.par[2] *
               X0[i,2]+estim.par[3]*x2+ z*sqrt(var_x0[i,i])))
}
lines(X0[,2],pi_lower,col="blue",lty=3)
lines(X0[,2],pi_upper,col="blue",lty=3)
################## for x2 = 1
pi_lower1 <- c()
x2 <- 1
for (i in 1:N) {
  pi_lower1[i]<-exp(estim.par[1] +  estim.par[2] * 
                     X1[i,2]+estim.par[3]*x2 - z*sqrt(var_x1[i,i])) / 
    (1 + exp(estim.par[1] +  estim.par[2] *
               X1[i,2]+estim.par[3]*x2- z*sqrt(var_x1[i,i])))
}
pi_upper1 <- c()
for (i in 1:N) {
  pi_upper1[i]<-exp(estim.par[1] +  estim.par[2] * 
                     X1[i,2]+estim.par[3]*x2 + z*sqrt(var_x1[i,i])) / 
    (1 + exp(estim.par[1] +  estim.par[2] *
               X1[i,2]+estim.par[3]*x2+ z*sqrt(var_x1[i,i])))
}
lines(X1[,2],pi_lower1,col="orange",lty=3)
lines(X1[,2],pi_upper1,col="orange",lty=3)

##############################################
##############################################
##############################################
##############################################
############### exercise 2 ###################
##############################################
##############################################
##############################################
##############################################

################### (a)
set.seed(40)
beta <- c(-2,0.1,1)
N <- 1000
Logit_data_version2 <- function(N,beta){
  x0 <- rep(1,N)
  x1 <- runif(N,18,60)
  x2 <- rbinom(N,1,0.5)
  X <- cbind(x0,x1,x2)
  pai <- exp(X%*%beta)/(1+exp(X%*%beta))
  Y <- rbinom(N,1,pai)
  data <- cbind(X,Y)
}
# this function need our data be named as sim
Log_likelihood <- function(beta){
  y <- sim[,4]
  X <- cbind(sim[,1],sim[,2],sim[,3])
  L <-  sum(-y*log(1 + exp(-(X%*%beta))) -
              (1-y)*log(1 + exp(X%*%beta)))
}

library(miscTools)
library(maxLik)
T <- 100
training_PE <- c()
testing_PE <- c()
## loop
for (i in 1:T) {
  sim <- Logit_data_version2(N,beta)
  sim_test <- Logit_data_version2(N,beta)
  #generate training and test datasets
  estim<-maxBFGS(Log_likelihood,start=c(0,0,0))
  par<-estim$estimate
  X <- cbind(sim[,1],sim[,2],sim[,3])
  X_test <- cbind(sim_test[,1],sim_test[,2],sim_test[,3])
  ## prediction y for training and test
   pai <- exp(X%*%par)/(1+exp(X%*%par))
  pai_test <- exp(X_test%*%par)/(1+exp(X_test%*%par))
  y_training <- c()
  y_test <- c()
  for (a in 1:N) {
    if(pai[a]>=0.5){
      y_training[a] <- 1
    }
    else{y_training[a] <- 0}
  }
  for (b in 1:N) {
    if(pai_test[a]>=0.5){
      y_test[b] <- 1
    }
    else{y_test[b] <- 0}
  }
  # the mean of T/F vector is avg.P.E.
  training_PE[i] <- mean(y_training==sim[,4])
  testing_PE[i] <- mean(y_test==sim_test[,4])
}
result_null <- c(1-mean(training_PE),1-mean(testing_PE))
result_null

###########################################
############## (b)
# smaller N could satisfy, interesting is that if N=100
# our demands won't be met. 
set.seed(40)
beta <- c(-2,0.1,1)
N <- 500
Logit_data_version3 <- function(N,beta){
  x0 <- rep(1,N)
  x1 <- runif(N,18,60)
  x2 <- rbinom(N,1,0.5)
  X <- cbind(x0,x1,x2)
  pai <- exp(X%*%beta)/(1+exp(X%*%beta))
  Y <- rbinom(N,1,pai)
  data <- cbind(X,Y)
}
T <- 100
training_PE <- c()
testing_PE <- c()
## loop
for (i in 1:T) {
  sim <- Logit_data_version3(N,beta)
  sim_test <- Logit_data_version3(N,beta)
  #generate training and test datasets
  estim<-maxBFGS(Log_likelihood,start=c(0,0,0))
  par<-estim$estimate
  X <- cbind(sim[,1],sim[,2],sim[,3])
  X_test <- cbind(sim_test[,1],sim_test[,2],sim_test[,3])
  ## prediction y for training and test
  pai <- exp(X%*%par)/(1+exp(X%*%par))
  pai_test <- exp(X_test%*%par)/(1+exp(X_test%*%par))
  y_training <- c()
  y_test <- c()
  for (a in 1:N) {
    if(pai[a]>=0.5){
      y_training[a] <- 1
    }
    else{y_training[a] <- 0}
  }
  for (b in 1:N) {
    if(pai_test[a]>=0.5){
      y_test[b] <- 1
    }
    else{y_test[b] <- 0}
  }
  # the mean of T/F vector is avg.P.E.
  training_PE[i] <- 1-mean(y_training==sim[,4])
  testing_PE[i] <- 1-mean(y_test==sim_test[,4])
}
result_X <- c(mean(training_PE),mean(testing_PE))
result_X
result_null

## note, higher power of X also do this



## (c)
# less informative means likelihood function's abs.
# is smaller, we change the prob. to make each results
# have smaller Prob. to appear
set.seed(40)
beta <- c(-2,0.1,1)
Logit_data_version4 <- function(N,beta){
  x0 <- rep(1,N)
  x1 <- runif(N,-10,10)
  x2 <- rbinom(N,1,0.5)
  X <- cbind(x0,x1,x2)
  pai <- exp(X%*%beta)/(1+exp(X%*%beta))
  Y <- rbinom(N,1,pai)
  data <- cbind(X,pai,Y)
}
sim <- Logit_data_version4(1000,beta)
beta1c <- seq(-0.5,0.5,by=0.025)
beta2c <- seq(-1,3,by=0.05)
## for coefficient beta1
beta <- c(-2,0.1,1)
L12c <- c()
for (i in 1:length(beta1)) {
  beta[2] <- beta1[i]
  L12c[i] <- Likelihood(beta)
}
xc1 <- beta1[match(max(L12c),L12c)]
yc1 <- max(L12c)
plot(beta1,L1)
points(x,y,pch=18,col='red')
points(beta1c,L12c,col="blue","l")
points(xc1,yc1,pch=18,col='red')
xc1
# beta2
beta <- c(-2,0.1,1)
L22c <- c()
for (i in 1:length(beta2)) {
  beta[3] <- beta2[i]
  L22c[i] <- Likelihood(beta)
}
xc2 <- beta2[match(max(L22c),L22c)]
yc2 <- max(L22c)
plot(beta2,L2)
points(xb2,yb2,pch=18,col='red')
points(beta2c,L22c,col="blue","l")
points(xc2,yc2,pch=18,col='red')
xc2
## less N also makes that








