###Simulating OLS samples and plot the regression lines#### 
#set.seed(32323)
## Two explanatory variables plus an intercept:
N         <- 200 # Number of observations
X.1       <- rep(1, N)
X.2      <- rnorm(N, mean=0, sd=1) # (pseudo) random numbers form a normal distr
X         <- cbind(X.1, X.2,X.2^2,X.2^3)
###Homoscedastic error term
eps       <-rnorm(N,0,10)# 
beta.vec  <- c(1,1.5,-1.5,1.5)
y         <- X %*% beta.vec + eps
##Solving for beta hat###
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta.hat
x.grid<-sort(X.2) #
true_model<-beta.vec[1]+beta.vec[2]*x.grid+beta.vec[3]*x.grid^2+beta.vec[4]*x.grid^3
estim_model<-beta.hat[1]+beta.hat[2]*x.grid+beta.hat[3]*x.grid^2+beta.hat[4]*x.grid^3
plot(estim_model,type="l")
lines(true_model,type="l",lty=1,col="red")
####Calculate the covariance matrix###
#####calculate the fitted values#####
y.hat<- X %*% beta.hat 
eps.hat<-y-X %*% beta.hat
###calculate the covariance matrix#
se<-(t(eps.hat)%*%(eps.hat))/(N-5)
cov<-se[1]*solve(t(X) %*% X)
new_data<-X[order(X[,2]),]
#new_data<-x.grid
var<-c()
for(i in 1:N)
{
  var[i]=t(new_data[i,])%*%cov%*%(new_data[i,])
}
d<-2*(sqrt(var))
estim<-beta.hat[1]+beta.hat[2]*new_data[,2]+beta.hat[3]*new_data[,2]^2+beta.hat[4]*new_data[,2]^3
conf_low<-estim-d
conf_high<-estim+d
plot(estim,type="l")
lines(conf_low,lty=2,col="red")
lines(conf_high,lty=2,col="blue")
######Bootstrapped confidence intervals#####################
B=200
b_sample<-matrix(NA,N,B)
beta.hat<-matrix(NA,length(X),B)
data<-cbind(y,X)
function_B<-matrix(NA,length(x.grid),B)

for(i in 1:B)
{
  XB<- data[sample(nrow(data),replace=T, N), ]
  beta.hat<-solve(t(XB[,2:5]) %*% XB[,2:5]) %*% t(XB[,2:5]) %*%XB[,1]
  function_B[,i]<-beta.hat[1]+beta.hat[2]*x.grid+beta.hat[3]*x.grid^2+beta.hat[4]*x.grid^3
}
plot(function_B[,1])
plot(function_B[,2])
sorted<-t(apply(function_B,1,sort)) 
####Calculate the empirical quantiles####################
conf_highB<-sorted[,195]
conf_lowB<-sorted[,5]
###########################################
plot(estim,type="l",lty=3)
lines(conf_highB)
lines(conf_lowB)

####Calculate the coverage probabilities#################################
cov_prob_B<-true_model>conf_lowB&true_model<conf_highB
sum(cov_prob_B)/length(x.grid)
cov_prob_A<-true_model>conf_low&true_model<conf_high
sum(cov_prob_A)/length(x.grid)
###Calculate interval length##############################################
int_lengthB<-conf_highB-conf_lowB
int_length<-conf_high-conf_low
sum(int_lengthB/int_length>1)####counts the number of times the bootstrapped interval length is larger than the analytical one#########
##########################################################################
data.conf<-function(model,N)
{
  #########Simulating OLS samples and plot the regression lines#### 
  #set.seed(32323)
  ## Two explanatory variables plus an intercept:
  #N         <- 200 # Number of observations
  X.1       <- rep(1, N)
  X.2      <- rnorm(N, mean=0, sd=1) # (pseudo) random numbers form a normal distr
  X         <- cbind(X.1, X.2,X.2^2,X.2^3)
  ###Error term###########################################
  if(model==1)
  {
    eps       <-rnorm(N,0,10)# 
  }
  if(model==2)
  {
    eps       <-rgamma(N,0.1,0.01)
    eps<-scale(eps, scale = FALSE)
  }
  if(model==3)
  {
    eps       <-(X.2^2)*rnorm(N,0,10)# 
  }
  ############Different distribution####################################
  if(model==4)
  {
    #Sample N random uniforms U
    U =runif(N)
    #Variable to store the samples from the mixture distribution                                             
   eps = rep(NA,N)
    #Sampling from the mixture
    for(i in 1:N){
      if(U[i]<.3){
        eps[i] = rnorm(1,-5.9,1)
      }else if(U[i]<.8){
        eps[i] = rnorm(1,4.1,1)
      }else{
        eps[i] = rnorm(1,-1.9,.1)
      }
    }
  }
  #Density plot of the random samples
  plot(density(eps),main="Density Estimate of the Mixture Model")
  ####################################################################
  if(model==5)
  {
    eps       <-  rt(N, df=1)# 
  }

  plot(density(eps),main="Density Estimate of the t-distribution")
  
  
  if(model==6)
  {
    #Sample N random uniforms U

    #Variable to store the samples from the mixture distribution                                             
    eps =  runif(N,-1,1)
    #Sampling from the mixture
  }
  
  ###Heteroscedastic error term
  beta.vec  <- c(1,1.5,-1.5,1.5)
  y         <- X %*% beta.vec + eps
  ##Solving for beta hat###
  beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
  beta.hat
  x.grid<-sort(X.2) #seq(min(X.2),max(X.2),le=N)
  true_model<-beta.vec[1]+beta.vec[2]*x.grid+beta.vec[3]*x.grid^2+beta.vec[4]*x.grid^3
  estim_model<-beta.hat[1]+beta.hat[2]*x.grid+beta.hat[3]*x.grid^2+beta.hat[4]*x.grid^3
  plot(estim_model,type="l")
  lines(true_model,type="l",lty=1,col="red")
  ####Calculate the covariance matrix###
  #####calculate the fitted values#####
  y.hat<- X %*% beta.hat 
  eps.hat<-y-X %*% beta.hat
  ###calculate the covariance matrix#
  se<-(t(eps.hat)%*%(eps.hat))/(N-5)
  cov<-se[1]*solve(t(X) %*% X)
  new_data<-X[order(X[,2]),]
  #new_data<-x.grid
  var<-c()
  for(i in 1:N)
  {
    var[i]=t(new_data[i,])%*%cov%*%(new_data[i,])
  }
  d<-2*(sqrt(var))
  estim<-beta.hat[1]+beta.hat[2]*new_data[,2]+beta.hat[3]*new_data[,2]^2+beta.hat[4]*new_data[,2]^3
  conf_low<-estim-d
  conf_high<-estim+d
  plot(estim,type="l")
  lines(conf_low,lty=2,col="red")
  lines(conf_high,lty=2,col="blue")
  ######Bootstrapped confidence intervals#####################
  B=N
  b_sample<-matrix(NA,N,B)
  beta.hat<-matrix(NA,length(X),B)
  data<-cbind(y,X)
  function_B<-matrix(NA,length(x.grid),B)
  
  for(i in 1:B)
  {
    XB<- data[sample(nrow(data),replace=T, N), ]
    beta.hat<-solve(t(XB[,2:5]) %*% XB[,2:5]) %*% t(XB[,2:5]) %*%XB[,1]
    function_B[,i]<-beta.hat[1]+beta.hat[2]*x.grid+beta.hat[3]*x.grid^2+beta.hat[4]*x.grid^3
  }
  plot(function_B[,1])
  plot(function_B[,2])
  sorted<-t(apply(function_B,1,sort)) 
  ####Calculate the empirical quantiles####################
  conf_highB<-sorted[,B*0.95]
  conf_lowB<-sorted[,B*0.05]
  ###########################################
  plot(estim,type="l",lty=3)
  lines(conf_highB)
  lines(conf_lowB)
  lines(conf_low,lty=2,col="red")
  lines(conf_high,lty=2,col="blue")
  lines(true_model,col="green")
  ####Calculate the coverage probabilities#################################
  cov_prob_B<-true_model>conf_lowB&true_model<conf_highB
  ratio1_B= sum(cov_prob_B)/length(x.grid)
  cov_prob_A<-true_model>conf_low&true_model<conf_high
  ratio1=sum(cov_prob_A)/length(x.grid)
  ###Calculate interval length##############################################
  int_lengthB<-conf_highB-conf_lowB
  int_length<-conf_high-conf_low
  ratio2=sum(int_lengthB/int_length>1)####counts the number of times the bootstrapped interval length is larger than the analytical one#########
  ratio2_int=mean(int_length)
  ratioB_int=mean(int_lengthB)
  return(list(ratio2=ratio2,ratio1_B=ratio1_B,ratio1=ratio1,ratio2_int=ratio2_int,ratioB_int=ratioB_int))
}
##########
reps<-50
result_1<-replicate(reps,data.conf(model=1,N=200)$ratio1)
result_1B<-replicate(reps,data.conf(model=1,N=200)$ratio1_B)
result_2<-replicate(reps,data.conf(model=1,N=200)$ratio2)
result_int<-replicate(reps,data.conf(model=1,N=200)$ratio2_int)
result_int_B<-replicate(reps,data.conf(model=1,N=200)$ratioB_int)
###cov.prob, analytical standard errors########
mean(result_1)
######cov_ probability, 
mean(result_1B)
######interval length ratio sum, if >0.5*N: ###############################
mean(result_2)
####avg.interval_length#############################################
mean(result_int)
####avg.interval_length#############################################
mean(result_int_B)
####
##########################################################################
####For the  Gamma######################################## 
result_1<-replicate(reps,data.conf(model=2,N=200)$ratio1)
result_1B<-replicate(reps,data.conf(model=2,N=200)$ratio1_B)
result_2<-replicate(reps,data.conf(model=2,N=200)$ratio2)
result_int<-replicate(reps,data.conf(model=2,N=200)$ratio2_int)
result_int_B<-replicate(reps,data.conf(model=2,N=200)$ratioB_int)
###cov.prob, analytical standard errors########
mean(result_1)
######cov_ probability, 
mean(result_1B)
######interval length ratio sum, if >0.5*N: ###############
mean(result_2)
####avg.interval_length#############################################
mean(result_int)
####avg.interval_length#############################################
mean(result_int_B)

####For the heteroscedastic errors######################################## 
result_1<-replicate(reps,data.conf(model=3,N=200)$ratio1)
result_1B<-replicate(reps,data.conf(model=3,N=200)$ratio1_B)
result_2<-replicate(reps,data.conf(model=3,N=200)$ratio2)
result_int<-replicate(reps,data.conf(model=3,N=200)$ratio2_int)
result_int_B<-replicate(reps,data.conf(model=3,N=200)$ratioB_int)
###cov.prob, analytical standard errors########
mean(result_1)
######cov_ probability, 
mean(result_1B)
######interval length ratio sum, if >0.5*N: ###############
mean(result_2)
####avg.interval_length#############################################
mean(result_int)
####avg.interval_length#############################################
mean(result_int_B)
####For the mixture model######################################## 
result_1<-replicate(reps,data.conf(model=4,N=200)$ratio1)
result_1B<-replicate(reps,data.conf(model=4,N=200)$ratio1_B)
result_2<-replicate(reps,data.conf(model=4,N=200)$ratio2)
result_int<-replicate(reps,data.conf(model=4,N=200)$ratio2_int)
result_int_B<-replicate(reps,data.conf(model=4,N=200)$ratioB_int)
###cov.prob, analytical standard errors########
mean(result_1)
######cov_ probability, 
mean(result_1B)
######interval length ratio sum, if >0.5*N: ###############
mean(result_2)
####avg.interval_length#############################################
mean(result_int)
####avg.interval_length#############################################
mean(result_int_B)
####For the t distribution######################################## 
result_1<-replicate(reps,data.conf(model=6,N=200)$ratio1)
result_1B<-replicate(reps,data.conf(model=6,N=200)$ratio1_B)
result_2<-replicate(reps,data.conf(model=6,N=200)$ratio2)
result_int<-replicate(reps,data.conf(model=6,N=200)$ratio2_int)
result_int_B<-replicate(reps,data.conf(model=6,N=200)$ratioB_int)
###cov.prob, analytical standard errors########
mean(result_1)
######cov_ probability, 
mean(result_1B)
######interval length ratio sum, if >0.5*N: ###############
mean(result_2)
####avg.interval_length#############################################
mean(result_int)
####avg.interval_length#############################################
mean(result_int_B)















############Different distribution####################################
N = 200                
#Sample N random uniforms U
U =runif(N)
#Variable to store the samples from the mixture distribution                                             
rand.samples = rep(NA,N)
#Sampling from the mixture
for(i in 1:N){
  if(U[i]<.3){
    rand.samples[i] = rnorm(1,-5.9,1)
  }else if(U[i]<.8){
    rand.samples[i] = rnorm(1,4.1,1)
  }else{
    rand.samples[i] = rnorm(1,-1.9,.1)
  }
}

#Density plot of the random samples
plot(density(rand.samples),main="Density Estimate of the Mixture Model")

