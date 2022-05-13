rm(list = ls())

set.seed(100)
#setup
#install.packages("mvtnorm")
#install.packages("MASS")
library(mvtnorm)
library(MASS)
#install.packages("tidyverse")
library(tidyverse)

#install.packages("ggplot2")
library(ggplot2)

#Aufgabe 2

mu1 <- c(-3, 3)
mu2 <- c(5, 5)

Val <- c(16,-2,-2,9)
cov <- matrix(data = Val,2,2 )

#a)

#LDA
n_sim <- 100
mse.sim.lda.training <- c()
mse.sim.lda.test <- c()
mse.sim.log.training <- c()
mse.sim.log.test <- c()

#container for specificity and sensitivity
MSE_training.lda_spec <- c()
spez.lda.training <- c()
train.No_No <- c()
train.T2 <- c()
train.Yes_Yes <- c()
train.T1 <- c()

specifity.train.lda <- c()
sensitivity.train.lda<- c()


#container tests for specificity and sensitivity
MSE_test.lda_spec <- c()
spez.lda.test <- c()
test.No_No<- c()
test.T2 <- c()
test.Yes_Yes <- c()
test.T1 <- c()

specifity.test.lda<- c()
sensitivity.test.lda<- c()



for (i in 1:n_sim) {
  
  #a)
  
  n1 <- 300
  n2 <- 500
  n  <- n1+n2 #by assumption of us
  
  pi_1 <- n1/n
  pi_2 <- n2/n
  
  #trainset <- 800
  #testset <- 200
  
  x1 <- mvrnorm(n1, mu1, cov ) #not a regressor
  x2 <- mvrnorm(n2, mu2, cov ) #not a regressor
  
  
  y_original1<- rep(1,n1)
  y_original2<- rep(0,n2)
  
  
  #print(y_orginal)
  trainingdata1 <- data.frame(y_original1, x1)
  names(trainingdata1) <- c("y_original","X1","X2")
  
  trainingdata2 <- data.frame(y_original2, x2)
  names(trainingdata2) <- c("y_original","X1","X2")
  
  trainingdata_final<- rbind(trainingdata1,trainingdata2)
  
  trainingdata_x_log <- t(rbind(trainingdata_final$X1,trainingdata_final$X2))
  names(trainingdata_x_log) <- c("X1","X2")
  
  #generating of testset
  
  x1_test <- mvrnorm(n1, mu1, cov )
  x2_test <- mvrnorm(n2, mu2, cov )
  
  
  
  
  testdata1 <- data.frame(y_original1, x1_test)
  names(testdata1) <- c("y_original_test","X1","X2")
  
  testdata2 <- data.frame(y_original2, x2_test)
  names(testdata2) <- c("y_original_test","X1","X2")
  
  
  testdata_final<- rbind(testdata1,testdata2)
  
  
  testdata_x_log <- t(rbind(testdata_final$X1,testdata_final$X2))
  names(testdata_x_log) <- c("X1","X2")
  
  

  
  #LDA
  
  model.training <- lda(y_original~.,data = trainingdata_final) #prior = c(0.5,0.5)) #, prior = 0.2
  prediction.training <- model.training %>% predict(trainingdata_final)
  #understand this equation!!!
  lda.data_training <- cbind(trainingdata_final,prediction.training)
  
  
  
  
  
  #LDA
  
  prediction.test <- model.training %>% predict(testdata_final)
  lda.data_test <- cbind(testdata_final,prediction.test)
  
  head(lda.data_training)
  
  #LDA
  
  mse.sim.lda.training[i] <- mean((lda.data_training$y_original- strtoi((lda.data_training$class)))^2)
  mse.sim.lda.test[i] <- mean((lda.data_test$y_original_test- strtoi((lda.data_test$class)))^2)


  
  
  #This here is the calculation of the specificity and sensitivity

  #training
  MSE_training.lda_spec <- lda.data_training$y_original- strtoi((lda.data_training$class))
  

  for (a in 1:length(MSE_training.lda_spec)){
    if (MSE_training.lda_spec[a]==1) spez.lda.training[a]="T1"
    if (MSE_training.lda_spec[a]==-1) spez.lda.training[a]="T2"
    if (MSE_training.lda_spec[a]==0 & lda.data_training$y_original[a]==1) spez.lda.training[a]="Yes_Yes"
    if (MSE_training.lda_spec[a]==0 & lda.data_training$y_original[a]==0) spez.lda.training[a]="No_No" 
  }
  train.No_No<- sum(spez.lda.training == "No_No")
  train.T2 <- sum(spez.lda.training == "T2")
  train.Yes_Yes <- sum(spez.lda.training == "Yes_Yes")
  train.T1 <- sum(spez.lda.training == "T1")
  
  specifity.train.lda[i] <- 1-(train.T2/(train.T2+train.No_No))
  sensitivity.train.lda[i] <- (train.Yes_Yes)/(train.Yes_Yes+train.T1)

  #test
  #lda
  MSE_test.lda_spec <- lda.data_test$y_original- strtoi((lda.data_test$class))
  
  
  spez.lda.test <- c()
  for (b in 1:length(MSE_test.lda_spec)){
    if (MSE_test.lda_spec[b]==1) spez.lda.test[b]="T1"
    if (MSE_test.lda_spec[b]==-1) spez.lda.test[b]="T2"
    if (MSE_test.lda_spec[b]==0 & lda.data_test$y_original_test[b]==1) spez.lda.test[b]="Yes_Yes"
    if (MSE_test.lda_spec[b]==0 & lda.data_test$y_original_test[b]==0) spez.lda.test[b]="No_No" 
  }
  
  test.No_No<- sum(spez.lda.test == "No_No")
  test.T2 <- sum(spez.lda.test == "T2")
  test.Yes_Yes <- sum(spez.lda.test == "Yes_Yes")
  test.T1 <- sum(spez.lda.test == "T1")
  
  specifity.test.lda[i] <- 1-(test.T2/(test.T2+test.No_No))
  sensitivity.test.lda[i] <- (test.Yes_Yes)/(test.Yes_Yes+test.T1)

  
}


#hist(specifity.train.lda)
#abline(v = mean(specifity.train.lda), col = "red")
#hist(sensitivity.train.lda)
#abline(v = mean(sensitivity.train.lda), col = "red")










#logistic container training set for sensitivity and specificity
#train
spez.logistic.training <- c()
MSE_training.logistic_spec <- c()


train.No_No.log <- c()
train.T2.log <- c()
train.Yes_Yes.log <- c()
train.T1.log <- c()

specifity.train.logistic <- c()
sensitivity.train.logistic <- c()
#test
spez.logistic.test <- c()
MSE_test.logistic_spec <- c()

test.No_No.log<- c()
test.T2.log <- c()
test.Yes_Yes.log <- c()
test.T1.log <- c()

specifity.test.logistic <- c()
sensitivity.test.logistic <- c()

#logistic-----------------------------------


for (k in 1:n_sim) {
  
  n1 <- 300
  n2 <- 500
  n  <- n1+n2 #by assumption of us
  
  pi_1 <- n1/n
  pi_2 <- n2/n
  
  x1 <- mvrnorm(n1, mu1, cov ) #not a regressor
  x2 <- mvrnorm(n2, mu2, cov ) #not a regressor
  
  
  y_original1<- rep(1,n1)
  y_original2<- rep(0,n2)
  
  
  #print(y_orginal)
  trainingdata1 <- data.frame(y_original1, x1)
  names(trainingdata1) <- c("y_original","X1","X2")
  
  trainingdata2 <- data.frame(y_original2, x2)
  names(trainingdata2) <- c("y_original","X1","X2")
  
  trainingdata_final<- rbind(trainingdata1,trainingdata2)
  names(trainingdata_final) <- c("y_original","X1","X2")
  
  trainingdata_x_log <- t(rbind(trainingdata_final$X1,trainingdata_final$X2))
  names(trainingdata_x_log) <- c("X1","X2")
  
  #generating of testset
  
  n1_test <- 300
  n2_test <- 500
  
  
  x1_test <- mvrnorm(n1_test, mu1, cov )
  x2_test <- mvrnorm(n2_test, mu2, cov )
  
  y_original1<- rep(1,n1_test)
  y_original2<- rep(0,n2_test)
  
  
  testdata1 <- data.frame(y_original1, x1_test)
  names(testdata1) <- c("y_original_test","X1","X2")
  
  testdata2 <- data.frame(y_original2, x2_test)
  names(testdata2) <- c("y_original_test","X1","X2")
  
  
  testdata_final<- rbind(testdata1,testdata2)
  names(testdata_final) <- c("y_original_test","X1","X2")
  
  
  testdata_x_log <- t(rbind(testdata_final$X1,testdata_final$X2))
  names(testdata_x_log) <- c("X1","X2")
  
  
  
  #Logistic Regression
  
  log_train <- glm(trainingdata_final$y_original~trainingdata_x_log,data = trainingdata_final,family= 'binomial')
  
  training_log_bef_pi <- cbind(rep(1,n),trainingdata_x_log)
  
  #to reduce the calculation time just exclude the print fct.
  pi_log_train <- c()
  for (i in 1:n) {
    pi_log_train[i] <- exp(as.vector(log_train$coefficients)%*%as.vector(t(training_log_bef_pi[i,])))/(1+exp(as.vector(log_train$coefficients)%*%as.vector(t(training_log_bef_pi[i,]))))
    #print(pi_log_train)
  }
  
  y_train_log <- c()
  for (m in 1:n) {
    if (pi_log_train[m]>0.375)  y_train_log[m] = 1 else y_train_log[m]=0
    #print(y_train_log)
  }
  
  
  test_log_bef_pi <- cbind(rep(1,n),testdata_x_log)
  
  pi_log_test <- c()
  for (l in 1:n) {
    pi_log_test[l] <- exp(as.vector(log_train$coefficients)%*%as.vector(t(test_log_bef_pi[l,])))/(1+exp(as.vector(log_train$coefficients)%*%as.vector(t(test_log_bef_pi[l,]))))
    #print(pi_log_test)
  }
  
  y_test_log <- c()
  for (j in 1:n) {
    if (pi_log_test[j]>0.375)  y_test_log[j] = 1 else y_test_log[j]=0
    #print(y_test_log)
  }
  
  
  mse.sim.log.training[k] <- mean(((trainingdata_final$y_original)-y_train_log)^2)
  mse.sim.log.test[k] <- mean(((testdata_final$y_original_test)-y_test_log)^2)
  
  
  
  #Logistic Regression Specificity and Sensitivity
  #training
  MSE_training.logistic_spec <- trainingdata_final$y_original- y_train_log
  
  
 
  for (a in 1:length(MSE_training.logistic_spec)){
   if (MSE_training.logistic_spec[a]==1) spez.logistic.training[a]="T1"
    if (MSE_training.logistic_spec[a]==-1) spez.logistic.training[a]="T2"
    if (MSE_training.logistic_spec[a]==0 & trainingdata_final$y_original[a]==1) spez.logistic.training[a]="Yes_Yes"
    if (MSE_training.logistic_spec[a]==0 & trainingdata_final$y_original[a]==0) spez.logistic.training[a]="No_No" 
  }
  
  train.No_No.log<- sum(spez.logistic.training == "No_No")
  train.T2.log <- sum(spez.logistic.training == "T2")
  train.Yes_Yes.log <- sum(spez.logistic.training == "Yes_Yes")
  train.T1.log <- sum(spez.logistic.training == "T1")
  
  specifity.train.logistic[k] <- 1-(train.T2.log/(train.T2.log+train.No_No.log))
  sensitivity.train.logistic[k] <- (train.Yes_Yes.log)/(train.Yes_Yes.log+train.T1.log)
  
  #test
  MSE_test.logistic_spec <- testdata_final$y_original- y_test_log
  
  
  for (b in 1:length(MSE_test.logistic_spec)){
    if (MSE_test.logistic_spec[b]==1) spez.logistic.test[b]="T1"
    if (MSE_test.logistic_spec[b]==-1) spez.logistic.test[b]="T2"
    if (MSE_test.logistic_spec[b]==0 & testdata_final$y_original_test[b]==1) spez.logistic.test[b]="Yes_Yes"
    if (MSE_test.logistic_spec[b]==0 & testdata_final$y_original_test[b]==0) spez.logistic.test[b]="No_No" 
 }
  
  test.No_No.log<- sum(spez.logistic.test == "No_No")
  test.T2.log <- sum(spez.logistic.test == "T2")
  test.Yes_Yes.log <- sum(spez.logistic.test == "Yes_Yes")
  test.T1.log <- sum(spez.logistic.test == "T1")
  
  specifity.test.logistic[k] <-  1-(test.T2.log/(test.T2.log+test.No_No.log))
  sensitivity.test.logistic[k] <- (test.Yes_Yes.log)/(test.Yes_Yes.log+test.T1.log)
  
  
  
}


#hist(specifity.train.logistic)
#abline(v = mean(specifity.train.logistic), col = "red")
#hist(sensitivity.train.logistic)
#abline(v = mean(sensitivity.train.logistic), col = "red")








#lda analysis of mse

#training
hist(mse.sim.lda.training, xlab = "MSE of LDA",main = paste("Histogram of" , "Training MSE LDA with 100 Simulations"))
abline(v = mean(mse.sim.lda.training), col = "red")

mean_mse_lda_training <- mean(mse.sim.lda.training)
var_mse_lda_training <- var(mse.sim.lda.training)

#test
hist(mse.sim.lda.test, xlab = "MSE of LDA",main = paste("Histogram of" , "Test MSE LDA with 100 Simulations"))
abline(v = mean(mse.sim.lda.test), col = "red")

mean_mse_lda_test <- mean(mse.sim.lda.test)
var_mse_lda_test <- var(mse.sim.lda.test)



#logistic analysis of mse

#training
hist(mse.sim.log.training, xlab = "MSE of LRM",main = paste("Histogram of" , "Training MSE LRM with 100 Simulations"))
abline(v = mean(mse.sim.log.training), col = "red")
mean_mse_log_training <- mean(mse.sim.log.training)
var_mse_log_training <- var(mse.sim.log.training)

#test
hist(mse.sim.log.test, xlab = "MSE of LRM", main = paste("Histogram of" , " Test MSE LRM with 100 Simulations"))
abline(v = mean(mse.sim.log.test), col = "red")
mean_mse_log_test <- mean(mse.sim.log.test)
var_mse_log_test <- var(mse.sim.log.test)





#b)

#1) if n is small and normal distributed, the MSE of the lda should be smaller
#2) with lda we can have more than two response classes, this case we cannot compare to
# lrm, as lrm cannot have more than two
#3) we have to try but the question here is what is happening when the classes have a very different mean
# When there is substantial separation between the two classes, the
#parameter estimates for the logistic regression model are surprisingly
#unstable.





#c)
#sensitivity and specificity increases when I decrease pi_1 and pi_2, e.g. the posterior probability of default increases. 
#It increases because above the threshold of e.g. 20% instead of before 50% every Household is put into default.
#This leads to better predicting sensitivity but will increase specificity and in total the error.




