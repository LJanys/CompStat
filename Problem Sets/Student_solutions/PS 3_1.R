rm(list = ls())


#setup
#install.packages("mvtnorm")
#install.packages("MASS")
library(mvtnorm)
library(MASS)
#install.packages("tidyverse")
library(tidyverse)

#install.packages("ggplot2")
library(ggplot2)

mu1 <- c(-3, 3)
mu2 <- c(5, 5)

Val <- c(16,-2,-2,9)
cov <- matrix(data = Val,2,2 )

#a)

n1 <- 300
n2 <- 500
n  <- 800 #by assumption of us

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


#b)

#LDA

model.training <- lda(y_original~.,data = trainingdata_final) 
#estimates the model with pi being 0.375
prediction.training <- model.training %>% predict(trainingdata_final) 
# %>% forward pipe operator
#predict: explanatory variables are looked for 
#and a prediction is made using LDA coefficients from model.training

lda.data_training <- cbind(trainingdata_final,prediction.training)



#Logistic Regression

log_train <- glm(trainingdata_final$y_original~trainingdata_x_log,data = trainingdata_final,family= 'binomial')

training_log_bef_pi <- cbind(rep(1,n),trainingdata_x_log)

#to reduce the calculation time just exclude the print fct.
#calculate training p(x)
pi_log_train <- c()
for (i in 1:n) {
  pi_log_train[i] <- exp(as.vector(log_train$coefficients)%*%as.vector(t(training_log_bef_pi[i,])))/(1+exp(as.vector(log_train$coefficients)%*%as.vector(t(training_log_bef_pi[i,]))))
  #print(pi_log_train)
}

#training results for y
#assign whether  pi_log train is 0.375 or not
y_train_log <- c()
for (i in 1:n) {
  if (pi_log_train[i]>0.375)  y_train_log[i] = 1 else y_train_log[i]=0
  #print(y_train_log)
}


test_log_bef_pi <- cbind(rep(1,n),testdata_x_log)

#test pi
pi_log_test <- c()
for (i in 1:n) {
  pi_log_test[i] <- exp(as.vector(log_train$coefficients)%*%as.vector(t(test_log_bef_pi[i,])))/(1+exp(as.vector(log_train$coefficients)%*%as.vector(t(test_log_bef_pi[i,]))))
  #print(pi_log_test)
}

#test y
y_test_log <- c()
for (i in 1:n) {
  if (pi_log_test[i]>0.375)  y_test_log[i] = 1 else y_test_log[i]=0
  #print(y_test_log)
}

#c)


#LDA

prediction.test <- model.training %>% predict(testdata_final)
lda.data_test <- cbind(testdata_final,prediction.test)

#head(lda.data_training)



MSE_training.lda <- mean((trainingdata_final$y_original- strtoi((lda.data_training$class)))^2)
MSE_test.lda <- mean((testdata_final$y_original_test- strtoi((lda.data_test$class)))^2) #strtoi = string to integer


#Logistic Regression

MSE_training.log <- mean(((trainingdata_final$y_original)-y_train_log)^2)
MSE_test.log <- mean(((testdata_final$y_original_test)-y_test_log)^2)

#d)
#Idea: For good predictions the sensitivity and specificity should comparable smaller,
#as the total error is smallest

#training
#LDA
MSE_training.lda_spec <- lda.data_training$y_original- strtoi((lda.data_training$class))

#algorithm to order the errors
spez.lda.training <- c()
for (i in 1:length(MSE_training.lda_spec)){
  if (MSE_training.lda_spec[i]==1) spez.lda.training[i]="T1"
  if (MSE_training.lda_spec[i]==-1) spez.lda.training[i]="T2"
  if (MSE_training.lda_spec[i]==0 & lda.data_training$y_original[i]==1) spez.lda.training[i]="Yes_Yes"
  if (MSE_training.lda_spec[i]==0 & lda.data_training$y_original[i]==0) spez.lda.training[i]="No_No" 
}

train.No_No<- sum(spez.lda.training == "No_No")
train.T2 <- sum(spez.lda.training == "T2")
train.Yes_Yes <- sum(spez.lda.training == "Yes_Yes")
train.T1 <- sum(spez.lda.training == "T1")

specifity.train.lda <- 1-(train.T2/(train.T2+train.No_No))
sensitivity.train.lda <- (train.Yes_Yes)/(train.Yes_Yes+train.T1)

#Logistic

MSE_training.logistic_spec <- lda.data_training$y_original- y_train_log


spez.logistic.training <- c()
for (i in 1:length(MSE_training.logistic_spec)){
  if (MSE_training.logistic_spec[i]==1) spez.logistic.training[i]="T1"
  if (MSE_training.logistic_spec[i]==-1) spez.logistic.training[i]="T2"
  if (MSE_training.logistic_spec[i]==0 & lda.data_training$y_original[i]==1) spez.logistic.training[i]="Yes_Yes"
  if (MSE_training.logistic_spec[i]==0 & lda.data_training$y_original[i]==0) spez.logistic.training[i]="No_No" 
}

train.No_No.log<- sum(spez.logistic.training == "No_No")
train.T2.log <- sum(spez.logistic.training == "T2")
train.Yes_Yes.log <- sum(spez.logistic.training == "Yes_Yes")
train.T1.log <- sum(spez.logistic.training == "T1")


specifity.train.logistic <- 1-(train.T2.log/(train.T2.log+train.No_No.log))
sensitivity.train.logistic <- (train.Yes_Yes.log)/(train.Yes_Yes.log+train.T1.log)


#test

#lda
MSE_test.lda_spec <- lda.data_test$y_original- strtoi((lda.data_test$class))


spez.lda.test <- c()
for (i in 1:length(MSE_test.lda_spec)){
  if (MSE_test.lda_spec[i]==1) spez.lda.test[i]="T1"
  if (MSE_test.lda_spec[i]==-1) spez.lda.test[i]="T2"
  if (MSE_test.lda_spec[i]==0 & lda.data_test$y_original_test[i]==1) spez.lda.test[i]="Yes_Yes"
  if (MSE_test.lda_spec[i]==0 & lda.data_test$y_original_test[i]==0) spez.lda.test[i]="No_No" 
}

test.No_No<- sum(spez.lda.test == "No_No")
test.T2 <- sum(spez.lda.test == "T2")
test.Yes_Yes <- sum(spez.lda.test == "Yes_Yes")
test.T1 <- sum(spez.lda.test == "T1")

specifity.test.lda <- 1-(test.T2/(test.T2+test.No_No))
sensitivity.test.lda <- (test.Yes_Yes)/(test.Yes_Yes+test.T1)

#logistic

MSE_test.logistic_spec <- lda.data_test$y_original- y_test_log


spez.logistic.test <- c()
for (i in 1:length(MSE_test.logistic_spec)){
  if (MSE_test.logistic_spec[i]==1) spez.logistic.test[i]="T1"
  if (MSE_test.logistic_spec[i]==-1) spez.logistic.test[i]="T2"
  if (MSE_test.logistic_spec[i]==0 & lda.data_test$y_original_test[i]==1) spez.logistic.test[i]="Yes_Yes"
  if (MSE_test.logistic_spec[i]==0 & lda.data_test$y_original_test[i]==0) spez.logistic.test[i]="No_No" 
}

test.No_No.log<- sum(spez.logistic.test == "No_No")
test.T2.log <- sum(spez.logistic.test == "T2")
test.Yes_Yes.log <- sum(spez.logistic.test == "Yes_Yes")
test.T1.log <- sum(spez.logistic.test == "T1")

specifity.test.logistic <- 1-(test.T2.log/(test.T2.log+test.No_No.log))
sensitivity.test.logistic <- (test.Yes_Yes.log)/(test.Yes_Yes.log+test.T1.log)




