## Problem set 7 (Alonzo, Arias, Ducco)

#suppressMessages(install.packages("rpart"))
#suppressMessages(install.packages("rpart.plot"))
#suppressMessages(install.packages("ggplot2"))


suppressMessages(library("rpart"))
suppressMessages(library("rpart.plot"))
suppressMessages(library("ggplot2"))



set.seed(666) #we set a seed so that we could double check our codes

n<-100
mean_x<-0
sd_x<-4
mean_e<-0
sd_e<-1
beta<-c(3,4)


############################

## Question 1a)

data_gen <- function(n,mean_x,sd_x,mean_e,sd_e,beta){
  X_1<-rnorm(n,mean_x,sd_x)
  X_2<-rnorm(n,mean_x,sd_x)
  e<-rnorm(n,mean_e,sd_e)
  y<- beta[1]^(X_1) + beta[2]^(X_2) + e
  return(data.frame(X_1,X_2,y))
}
# We create a function to generate data

#fitting a pruned and long tree to the training data
data<-data_gen(n,mean_x,sd_x,mean_e,sd_e,beta)

tree<-rpart(formula=y ~ .,data=data)

long_tree<-rpart(formula=y~ .,data=data, control=list(cp=0))

rpart.plot(tree)

rpart.plot(long_tree)

## Question 1b)

# MSE of a linear regression model, full tree and pruned tree
test_set<-data_gen(n,mean_x,sd_x,mean_e,sd_e,beta)


lm_obj<-lm(y~ X_1 +X_2,data=data)


mean_sqe<-function(test,model){
  
  
  y_fit<-predict(model,test)
  return(mean((test$y-y_fit)^2))
  
}

print("mean squared error of linear regression: ")
mean_sqe(test_set,lm_obj)


print("mean squared error of pruned tree")

mean_sqe(test_set,tree)

print("mean squared error of longest tree")

mean_sqe(test_set,long_tree)

sqes<- c(mean_sqe(test_set,lm_obj),mean_sqe(test_set,tree),mean_sqe(test_set,long_tree))

names<-c("linear_regression",'pruned_tree','full_tree')
MSEE<-data.frame(names,sqes)
p<-ggplot(data=MSEE, aes(x=names, y=sqes)) +
  geom_bar(stat="identity")
p

## Question 2a)
# A dgp well suited for regression trees
data_gen_tree<-function(n,mean_x,sd_x,mean_e,sd_e,beta){
  X_1<-rnorm(n,mean_x,sd_x)
  X_2<-rnorm(n,mean_x,sd_x)
  e<-rnorm(n,mean_e,sd_e)
  y<- -100*beta[1]*(X_1>=1) +100*beta[2]*(X_1<1) + e
  return(data.frame(X_1,X_2,y))
}

scores_a<-matrix(NA,3,100)

for (i in 1:100){
  
  
  train<-data_gen_tree(n,mean_x,sd_x,mean_e,sd_e,beta)
  
  
  test<-data_gen_tree(n,mean_x,sd_x,mean_e,sd_e,beta)
  
  
  lm_obj<-lm(y~X_1 + X_2,train)
  
  
  pruned_tree<-rpart(y~.,train,method='anova')
  
  
  long_tree<-rpart(y~.,train,method='anova',control=list(cp=0))
  
  scores_a[,i]<-c(mean_sqe(test,lm_obj),mean_sqe(test,pruned_tree),mean_sqe(test,long_tree))
}

print("means")
rowMeans(scores_a)
print("standard errors")
sqrt(diag(cov(t(scores_a))))

boxplot(t(scores_a))

## Question 2b)
# A dgp well suited for the linear regression model

data_gen_lin<-function(n,mean_x,sd_x,mean_e,sd_e,beta){
  X_1<-rnorm(n,mean_x,sd_x)
  X_2<-rnorm(n,mean_x,sd_x)
  e<-rnorm(n,mean_e,sd_e)
  y<- beta[1]*X_1 + beta[2]*X_2 + e
  return(data.frame(X_1,X_2,y))
}

scores_b<-matrix(NA,3,100)

for (i in 1:100){
  
  train<-data_gen_lin(n,mean_x,sd_x,mean_e,sd_e,beta)
  
  
  test<-data_gen_lin(n,mean_x,sd_x,mean_e,sd_e,beta)
  
  
  lm_obj<-lm(y~X_1 + X_2,train)
  
  
  pruned_tree<-rpart(y~.,method='anova',train)
  
  
  long_tree<-rpart(y~.,train,method='anova',control=list(cp=0))
  
  scores_b[,i]<-c(mean_sqe(test,lm_obj),mean_sqe(test,pruned_tree),mean_sqe(test,long_tree))
}

print("means")
rowMeans(scores_b)
print("standard deviation")
sqrt(diag(cov(t(scores_b))))

boxplot(t(scores_b))

## Question 2c)
#First some explanations about how does cp works
long_tree$cptable[,1:2]

cp_vals<-long_tree$cptable[,1]

rpart.plot(rpart(y~.,train,method='anova',control=list(cp=cp_vals[2])))


rpart.plot(rpart(y~.,train,method='anova',control=list(cp=cp_vals[4])))

rpart.plot(rpart(y~.,train,method='anova',control=list(cp=cp_vals[6])))

#How does pruning reduce the variance?
variances<-matrix(NA,10,100)

for (j in 1:100){
  
  train<-data_gen(n,mean_x,sd_x,mean_e,sd_e,beta)
  test<-data_gen(n,mean_x,sd_x,mean_e,sd_e,beta)
  
  tree<-rpart(y~.,train,control=list(cp=0))
  
  cp_vals<-tree$cptable[,1]
  
  for (i in 1:10){
    
    
    model=rpart(y~., data=data, method='anova', control=list(cp=cp_vals[i])) 
    variances[i,j]<-(sd(predict(model,test)))^2
    
  }
}
vars<-rowMeans(variances)
plot(vars[2:10])

## Repeating Question 2b by putting more noise to the model as discussed at class
variances_new<-matrix(NA,10,100)

sd_e_new <- 10000 # very big standard error as the numbers of the model are very high

for (j in 1:100){
  
  train<-data_gen(n,mean_x,sd_x,mean_e,sd_e_new,beta)
  test<-data_gen(n,mean_x,sd_x,mean_e,sd_e_new,beta)
  
  tree<-rpart(y~.,train,control=list(cp=0))
  
  cp_vals<-tree$cptable[,1]
  
  for (i in 1:10){
    
    
    model=rpart(y~., data=data, method='anova', control=list(cp=cp_vals[i])) 
    variances_new[i,j]<-(sd(predict(model,test)))^2
    
  }
}
vars_new<-rowMeans(variances_new)
plot(vars_new[2:10])
#It confirm our hypothesis that with more noise there is more benefits of pruning a tree
