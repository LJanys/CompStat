#Excercise1
#a Generate the data and fit a regression tree
set.seed(666)
#install.packages("tree")
#install.packages('tree', repos = "http://cran.us.r-project.org")
library (tree)
library(rpart)
#install.packages("partykit")
library(partykit)
#install.packages("mlr")
library(mlr)
#install.packages("devtools")
library(devtools)
#install.packages("glmnet")
library(glmnet)
#install.packages("rpart.plot")
library(rpart.plot)

n <- 100
dgp <- function(n){
  x1 <- rnorm(n = n, 0, 4) 
  x2 <- rnorm(n = n, 0, 4)
  y <- (x1^2)+(x2^3)
  matrix <- cbind(y,x1,x2)
  data <-data.frame(matrix)}
data <- dgp(n=n)
getParamSet("regr.rpart")##check which para can be tweak and wut is defualt value

rpart_df <- rpart(y ~ ., data,control = c(cp = 0,minsplit = 0))
summary(rpart_df)

prp(rpart_df,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output)
summary(rpart_df)


 
#b
df_test <- dgp(n = 100)
rpart_pred <- predict(rpart_df,df_test)
sse_rpart <- sum((df_test['y']-rpart_pred)^2)
mse_rpart <- sse_rpart/100 


reg.df <- lm(y~.,data)
pred_y_lm <- predict(reg.df,df_test)
sse_lm <- sum((df_test['y']-pred_y_lm)^2)
mse_lm <- sse_lm/100
mse_lm

##Pruned the tree
#grid_search to search through the best parameter values from a given set of grid of parameters, 
#basically a cross-validation method. The model and parameters are fed in, best parameter values are extracted, then prediction!
d.tree.params <- makeRegrTask(
  data=data, 
  target="y"
)

param_grid_multi <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=1:30),
  makeNumericParam("cp", lower = 0.001, upper = 0.01),
  makeDiscreteParam("minsplit", values= 1:10),
  makeDiscreteParam("minbucket",value = 1:10)
)



control_grid = makeTuneControlGrid()
resample = makeResampleDesc("CV", iters = 2L)#2 folders validation
measure = expvar#explained variance

dt_tuneparam <- tuneParams(learner="regr.rpart", 
                           task=d.tree.params, 
                           resampling = resample,
                           measures = measure,
                           par.set=param_grid_multi, 
                           control=control_grid, 
                           show.info = TRUE)
dt_tuneparam

rpart_df_pruned <- rpart(y ~ ., data,control = c(cp = 0.005, 
                                                 maxdepth = 6, minsplit =2, minbucket = 1))
rpart_pred_pruned <- predict(rpart_df_pruned,df_test)
sse_rpart_pruned <- sum((df_test['y']-rpart_pred_pruned)^2)
mse_rpart_pruned <- sse_rpart_pruned/100

##to plot rpart trees
prp(rpart_df_pruned,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output)
summary(rpart_df_pruned)



#############################################Outcome
outcome <- cbind(mse_rpart,mse_rpart_pruned,mse_lm)
outcome <- data.frame(outcome)
rownames(outcome) <- "Mean_squared_error"
colnames(outcome) <- c("Unpruned_tree","Pruned_tree","Linear_model")
outcome




#Excercise2
#a
#Excercise2 Simulation tree
#a Propose a dgp that will be well suited for analysis using regression trees and evaluate relevant properties in a small simulation study.
set.seed(1)
T = 1000
n = 100

dgp2 <- function(n){
  x1 <- rlnorm(n = n, 1, log(4))
  x2 <- rnorm(n = n, 0, 4)
  x3 <- rbinom(n = n,2,0.3)
  x4 <- rbinom(n = n,1,0.5)
  y <-  x1+x2^2+x3+x4
  matrix <- cbind(y,x1,x2,x3,x4)
  data <-data.frame(matrix)}
data <- dgp2(n)
data

#construct ols, lasso, ridge and compare mse with regression tree 1000 times
treemse<- c()
lmmse <- c()
compare <- c()
lasso_tree <- c()
ridge_tree <- c()
lambdas <- 10^seq(2, -3, by =-.1)
lasso <- c()
ridge <- c()
for (t in 1:T){
  df_train <- dgp2(n)
  df_test <- dgp2(n)
  df.tree <- tree(y~.,df_train)
  y_hat_tree <- predict(df.tree,df_test)
  treemse[t] <- sum((df_test['y']-y_hat_tree)^2) 
  ##Lm
  df.lm <- lm(y~.,df_train)
  y_hat_lm <- predict(df.lm,df_test)
  lmmse[t] <- sum((df_test['y']-y_hat_lm)^2)
  if (treemse[t]<lmmse[t]){
    compare[t] <- 1
  }
  else{compare[t] <- 0}
  ##Lasso
  x <- data.matrix(df_train[,c("x1","x2")])
  y <- df_train$y
  x.test <- data.matrix(df_test[,c("x1","x2")])
  lasso_reg <- cv.glmnet(x,y, alpha = 1, lambda = lambdas, 
                         standardize = TRUE, nfolds = 5)
  lambda_best <- lasso_reg$lambda.min
  lasso_model <- glmnet(x,y, alpha = 1, lambda = lambda_best, standardize = TRUE)
  predictions_train <- predict(lasso_model, s = lambda_best, newx = x.test)
  lasso[t] <- sum((df_test['y']-predictions_train)^2)
  if (treemse[t]>lasso[t]){
    lasso_tree[t] <- 0
  }
  else{lasso_tree[t] <- 1}
  
  ##Ridge
  
  ridge_reg <- cv.glmnet(x,y, alpha = 0, lambda = lambdas, 
                         standardize = TRUE, nfolds = 5)
  lambda_best.r <- ridge_reg$lambda.min
  ridge_model <- glmnet(x,y, alpha = 0, lambda = lambda_best.r, standardize = TRUE)
  predictions_train <- predict(ridge_model, s = lambda_best.r, newx = x.test)
  ridge[t] <- sum((df_test['y']-predictions_train)^2)
  if (treemse[t]>ridge[t]){
    ridge_tree[t] <- 0
  }
  else{ridge_tree[t] <- 1}
}

mean(treemse)
mean(lmmse)
mean(lasso)
mean(ridge)
sum(compare)
sum(lasso_tree)
sum(ridge_tree)



#Exercise 2
#b Propose a dgp that will be well suited for analysis using the linear regression model 
#(i.e. where the linear regression model is more likely to “beat" the regression tree method) and evaluate.
set.seed(1)
T = 500
n = 100
dgp3 <- function(n){
  x1 <- rnorm(n = n, 0, 4) 
  x2 <- rnorm(n = n, 0, 4)
  y <- x1+x2
  matrix <- cbind(y,x1,x2)
  data <-data.frame(matrix)}
treemse<- c()
lmmse <- c()
compare <- c()
for (t in 1:T){
  df_train <- dgp3(n)
  df_test <- dgp3(n)
  df.tree <- tree(y~.,df_train)
  y_hat_tree <- predict(df.tree,df_test)
  treemse[t] <- sum((df_test['y']-y_hat_tree)^2) 
  ##
  df.lm <- lm(y~.,df_train)
  y_hat_lm <- predict(df.lm,df_test)
  lmmse[t] <- sum((df_test['y']-y_hat_lm)^2)
  if (treemse[t]>lmmse[t]){
    compare[t] <- 1
  }
  else{compare[t] <- 0}
}

mean(treemse)
mean(lmmse)
sum(compare)


#c
set.seed(666)
T = 50
error_unpruned <- c()
error_pruned <- c()
prunedTreeWin <- c()
variance_decrease <- c()
for(t in 1:T){
  df_train <- dgp2(100)
  df.test <- dgp2(100)
  df.tree <- rpart(y~.,df_train,control = c(cp = 0,minsplit = 0))
  unpruned <- predict(df.tree,df.test)
  error_unpruned[t] <- sum((df.test['y']-unpruned)^2)
  
  #pruned
  d.tree.params <- makeRegrTask(
  data=df_train, 
  target="y")
  
  param_grid_multi <- makeParamSet( 
  makeNumericParam("cp", lower = 0.001, upper = 0.01),
  makeDiscreteParam("minbucket",value = 1:10))
  
  control_grid = makeTuneControlGrid()
  resample = makeResampleDesc("CV", iters = 2L)#2 folders validation
  measure = expvar#explained variance
  
  dt_tuneparam <- tuneParams(learner="regr.rpart", 
                           task=d.tree.params, 
                           resampling = resample,
                           measures = measure,
                           par.set=param_grid_multi, 
                           control=control_grid, 
                           show.info = TRUE)
  best_parameters = setHyperPars(
    makeLearner("regr.rpart", predict.type = "response"), 
    par.vals = dt_tuneparam$x)
  
  best_model = train(best_parameters, d.tree.params)
  
  d.tree.mlr.test <- makeRegrTask(
    data=df.test, 
    target="y")
  
  results <- predict(best_model, task = d.tree.mlr.test)$data
  error_pruned[t] <- sum((df.test['y']-results[["data"]][["response"]])^2)
  if (error_pruned[t]<error_unpruned[t]){
    prunedTreeWin[t] <- 1}
  else{prunedTreeWin[t] <- 0}
  variance_decrease[t] <- (error_pruned[t] - error_unpruned[t])
}
sum(prunedTreeWin)
variance_decrease
mean(variance_decrease)

#end


















