###### PROBLEM SET 4 ######

rm(list = ls())

## Packages for the exercises
library("ggplot2")
set.seed(42)
library(cowplot) # to arrange plots nicely above each other/ in a grid


##### EXERCISE 1 #####

### specification

#variable parameters
n <- 500 # we chose this value relatively low in order to have shorter calculation time
sigma_2 <- 1 # we chose this value for simplicity
se <- 1

#parameters as given in the PS
beta_0 <- 1 
beta_1 <- 1.5
beta_2 <- -1.5
beta_3 <- 1.5
beta_4 <- 0.5
beta <- c(beta_0, beta_1, beta_2, beta_3, beta_4)


# draw training and test data
x_train <- rnorm(n, mean=0, sd=sqrt(sigma_2))
X_train <- cbind(rep(1, n), x_train, x_train^2, x_train^3, x_train^4)
error_train <- rnorm(n, sd = se)
y_train <- X_train%*%beta + error_train

x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2))) # we will use the fact that it is sorted later
X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
error_test <- rnorm(n, sd = se)
y_test <- X_test%*%beta + error_test

### a
# Calculate the analytical standard errors for the polynomial specification above 
# (as presented in the lecture) and use these to calculate the approximate 
# confidence intervals as 2 SE for each value of X you consider

# beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train # calculate OLS traditional way
beta_hat <- solve(t(X_train)%*%X_train,t(X_train)%*%y_train) # calculate OLS with linear system: More stable than inverting and more efficient time- and space wise

y_test_hat <- X_test%*%beta_hat # fitted values for test data

Cov <-  se*solve(t(X_test)%*%X_test) # covariance matrix

Var <- vector() # to fill with variances of f(x)


for (i in 1:length(x_test)) {
  x_0 <- x_test[i]
  l_0 <- c(1, x_0, x_0^2, x_0^3, x_0^4) # see problem set for definition
  Var[i] <- t(l_0)%*%Cov%*%l_0
}

sd <- sqrt(Var) # calculate analytic standard errors

CI_y_hat <- cbind(y_test_hat - 2*sd, y_test_hat + 2*sd) # confidence interval fitted values with analytical standard errors

data_a <- data.frame(x_test,y_test, y_test_hat, CI_low = CI_y_hat[,1], CI_high = CI_y_hat[,2])

plot_a <- ggplot(data=data_a) + # plot of fitted values and CI
  geom_point(mapping = aes(x = x_test, y = y_test)) +
  geom_line(mapping = aes(x = x_test, y = y_test_hat), color = "red") +
  geom_ribbon(aes(x = x_test, ymin = CI_low, ymax = CI_high),fill= "red", alpha = 0.3) +
  labs(x = "Test data", y = "Fitted values / y", title ="Analytical standard errors")

plot_a # plot for whole range of x

plot_a_zoom <- plot_a + ylim(-10, 10) + xlim(-1,1) # zoomed in plot
plot_a_zoom

plot_grid(plot_a, plot_a_zoom, ncol = 2, align = 'h', axis = 'l') # both plots in one plot




### b
# Calculate the naive bootstrap confidence intervals (i.e. using the naive bootstrap quantiles)
# for B bootstrap draws from the original data, such that the nominal coverage for the two methods 
# is the same.


# randomly draw datasets with replacement from the training data, 
# each sample the same size as the original training set

B = 100 # number of bootstrap draws, not too computationally expensive

bootstrap_fitted_values <- matrix(NA, B, n) # for saving fitted values from each bootstrap draw
beta_boot <- matrix(NA, B, 5)

for (i in 1:B) {
  indices_boot <- sort(sample(1:n, size=n, replace=TRUE)) # draw indices for randomly drawing bootstrap sample with replacement
  x_boot <- x_test[indices_boot] # bootstrapped sample x_test
  y_boot <- y_test[indices_boot] # corresponding y
  X_boot <- cbind(rep(1, n), x_boot, x_boot^2, x_boot^3, x_boot^4)
#  beta_hat_boot <- solve(t(X_boot)%*%X_boot)%*%t(X_boot)%*%y_boot # OLS with inverted matrix
  beta_hat_boot <- solve(t(X_boot)%*%X_boot,t(X_boot)%*%y_boot) # OLS with linear system
  beta_boot[i,] <- beta_hat_boot

  y_hat_boot <- X_boot%*%beta_hat_boot # fitted values
  bootstrap_fitted_values[i,] <- y_hat_boot # save fitted values in one matrix row for each rep.
}

## CI betas
CI_beta <- matrix(NA, 2, 5)
for (i in 1:5) {
  CI_beta[,i] <- quantile(sort(beta_boot[,i]), probs = c(0.025, 0.975))
}

# vectors of low/high betas
beta_low <- CI_beta[1,]
beta_high <- CI_beta[2,]

y_low <- X_test %*% beta_low
y_high <- X_test %*% beta_high
CI_y_boot <- cbind(y_low, y_high)

data_b <- data.frame(x_test, y_test, y_hat_boot, CI_low = CI_y_boot[,1], CI_high = CI_y_boot[,2])

plot_b <- ggplot(data=data_b) +
  geom_point(mapping = aes(x = x_test, y = y_test)) +
  geom_line(mapping = aes(x = x_test, y = y_hat_boot), color = "red") +
  geom_ribbon(aes(x = x_test, ymin = CI_low, ymax = CI_high),fill= "red", alpha = 0.3) +
  labs(x = "Test data", y = "Fitted values / y", title ="Bootstrapped standard errors using CIs for betas") 
plot_b
  
plot_b_zoom <- plot_b + ylim(-10, 10) + xlim(-1,1) # zoomed in plot
plot_b_zoom

plot_grid(plot_b, plot_b_zoom, ncol = 2, align = 'h', axis = 'l') # both plots in one plot


#### Second try


CI_boot <- matrix(data=NA, nrow = n, ncol = 2) # empty matrix for corresponding CI of each draw

for (i in 1:n) {
  x_sort <- sort(bootstrap_fitted_values[,i]) # sort x_values descending
  CI_boot[i,] <- quantile(x_sort, probs = c(0.025, 0.975)) # 95% CI
}

cor(CI_boot, CI_y_hat)

data_b_2 <- data.frame(x_test,y_test, y_hat_boot, CI_low = CI_boot[,1], CI_high = CI_boot[,2])

plot_b_2 <- ggplot(data=data_b_2) +
  geom_point(mapping = aes(x = x_test, y = y_boot)) +
  geom_line(mapping = aes(x = x_test, y = y_hat_boot), color = "red") +
  geom_ribbon(aes(x = x_test, ymin = CI_low, ymax = CI_high),fill= "red", alpha = 0.3) +
  labs(x = "Test data", y = "Fitted values / y", title ="Bootstrapped standard errors (CIs for y hat)") 
plot_b_2

plot_b_2_zoom <- plot_b_2 + ylim(-10, 10) + xlim(-1,1) # zoomed in plot
plot_b_2_zoom

plot_grid(plot_b_2, plot_b_2_zoom, ncol = 2, align = 'h', axis = 'l') # both plots in one plot

plot_grid(plot_b, plot_b_zoom, plot_b_2, plot_b_2_zoom, ncol = 2, nrow = 2) # all four plots in one plot
# we estimated the CIs in two different ways: 
# First: calculating the CI for beta and afterwards applying to fitted values
# Second: directly calculating the CI for fitted values



##  Possible alternative: use boot function


### c
# Calculate the coverage probability at four different values of X, chosen by you

K = 100 # number of repetitions


### for a

count<- rep(0,n) # to later count how many times y is included in CI
 
for (k in 1:K) {
  # draw new data each time
  x_train <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_train <- cbind(rep(1, n), x_train, x_train^2, x_train^3, x_train^4)
  error_train <- rnorm(n, sd = se)
  y_train <- X_train%*%beta + error_train
  
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  
  
#  beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train
  beta_hat <- solve(t(X_train)%*%X_train,t(X_train)%*%y_train)
  y_test_hat <- X_test%*%beta_hat
  
  Cov <-  se*solve(t(X_test)%*%X_test) # covariance matrix
  
  Var <- vector() # for variances of f(x)
  
  
  for (i in 1:length(x_test)) {
    x_0 <- x_test[i]
    l_0 <- c(1, x_0, x_0^2, x_0^3, x_0^4) # see problem set for definition
    Var[i] <- t(l_0)%*%Cov%*%l_0
    }
  
  sd <- sqrt(Var) # calculate analytical standard errors
  
  CI_y_hat <- cbind(y_test_hat - 2*sd, y_test_hat + 2*sd)
  
  count <- count + as.integer(y_test >= CI_y_hat[,1] & y_test <= CI_y_hat[,2]) # count if y in between upper and lower bound of CI

}

cover <- count/K # calculate coverage probability
cor(y_test, y_test_hat)
mean(cover)



### for b

## using first method

count_boot <- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test

  beta_boot <- matrix(NA, B, 5)
  
  for (i in 1:B) {
    indices_boot <- sort(sample(1:n, size=n, replace=TRUE)) # draw indices for randomly drawing bootstrap sample with replacement
    x_boot <- x_test[indices_boot] # bootstrapped sample x_test
    y_boot <- y_test[indices_boot] # corresponding y
    X_boot <- cbind(rep(1, n), x_boot, x_boot^2, x_boot^3, x_boot^4)
    #  beta_hat_boot <- solve(t(X_boot)%*%X_boot)%*%t(X_boot)%*%y_boot # OLS with inverted matrix
    beta_hat_boot <- solve(t(X_boot)%*%X_boot,t(X_boot)%*%y_boot) # OLS with linear system
    beta_boot[i,] <- beta_hat_boot
    
    #y_hat_boot <- X_boot%*%beta_hat_boot # fitted values
    #bootstrap_fitted_values[i,] <- y_hat_boot # save fitted values in one matrix row for each rep.
  }
  
  ## CI betas
  CI_beta <- matrix(NA, 2, 5)
  for (i in 1:5) {
    CI_beta[,i] <- quantile(sort(beta_boot[,i]), probs = c(0.025, 0.975))
  }
  
  # vectors of low/high betas
  beta_low <- CI_beta[1,]
  beta_high <- CI_beta[2,] 
  
  y_low <- X_test %*% beta_low # calculate lower bound of CIs
  y_high <- X_test %*% beta_high # calculate upper bound of CIs
  CI_y_boot <- cbind(y_low, y_high)
  

count_boot <- count_boot + as.integer(y_test >= CI_y_boot[,1] & y_test <= CI_y_boot[,2]) # adds 1 if certain value is in the confidence interval

}

cover_boot <- count_boot/K
mean(cover_boot)



## using second method

count_boot_2 <- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  
  bootstrap_fitted_values <- matrix(NA, B, n)
  
  for (i in 1:B) {
    indices_boot <- sort(sample(1:n, size=n, replace=TRUE))
    x_boot <- x_test[indices_boot]
    y_boot <- y_test[indices_boot]
    X_boot <- cbind(rep(1, n), x_boot, x_boot^2, x_boot^3, x_boot^4)
    beta_hat_boot <- solve(t(X_boot)%*%X_boot)%*%t(X_boot)%*%y_boot
    y_hat_boot <- X_boot%*%beta_hat_boot
    bootstrap_fitted_values[i,] <- y_hat_boot
  }
  
  CI_boot_2 <- matrix(data=NA, nrow = n, ncol = 2)
  
  for (i in 1:n) {
    x_sort <- sort(bootstrap_fitted_values[,i])
    CI_boot_2[i,] <- quantile(x_sort, probs = c(0.025, 0.975))
  }
  
  count_boot_2 <- count_boot_2 + as.integer(y_test >= CI_boot_2[,1] & y_test <= CI_boot_2[,2])
  
}

cover_boot_2 <- count_boot_2/K
mean(cover_boot_2)


## compare at selected 4 values

p <- c(1, round(n/2), round(2*n/3), n) # selected 4 values
cover[p]
cover_boot[p]
cover_boot_2[p]


##### d
# Calculate the interval length at four different values of X

length <- (CI_y_hat[,2]- CI_y_hat[,1]) # for analytical standard errors
length[p]
length_boot <- (CI_y_boot[,2]- CI_y_boot[,1]) # for bootstrapped standard errors using first method
length_boot[p]
length_boot_2 <- (CI_boot_2[,2]- CI_boot_2[,1]) # for bootstrapped standard errors using second method
length_boot_2[p]





###### EXERCISE 2 ########
# Evaluate the two types of confidence intervals above along two dimensions: 
# interval length and coverage probability.

#### a
# Calculate both for a small simulation study of 100 repetitions.

# coverage probability
data_cover <- data.frame(m = 1:n, cover, cover_boot) # cover, cover_boot defined in 1c

plot_cover <- ggplot(data = data_cover) +  
  geom_line(mapping = aes(x = m, y = cover, color = "black")) +
  geom_line(mapping = aes(x = m, y = cover_boot), color = "red") +
  geom_line(mapping = aes(x = m, y = cover_boot_2), color = "darkgreen") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red", "Bootstrapped for y" = "darkgreen")) +
  labs(x = "Test observations in ascending order", y = "Coverage probability", title ="") 
plot_cover  


# length

data_length <- data.frame(m = 1:n, length, length_boot, length_boot_2) # length, length_boot, length_boot_2 defined in 1d

plot_length <- ggplot(data = data_length) +  
  geom_line(mapping = aes(x = m, y = length, color = "black")) +
  geom_line(mapping = aes(x = m, y = length_boot), color = "red") +
  geom_line(mapping = aes(x = m, y = length_boot_2), color = "darkgreen") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red", "Bootstrapped for y" = "darkgreen")) +
  labs(x = "Test observations in ascending order", y = "Interval length CI", title ="")
plot_length

plot_length_zoom <- plot_length + xlim(50, 150) + ylim(0,50)
plot_length_zoom

plot_grid(plot_length, plot_length_zoom, ncol = 2, align = 'h', axis = 'l') # both plots in one plot


### b

# smaller sample size
# with a smaller sample size bootstrap will have a competitive advantage because Central Limit Theorem for
# asymptotic normal distribution may not be applicable


n <- 500

K = 100 # number of repetitions

count<- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  # draw new data each time
  x_train <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_train <- cbind(rep(1, n), x_train, x_train^2, x_train^3, x_train^4)
  error_train <- rnorm(n, sd = se)
  y_train <- X_train%*%beta + error_train
  
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  
  
  #  beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train
  beta_hat <- solve(t(X_train)%*%X_train,t(X_train)%*%y_train)
  y_test_hat <- X_test%*%beta_hat
  
  Cov <-  se*solve(t(X_test)%*%X_test) # covariance matrix
  
  Var <- vector() # for variances of f(x)
  
  
  for (i in 1:length(x_test)) {
    x_0 <- x_test[i]
    l_0 <- c(1, x_0, x_0^2, x_0^3, x_0^4) # see problem set for definition
    Var[i] <- t(l_0)%*%Cov%*%l_0
  }
  
  sd <- sqrt(Var) # calculate analytical standard errors
  
  CI_y_hat <- cbind(y_test_hat - 2*sd, y_test_hat + 2*sd)
  
  count <- count + as.integer(y_test >= CI_y_hat[,1] & y_test <= CI_y_hat[,2]) # count if y in between upper and lower bound of CI
  
}

cover <- count/K # calculate coverage probability
cor(y_test, y_test_hat)
mean(cover)

##bootstrap

count_boot <- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  
  beta_boot <- matrix(NA, B, 5)
  
  for (i in 1:B) {
    indices_boot <- sort(sample(1:n, size=n, replace=TRUE)) # draw indices for randomly drawing bootstrap sample with replacement
    x_boot <- x_test[indices_boot] # bootstrapped sample x_test
    y_boot <- y_test[indices_boot] # corresponding y
    X_boot <- cbind(rep(1, n), x_boot, x_boot^2, x_boot^3, x_boot^4)
    #  beta_hat_boot <- solve(t(X_boot)%*%X_boot)%*%t(X_boot)%*%y_boot # OLS with inverted matrix
    beta_hat_boot <- solve(t(X_boot)%*%X_boot,t(X_boot)%*%y_boot) # OLS with linear system
    beta_boot[i,] <- beta_hat_boot
    
    #y_hat_boot <- X_boot%*%beta_hat_boot # fitted values
    #bootstrap_fitted_values[i,] <- y_hat_boot # save fitted values in one matrix row for each rep.
  }
  
  ## CI betas
  CI_beta <- matrix(NA, 2, 5)
  for (i in 1:5) {
    CI_beta[,i] <- quantile(sort(beta_boot[,i]), probs = c(0.025, 0.975))
  }
  
  # vectors of low/high betas
  beta_low <- CI_beta[1,]
  beta_high <- CI_beta[2,] 
  
  y_low <- X_test %*% beta_low # calculate lower bound of CIs
  y_high <- X_test %*% beta_high # calculate upper bound of CIs
  CI_y_boot <- cbind(y_low, y_high)
  
  
  count_boot <- count_boot + as.integer(y_test >= CI_y_boot[,1] & y_test <= CI_y_boot[,2]) # adds 1 if certain value is in the confidence interval
  
}

cover_boot <- count_boot/K
mean(cover_boot)


data_cover <- data.frame(m = 1:n, cover, cover_boot) # cover, cover_boot defined in 1c

plot_cover <- ggplot(data = data_cover) +  
  geom_line(mapping = aes(x = m, y = cover, color = "black")) +
  geom_line(mapping = aes(x = m, y = cover_boot), color = "red") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red")) +
  labs(x = "Test observations in ascending order", y = "Coverage probability", title ="") +
  ggtitle("n = 500") + theme(plot.title = element_text(size = 12))




# length
length <- (CI_y_hat[,2]- CI_y_hat[,1]) # for analytical standard errors
length_boot <- (CI_y_boot[,2]- CI_y_boot[,1]) # for bootstrapped standard errors using first method


data_length <- data.frame(m = 1:n, length, length_boot) # length, length_boot, length_boot_2 defined in 1d

plot_length <- ggplot(data = data_length) +  
  geom_line(mapping = aes(x = m, y = length, color = "black")) +
  geom_line(mapping = aes(x = m, y = length_boot), color = "red") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red")) +
  labs(x = "Test observations in ascending order", y = "Interval length CI", title ="") +
  ggtitle("Simulation study n") + theme(plot.title = element_text(size = 15, face = "bold"))
#plot_length





######smaller sample size

n <- 50

K = 100 # number of repetitions

count<- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  # draw new data each time
  x_train <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_train <- cbind(rep(1, n), x_train, x_train^2, x_train^3, x_train^4)
  error_train <- rnorm(n, sd = se)
  y_train <- X_train%*%beta + error_train
  
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  beta_hat <- solve(t(X_train)%*%X_train,t(X_train)%*%y_train)
  y_test_hat <- X_test%*%beta_hat
  Cov <-  solve(t(X_test)%*%X_test) # covariance matrix
  Var <- vector() # for variances of f(x)

  for (i in 1:length(x_test)) {
    x_0 <- x_test[i]
    l_0 <- c(1, x_0, x_0^2, x_0^3, x_0^4) # see problem set for definition
    Var[i] <- t(l_0)%*%Cov%*%l_0
  }
  sd <- sqrt(Var) # calculate analytical standard errors
  
  CI_y_hat <- cbind(y_test_hat - 2*sd, y_test_hat + 2*sd)
  
  count <- count + as.integer(y_test >= CI_y_hat[,1] & y_test <= CI_y_hat[,2]) # count if y in between upper and lower bound of CI
  
}

cover <- count/K # calculate coverage probability
cor(y_test, y_test_hat)
mean(cover)


## bootstrap

count_boot <- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  
  beta_boot <- matrix(NA, B, 5)
  
  for (i in 1:B) {
    indices_boot <- sort(sample(1:n, size=n, replace=TRUE)) # draw indices for randomly drawing bootstrap sample with replacement
    x_boot <- x_test[indices_boot] # bootstrapped sample x_test
    y_boot <- y_test[indices_boot] # corresponding y
    X_boot <- cbind(rep(1, n), x_boot, x_boot^2, x_boot^3, x_boot^4)
    #  beta_hat_boot <- solve(t(X_boot)%*%X_boot)%*%t(X_boot)%*%y_boot # OLS with inverted matrix
    beta_hat_boot <- solve(t(X_boot)%*%X_boot,t(X_boot)%*%y_boot) # OLS with linear system
    beta_boot[i,] <- beta_hat_boot
  }
  
  ## CI betas
  CI_beta <- matrix(NA, 2, 5)
  for (i in 1:5) {
    CI_beta[,i] <- quantile(sort(beta_boot[,i]), probs = c(0.025, 0.975))
  }
  # vectors of low/high betas
  beta_low <- CI_beta[1,]
  beta_high <- CI_beta[2,] 
  y_low <- X_test %*% beta_low # calculate lower bound of CIs
  y_high <- X_test %*% beta_high # calculate upper bound of CIs
  CI_y_boot <- cbind(y_low, y_high)
  count_boot <- count_boot + as.integer(y_test >= CI_y_boot[,1] & y_test <= CI_y_boot[,2]) # adds 1 if certain value is in the confidence interval
}

cover_boot <- count_boot/K
mean(cover_boot)

data_cover <- data.frame(m = 1:n, cover, cover_boot) # cover, cover_boot defined in 1c

plot_cover_n <- ggplot(data = data_cover) +  
  geom_line(mapping = aes(x = m, y = cover, color = "black")) +
  geom_line(mapping = aes(x = m, y = cover_boot), color = "red") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red")) +
  labs(x = "Test observations in ascending order", y = "Coverage probability", title ="") +
  ggtitle("n = 50") + theme(plot.title = element_text(size = 12))
#plot_cover_n 


# length

length <- (CI_y_hat[,2]- CI_y_hat[,1]) # for analytical standard errors
length_boot <- (CI_y_boot[,2]- CI_y_boot[,1]) # for bootstrapped standard errors using first method


data_length <- data.frame(m = 1:n, length, length_boot) # length, length_boot, length_boot_2 defined in 1d

plot_length_n <- ggplot(data = data_length) +  
  geom_line(mapping = aes(x = m, y = length, color = "black")) +
  geom_line(mapping = aes(x = m, y = length_boot), color = "red") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red")) +
  labs(x = "Test observations in ascending order", y = "Interval length CI", title ="")
#plot_length_n


plot_n <- plot_grid(plot_length, plot_length_n, plot_cover, plot_cover_n, ncol = 2, align = 'h', axis = 'l') # both plots in one plot
plot_n



#### when sigma^2 is significantly smaller than the standard deviation of the error term


n <- 500
sigma_2 <- 1

K = 100 # number of repetitions

count<- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  # draw new data each time
  x_train <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_train <- cbind(rep(1, n), x_train, x_train^2, x_train^3, x_train^4)
  error_train <- rnorm(n, sd = se)
  y_train <- X_train%*%beta + error_train
  
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  
  
  #  beta_hat <- solve(t(X_train)%*%X_train)%*%t(X_train)%*%y_train
  beta_hat <- solve(t(X_train)%*%X_train,t(X_train)%*%y_train)
  y_test_hat <- X_test%*%beta_hat
  
  Cov <-  se*solve(t(X_test)%*%X_test) # covariance matrix
  
  Var <- vector() # for variances of f(x)
  
  
  for (i in 1:length(x_test)) {
    x_0 <- x_test[i]
    l_0 <- c(1, x_0, x_0^2, x_0^3, x_0^4) # see problem set for definition
    Var[i] <- t(l_0)%*%Cov%*%l_0
  }
  
  sd <- sqrt(Var) # calculate analytical standard errors
  
  CI_y_hat <- cbind(y_test_hat - 2*sd, y_test_hat + 2*sd)
  
  count <- count + as.integer(y_test >= CI_y_hat[,1] & y_test <= CI_y_hat[,2]) # count if y in between upper and lower bound of CI
  
}

cover <- count/K # calculate coverage probability
cor(y_test, y_test_hat)
mean(cover)

##bootstrap

count_boot <- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  
  beta_boot <- matrix(NA, B, 5)
  
  for (i in 1:B) {
    indices_boot <- sort(sample(1:n, size=n, replace=TRUE)) # draw indices for randomly drawing bootstrap sample with replacement
    x_boot <- x_test[indices_boot] # bootstrapped sample x_test
    y_boot <- y_test[indices_boot] # corresponding y
    X_boot <- cbind(rep(1, n), x_boot, x_boot^2, x_boot^3, x_boot^4)
    #  beta_hat_boot <- solve(t(X_boot)%*%X_boot)%*%t(X_boot)%*%y_boot # OLS with inverted matrix
    beta_hat_boot <- solve(t(X_boot)%*%X_boot,t(X_boot)%*%y_boot) # OLS with linear system
    beta_boot[i,] <- beta_hat_boot
    
    #y_hat_boot <- X_boot%*%beta_hat_boot # fitted values
    #bootstrap_fitted_values[i,] <- y_hat_boot # save fitted values in one matrix row for each rep.
  }
  
  ## CI betas
  CI_beta <- matrix(NA, 2, 5)
  for (i in 1:5) {
    CI_beta[,i] <- quantile(sort(beta_boot[,i]), probs = c(0.025, 0.975))
  }
  
  # vectors of low/high betas
  beta_low <- CI_beta[1,]
  beta_high <- CI_beta[2,] 
  
  y_low <- X_test %*% beta_low # calculate lower bound of CIs
  y_high <- X_test %*% beta_high # calculate upper bound of CIs
  CI_y_boot <- cbind(y_low, y_high)
  
  
  count_boot <- count_boot + as.integer(y_test >= CI_y_boot[,1] & y_test <= CI_y_boot[,2]) # adds 1 if certain value is in the confidence interval
  
}

cover_boot <- count_boot/K
mean(cover_boot)


data_cover <- data.frame(m = 1:n, cover, cover_boot) # cover, cover_boot defined in 1c

plot_cover <- ggplot(data = data_cover) +  
  geom_line(mapping = aes(x = m, y = cover, color = "black")) +
  geom_line(mapping = aes(x = m, y = cover_boot), color = "red") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red")) +
  labs(x = "Test observations in ascending order", y = "Coverage probability", title ="") +
  ggtitle("sigma^2 = 1") + theme(plot.title = element_text(size = 12, face = "bold"))




# length
length <- (CI_y_hat[,2]- CI_y_hat[,1]) # for analytical standard errors
length_boot <- (CI_y_boot[,2]- CI_y_boot[,1]) # for bootstrapped standard errors using first method


data_length <- data.frame(m = 1:n, length, length_boot) # length, length_boot, length_boot_2 defined in 1d

plot_length <- ggplot(data = data_length) +  
  geom_line(mapping = aes(x = m, y = length, color = "black")) +
  geom_line(mapping = aes(x = m, y = length_boot), color = "red") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red")) +
  labs(x = "Test observations in ascending order", y = "Interval length CI", title ="") +
  ggtitle("Simulation study sigma^2") + theme(plot.title = element_text(size = 15, face = "bold"))
#plot_length



######smaller sigma^2

sigma_2 <- 0.05

count<- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  # draw new data each time
  x_train <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_train <- cbind(rep(1, n), x_train, x_train^2, x_train^3, x_train^4)
  error_train <- rnorm(n, sd = se)
  y_train <- X_train%*%beta + error_train
  
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  beta_hat <- solve(t(X_train)%*%X_train,t(X_train)%*%y_train)
  y_test_hat <- X_test%*%beta_hat
  Cov <-  se*solve(t(X_test)%*%X_test) # covariance matrix
  Var <- vector() # for variances of f(x)
  
  for (i in 1:length(x_test)) {
    x_0 <- x_test[i]
    l_0 <- c(1, x_0, x_0^2, x_0^3, x_0^4) # see problem set for definition
    Var[i] <- t(l_0)%*%Cov%*%l_0
  }
  sd <- sqrt(Var) # calculate analytical standard errors
  
  CI_y_hat <- cbind(y_test_hat - 2*sd, y_test_hat + 2*sd)
  
  count <- count + as.integer(y_test >= CI_y_hat[,1] & y_test <= CI_y_hat[,2]) # count if y in between upper and lower bound of CI
  
}

cover <- count/K # calculate coverage probability
cor(y_test, y_test_hat)
mean(cover)


## bootstrap

count_boot <- rep(0,n) # to later count how many times y is included in CI

for (k in 1:K) {
  x_test <- sort(rnorm(n, mean=0, sd= sqrt(sigma_2)))
  X_test <- cbind(rep(1, n), x_test, x_test^2, x_test^3, x_test^4)
  error_test <- rnorm(n, sd = se)
  y_test <- X_test%*%beta + error_test
  
  beta_boot <- matrix(NA, B, 5)
  
  for (i in 1:B) {
    indices_boot <- sort(sample(1:n, size=n, replace=TRUE)) # draw indices for randomly drawing bootstrap sample with replacement
    x_boot <- x_test[indices_boot] # bootstrapped sample x_test
    y_boot <- y_test[indices_boot] # corresponding y
    X_boot <- cbind(rep(1, n), x_boot, x_boot^2, x_boot^3, x_boot^4)
    #  beta_hat_boot <- solve(t(X_boot)%*%X_boot)%*%t(X_boot)%*%y_boot # OLS with inverted matrix
    beta_hat_boot <- solve(t(X_boot)%*%X_boot,t(X_boot)%*%y_boot) # OLS with linear system
    beta_boot[i,] <- beta_hat_boot
  }
  
  ## CI betas
  CI_beta <- matrix(NA, 2, 5)
  for (i in 1:5) {
    CI_beta[,i] <- quantile(sort(beta_boot[,i]), probs = c(0.025, 0.975))
  }
  # vectors of low/high betas
  beta_low <- CI_beta[1,]
  beta_high <- CI_beta[2,] 
  y_low <- X_test %*% beta_low # calculate lower bound of CIs
  y_high <- X_test %*% beta_high # calculate upper bound of CIs
  CI_y_boot <- cbind(y_low, y_high)
  count_boot <- count_boot + as.integer(y_test >= CI_y_boot[,1] & y_test <= CI_y_boot[,2]) # adds 1 if certain value is in the confidence interval
}

cover_boot <- count_boot/K
mean(cover_boot)

data_cover <- data.frame(m = 1:n, cover, cover_boot) # cover, cover_boot defined in 1c

plot_cover_s <- ggplot(data = data_cover) +  
  geom_line(mapping = aes(x = m, y = cover, color = "black")) +
  geom_line(mapping = aes(x = m, y = cover_boot), color = "red") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red")) +
  labs(x = "Test observations in ascending order", y = "Coverage probability", title ="") +
  ggtitle("sigma^2 = 0.05") + theme(plot.title = element_text(size = 12, face = "bold"))
#plot_cover_s 


# length

length <- (CI_y_hat[,2]- CI_y_hat[,1]) # for analytical standard errors
length_boot <- (CI_y_boot[,2]- CI_y_boot[,1]) # for bootstrapped standard errors using first method


data_length <- data.frame(m = 1:n, length, length_boot) # length, length_boot, length_boot_2 defined in 1d

plot_length_s <- ggplot(data = data_length) +  
  geom_line(mapping = aes(x = m, y = length, color = "black")) +
  geom_line(mapping = aes(x = m, y = length_boot), color = "red") +
  scale_color_manual(name = "", values = c("Analytical" = "black", "Bootstrapped for betas" = "red")) +
  labs(x = "Test observations in ascending order", y = "Interval length CI", title ="")
#plot_length_s


plot_sigma <- plot_grid(plot_length, plot_length_s, plot_cover, plot_cover_s, ncol = 2, align = 'h', axis = 'l') # all plots in one plot
plot_sigma





