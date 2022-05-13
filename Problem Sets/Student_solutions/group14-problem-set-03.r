# Packages
packages <- c(
    "MASS",      # Provides mvrnorm and lda
    "tidyverse", # Collection of packages for data science
    "ggpubr",    # Combined plots
    "cvms"       # Confusion matrix
)

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

# Seed
set.seed(123)

# Others
theme_set(theme_light())
theme_update(plot.title = element_text(hjust = 0.5))

#1.a)
n1 <- 300
n2 <- 500
n <- n1 + n2

mu1 <- c(-3, 3)
mu2 <- c(5, 5)
sigma <- matrix(c(16, -2, -2, 9), 2)

sim_fun <- function(n1, n2, mu1, mu2, sigma1, sigma2) {
    
    class <- factor(c(rep(1, n1), rep(2, n2)))
    
    X1 <- mvrnorm(n1, mu1, sigma1)
    X2 <- mvrnorm(n2, mu2, sigma2)
    
    df <- data.frame(class, rbind(X1, X2))
    
}

train_data <- sim_fun(n1, n2, mu1, mu2, sigma, sigma)

options(repr.plot.res = 600, repr.plot.height = 8, repr.plot.width = 12) # Adjusting plotting options

p1 <- ggplot(train_data, aes(X1, X2, color = class)) +
          geom_point() +
          scale_colour_discrete("Class", c("1", "2"))
p1

#1.b)
# Linear Discriminant Analysis
fit_train_lda <- lda(class ~ X1 + X2, data = train_data)
fit_train_lda

pred_train_lda <- predict(fit_train_lda, newdata = train_data)

head(pred_train_lda$class, 10)

head(pred_train_lda$posterior, 10)

head(pred_train_lda$x, 10)

p2 <- ggplot(train_data, aes(X1, X2, color = pred_train_lda$class)) +
          geom_point() +
          scale_colour_discrete("Class", c("1", "2")) +
          ggtitle("LDA Prediction")

p1 <- p1 + ggtitle("Original data")

ggarrange(p2, p1, common.legend = TRUE, legend = "bottom")

#Logistic Regression
fit_train_logit <- glm(class ~ X1 + X2, data = train_data, family = "binomial")
fit_train_logit

pred_train_logit <- predict(fit_train_logit, newdata = train_data, type = "response")
head(pred_train_logit, 10)

pred_train_logit_class <- factor(ifelse(pred_train_logit > 0.5, 2, 1)) # Threshold of 0.5
head(pred_train_logit_class, 10)

p3 <- ggplot(train_data, aes(X1, X2, color = pred_train_logit_class)) +
          geom_point() +
          scale_color_discrete("Class", c("1", "2")) +
          ggtitle("Logit Prediction")

ggarrange(p3, p2, common.legend = TRUE, legend = "bottom")

#1.c)
APE <- function(actual, predicted) {
    
    mean(actual != predicted)
    
}

APE_train_lda <- APE(train_data$class, pred_train_lda$class)
APE_train_logit <- APE(train_data$class, pred_train_logit_class)
paste("Mean training error of LDA:", APE_train_lda)
paste("Mean training error of logistic regression:", APE_train_logit)

test_data <- sim_fun(n1, n2, mu1, mu2, sigma, sigma)

pred_test_lda <- predict(fit_train_lda, newdata = test_data)

pred_test_logit <- predict(fit_train_logit, newdata = test_data, type = "response")
pred_test_logit_class <- factor(ifelse(pred_test_logit > 0.5, 2, 1))

APE_test_lda <- APE(test_data$class, pred_test_lda$class)
APE_test_logit <- APE(test_data$class, pred_test_logit_class)
paste("Mean test error of LDA:", APE_test_lda)
paste("Mean test error of logistic regression:", APE_test_logit)

#1.d)
confusion_matrix_train_lda <- table(actual = train_data$class, predicted = pred_train_lda$class)
confusion_matrix_train_logit <- table(actual = train_data$class, predicted = pred_train_logit_class)

options(repr.plot.res = 250, repr.plot.height = 5, repr.plot.width = 5) # Adjusting plotting options

suppressWarnings(
plot_confusion_matrix(as_tibble(confusion_matrix_train_lda), 
                      target_col = "actual", 
                      prediction_col = "predicted",
                      counts_col = "n") +
    ggtitle("Confusion matrix LDA")
)

suppressWarnings(
plot_confusion_matrix(as_tibble(confusion_matrix_train_logit), 
                      target_col = "actual", 
                      prediction_col = "predicted",
                      counts_col = "n") +
    ggtitle("Confusion matrix Logit")
)

#2.a)
rep <- 100

APE_train_lda <- rep(NA, rep)
APE_train_logit <- rep(NA, rep)
APE_test_lda <- rep(NA, rep)
APE_test_logit <- rep(NA, rep)

for (i in 1:rep) {
    
    train_data <- sim_fun(n1, n2, mu1, mu2, sigma, sigma)
    test_data <- sim_fun(n1, n2, mu1, mu2, sigma, sigma)
    
    
    fit_train_lda <- lda(class ~ X1 + X2, data = train_data)
    pred_train_lda <- predict(fit_train_lda, newdata = train_data)
    
    fit_train_logit <- glm(class ~ X1 + X2, data = train_data, family = "binomial")
    pred_train_logit <- predict(fit_train_logit, newdata = train_data, type = "response")
    pred_train_logit_class <- factor(ifelse(pred_train_logit > 0.5, 2, 1))
    
    
    APE_train_lda[i] <- APE(train_data$class, pred_train_lda$class)
    APE_train_logit[i] <- APE(train_data$class, pred_train_logit_class)
    
    
    pred_test_lda <- predict(fit_train_lda, newdata = test_data)
    
    pred_test_logit <- predict(fit_train_logit, newdata = test_data, type = "response")
    pred_test_logit_class <- factor(ifelse(pred_test_logit > 0.5, 2, 1))
    
    
    APE_test_lda[i] <- APE(test_data$class, pred_test_lda$class)
    APE_test_logit[i] <- APE(test_data$class, pred_test_logit_class)
    
}

df_sim <- data.frame(APE = c(APE_train_lda, APE_train_logit, APE_test_lda, APE_test_logit),
                     data = c(rep("train", 2*rep), rep("test", 2*rep)),
                     model = c(rep("lda", rep), rep("logit", rep)))

options(repr.plot.res = 250, repr.plot.height = 5, repr.plot.width = 7) # Adjusting plotting options

ggplot(df_sim, aes(model, APE, color = model)) +
    geom_boxplot(show.legend = FALSE) +
    labs(x = "", y = "APE") +
    facet_wrap(vars(data))

#2.b)
# Attempt to worsen the relative performance of LDA by allowing the covariance matrices to differ

sigma_adj_factor <- seq(1, 10, by = 0.2)

APE_test_lda <- c()
APE_test_logit <- c()

mean_APE_test_lda <- c()
mean_APE_test_logit <- c()

for (i in 1:length(sigma_adj_factor)) {
    
    sigma2 <- sigma * sigma_adj_factor[i]
    
        for (j in 1:100) {
            
            train_data <- sim_fun(n1, n2, mu1, mu2, sigma, sigma2)
            test_data <- sim_fun(n1, n2, mu1, mu2, sigma, sigma2)
            
            fit_train_lda <- lda(class ~ X1 + X2, data = train_data)
            fit_train_logit <- glm(class ~ X1 + X2, data = train_data, family = "binomial")
            
            pred_test_lda <- predict(fit_train_lda, newdata = test_data)
            pred_test_logit <- predict(fit_train_logit, newdata = test_data, type = "response")
            pred_test_logit_class <- factor(ifelse(pred_test_logit > 0.5, 2, 1))
            
            APE_test_lda[j] <- APE(test_data$class, pred_test_lda$class)
            APE_test_logit[j] <- APE(test_data$class, pred_test_logit_class)
            
        }
    
    mean_APE_test_lda[i] <- mean(APE_test_lda)
    mean_APE_test_logit[i] <- mean(APE_test_logit)
    
}

sigma_adj_factor 

test_error1 <- data.frame("size" =sigma_adj_factor, "LDA"= mean_APE_test_lda, "LR" = mean_APE_test_logit)

ggplot(test_error1, aes(x=sigma_adj_factor)) +
    geom_point(aes(y=mean_APE_test_lda, color="mean_APE_test_lda_2")) +
    geom_point(aes(y=mean_APE_test_logit, color="mean_APE_test_logit_2")) +
    theme_classic()+
    labs(subtitle="LDA/LR performance for differ covariance matrix") +
    xlab("sigma_adj_factor") +
    ylab("Average Prediction error")

mean_APE_test_lda
mean_APE_test_logit



# Attempt to worsen the relative performance of LDA by allowing the sample size to differ
sample_size_n1 <- seq(50,300,by =50 )
sample_size_n2 <- seq(750,500,by =-50 )
sample_size_n1
sample_size_n2

APE_test_lda_2 <- c()
APE_test_logit_2 <- c()

mean_APE_test_lda_2 <- c()
mean_APE_test_logit_2 <- c()

for (i in 1:length(sample_size_n1)) {
    
    sim_samp_n1 <- sample_size_n1[i]
    sim_samp_n2 <- sample_size_n2[i]
    
        for (j in 1:100) {
            
            train_data <- sim_fun(sim_samp_n1, sim_samp_n2, mu1, mu2, sigma, sigma)
            test_data <- sim_fun(sim_samp_n1, sim_samp_n2, mu1, mu2, sigma, sigma)
            
            fit_train_lda <- lda(class ~ X1 + X2, data = train_data)
            fit_train_logit <- glm(class ~ X1 + X2, data = train_data, family = "binomial")
            
            pred_test_lda <- predict(fit_train_lda, newdata = test_data)
            pred_test_logit <- predict(fit_train_logit, newdata = test_data, type = "response")
            pred_test_logit_class <- factor(ifelse(pred_test_logit > 0.5, 2, 1))
            
            APE_test_lda_2[j] <- APE(test_data$class, pred_test_lda$class)
            APE_test_logit_2[j] <- APE(test_data$class, pred_test_logit_class)
            
        }
    
    mean_APE_test_lda_2[i] <- mean(APE_test_lda_2)
    mean_APE_test_logit_2[i] <- mean(APE_test_logit_2)
    
}

mean_APE_test_lda_2
mean_APE_test_logit_2
sample_size <- cbind(sample_size_n1,sample_size_n2)


sample_size <- 1:6
# cbind(sample_size_n1,sample_size_n2)
test_error <- data.frame("size" =sample_size, "LDA"= mean_APE_test_lda_2, "LR" = mean_APE_test_logit_2)

ggplot(test_error, aes(x=sample_size)) +
    geom_point(aes(y=mean_APE_test_lda_2, color="mean_APE_test_lda_2")) +
    geom_point(aes(y=mean_APE_test_logit_2, color="mean_APE_test_logit_2")) +
    theme_classic()+
    labs(subtitle="LDA/LR performance for differ sample size") +
    xlab("Sample Size") +
    ylab("Average Prediction error")

mean_APE_test_lda_2
mean_APE_test_logit_2

#2.c)
sensitivity_2 <- c()
specificity_2 <- c()
total_error_2 <- c()

threshold_2 <- seq(from=0.1, to= 0.9, by=0.1)
for (i in 1:length(threshold_2)) {
  
  pred_train_logit_class_2 <- factor(ifelse(pred_test_logit >= threshold_2[i], 2, 1))
  confusion_matrix_train_logit_2 <- table(actual = train_data$class, predicted = pred_train_logit_class_2)
  
  sensitivity_2[i] <- confusion_matrix_train_logit_2[2,2]/ sum(confusion_matrix_train_logit_2[2,])
  specificity_2[i] <- confusion_matrix_train_logit_2[1,1]/ sum(confusion_matrix_train_logit_2[1,])
  total_error_2[i] <- (confusion_matrix_train_logit_2[1,2]+confusion_matrix_train_logit_2[2,1])/(sum(confusion_matrix_train_logit_2[1,])+sum(confusion_matrix_train_logit_2[2,]))
}


# cbind(sample_size_n1,sample_size_n2)
DATA <- data.frame("threshold" =threshold_2, "sensitivity"= sensitivity_2, "specificity" = specificity_2, "total_error" = total_error_2)

ggplot(DATA, aes(x=threshold_2)) +
    geom_point(aes(y=sensitivity_2, color="sensitivity_2")) +
    geom_point(aes(y=specificity_2, color="specificity_2")) +
    geom_point(aes(y=total_error_2, color="total_error_2")) +
    theme_classic()+
    labs(subtitle="Error Rate") +
    xlab("Threshold") +
    ylab("Error Rate")

sensitivity_2
specificity_2
total_error_2
