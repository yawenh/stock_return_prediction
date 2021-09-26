## Data Analysis Part: 
## 
## This part includes the regression models we fitted on the data set we 
## generated in the data preparation part. 
##
## Author: Yawen Hu 
## Updated: November 17, 2020 

# 79: -------------------------------------------------------------------------

# directories: ----------------------------------------------------------------
setwd("~/Desktop/urop")

# libraries: ------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(FNN)
library(glmnet)

# data: -----------------------------------------------------------------------
foo = load("./data/price_data.RData") 

# Regression Models : ---------------------------------------------------------

## 1.Linear regression: -------------------------------------------------------

### (1) Predictors: 3-min, 10-min, 30-min backward return of Asset 1, 2, 3.
fret_lm = fret %>%
  dplyr::select( Asset_1_FRet_10, ends_with(c("_3", "_10", "_30")) )

### (2) Set training data (70%) and testing data (30%).
train_size = nrow(fret_lm)*0.7
train_id = 1:train_size

train_lm <- fret_lm[train_id, ]
test_lm <- fret_lm[-train_id, ]

### (3) Fit Linear Model - lmod. 
lmod = lm(Asset_1_FRet_10 ~., train_lm)
summary(lmod)

#### (4) In-sample and Out-of-sample correlation. 

##### in-sample correlation:
y_true_train_lm = train_lm$Asset_1_FRet_10
y_pred_train_lm = predict(lmod, train_lm)
cor_in_sample_lm = cor(y_true_train_lm, y_pred_train_lm) 

##### out-of-sample correlation:
y_true_test_lm = test_lm$Asset_1_FRet_10
y_pred_test_lm = predict(lmod, test_lm)
cor_out_sample_lm = cor(y_true_test_lm, y_pred_test_lm) 


#### (5) Plot of rolling correlation between y_pred and y_true:
lm_plot = fret_lm %>%
  mutate( Asset_1_FRet_10_pred = predict(lmod, fret_lm) ) %>%
  transmute( y_true = Asset_1_FRet_10, y_pred = Asset_1_FRet_10_pred )

Rho = c()
for(i in 1:524160){
  if(i <= 30240){ 
    Rho[i] = cor(lm_plot[c(1:i), 1], lm_plot[c(1:i), 2]) 
  }
  else{ 
   Rho[i] = cor(lm_plot[c((i-30240):i), 1], lm_plot[c((i-30240):i), 2]) 
  }
}

lm_plot = lm_plot %>% mutate( time = c(1:524160), Rho = Rho )
# write_csv(lm_plot, "liner_model.csv")
#lm_plot = read_csv("./data/liner_model.csv")

lm_corr_plot = lm_plot %>%
  filter(!is.na(Rho)) %>%
  ggplot2::ggplot( aes(x = time, y = Rho) ) + 
  ggplot2::geom_point( colour = 'skyblue4', size = 0.5 ) +
  xlab("time (min)") +
  ylab("correlation") +
  ggtitle("Three-week backward rolling correlation between y_true and y_pred") +
  theme(plot.title = element_text(hjust = 0.5))
print(lm_corr_plot)

# ggsave("corr_lm.png")

## 2. KNN: --------------------------------------------------------------------

### (1) Predictors: 3-min, 10-min, 30-min backward return of Asset 1, 2, 3.
fret_knn = fret %>%
  dplyr::select( Asset_1_FRet_10, ends_with(c("_3", "_10", "_30")) )

### (2) Set training data (70%) and testing data (30%).
train_size <- nrow(fret_knn)*0.7
train_id <- 1:train_size

train_KNN <- fret_knn[train_id, ]
test_KNN <- fret_knn[-train_id, ]

### (3) Find best K value by testing MSE. 
#### Train:
k_range = c(5, 25, 125, 625)
trainMSE_KNN = c()
train_x = train_KNN %>% dplyr::select(-Asset_1_FRet_10)

for(i in 1:length(k_range)) {
  knnTrain <-knn.reg(train = train_x, 
                     y = train_KNN$Asset_1_FRet_10,
                     test = train_x, 
                     k = k_range[i])
  
  trainMSE_KNN[i] = mean((train_KNN$Asset_1_FRet_10 - knnTrain$pred)^2)
}

#### Test:
testMSE_KNN = c()
test_x = test_KNN %>% dplyr::select(-Asset_1_FRet_10)

for(i in 1:length(k_range)) {
  knnTest <-knn.reg(train = train_x, 
                    y = train_KNN$Asset_1_FRet_10,
                    test = test_x, 
                    k = k_range[i])
  
  testMSE_KNN[i] <-mean((test_KNN$Asset_1_FRet_10 - knnTest$pred)^2)
}

#### Plot training and testing MSE:
plot(trainMSE_KNN ~ k_range, type = "b", lwd = 2, col = "blue",
     main = "Training and Test MSE for KNN", xlab = "K", ylab = "MSE")
lines(testMSE_KNN ~ k_range, type = "b", lwd = 2, col = "red")

### Find Best k:
best_k = k_range[which.min(testMSE_KNN)]
X = fret_knn %>% dplyr::select(-Asset_1_FRet_10)
knn_best_k <-knn.reg(train = train_x, 
                     y = train_KNN$Asset_1_FRet_10,
                     test = X, 
                     k = best_k)
KNN_Pred_bestK = knn_best_k$pred

### (4) Correlation:
#### in-sample correlation
y_true_train_KNN = train_KNN$Asset_1_FRet_10
y_pred_train_KNN = KNN_Pred_bestK[1:nrow(train_KNN)]
cor_in_sample_KNN = cor(y_true_train_KNN, y_pred_train_KNN)

#### out-of-sample correlation
y_true_test_KNN = test_KNN$Asset_1_FRet_10
y_pred_test_KNN = KNN_Pred_bestK[(nrow(train_KNN)+1):length(KNN_Pred_bestK)]
cor_out_sample_KNN = cor(y_true_test_KNN, y_pred_test_KNN)


## 3. Ridge and LASSO: --------------------------------------------------------

### (1) Predictors: h-min backward return of Asset 1, 2, 3.
###                 h = {3,10,30,60,120,180,240,360,480,600,720,960,1200,1440}. 

### (2) Set training data (70%) and testing data (30%).
train_size <- nrow(fret)*0.7
train_id <- 1:train_size

train_RL <- fret[train_id, ]
test_RL <- fret[-train_id, ]

X = model.matrix(Asset_1_FRet_10 ~., fret)[, -1]
y = fret$Asset_1_FRet_10

### (3) Ridge model - ridge_mod. 
#### Model Fitting: 

ridge_mod = glmnet(X[train_id,], y[train_id], alpha = 0)

#### Best lambda through validation MSE
n = length(ridge_mod$lambda)

MSE_ridge = c()
for (i in 1:n) {
  ridge_pred_test = predict(ridge_mod, s = ridge_mod$lambda[i], newx = X[-train_id,])
  MSE_ridge[i] =  mean((ridge_pred_test - y[-train_id])^2)
}

ridge_lambda = data.frame( lambda = ridge_mod$lambda, MSE = MSE_ridge )

##### Plot the lambda with corresponding MSE
ridge_lambda_plot = ridge_lambda %>% 
  ggplot2::ggplot( aes(x = lambda, y = MSE) ) +
  ggplot2::geom_point( colour = 'orchid', size = 1) + 
  xlab("lambda") + 
  ylab("Validation MSE") + 
  ggtitle("Ridge lambda with corresponding validation MSE") +
  theme(plot.title = element_text(hjust = 0.5))

best_ridge_lambda = ridge_mod$lambda[which.min(MSE_ridge)]

#### use the optimal lambda for the model:
ridge_pred = predict(ridge_mod, s = best_ridge_lambda, newx = X)

#### correlation: 
##### in-sample correlation
y_true_train_ridge = train_RL$Asset_1_FRet_10
y_pred_train_ridge = ridge_pred[1:nrow(train_RL)]
cor_in_sample_ridge = cor(y_true_train_ridge, y_pred_train_ridge) 

##### out-of-sample correlation
y_true_test_ridge = test_RL$Asset_1_FRet_10
y_pred_test_ridge = ridge_pred[-(1:nrow(train_RL))]
cor_out_sample_ridge = cor(y_true_test_ridge, y_pred_test_ridge) 


### (4) LASSO model - lasso_mod. 
#### Model Fitting: 
lasso_mod =glmnet(X[train_id,], y[train_id], alpha=1)

#### Best lambda through validation MSE
n = length(lasso_mod$lambda)
MSE_lasso = c()

for (i in 1:n) {
  lasso_pred_test =predict(lasso_mod, s = lasso_mod$lambda[i], newx = X[-train_id,] )
  MSE_lasso[i] =  mean( (lasso_pred_test - y[-train_id])^2 )
}

#### find the best lambda: 
lasso_lambda = data.frame( lambda = lasso_mod$lambda, MSE = MSE_lasso )

##### Plot the lambda with corresponding MSE
lasso_lambda_plot = lasso_lambda %>% 
  ggplot2::ggplot( aes(x = lambda, y = MSE) ) +
  ggplot2::geom_point( colour = 'orchid', size = 1) + 
  xlab("lambda") + 
  ylab("Validation MSE") + 
  ggtitle("LASSO lambda with corresponding validation MSE") +
  theme(plot.title = element_text(hjust = 0.5))

best_lasso_lambda = lasso_mod$lambda[which.min(MSE_lasso)]

#### use the optimal lambda for the model:
lasso_pred = predict(lasso_mod, s = best_lasso_lambda, newx = X)


#### Correlation:
##### in-sample correlation
y_true_train_lasso = train_RL$Asset_1_FRet_10
y_pred_train_lasso = lasso_pred[1:nrow(train_RL)]
cor_in_sample_lasso = cor(y_true_train_lasso, y_pred_train_lasso) 

##### out-of-sample correlation
y_true_test_lasso = test_RL$Asset_1_FRet_10
y_pred_test_lasso = lasso_pred[-(1:nrow(train_RL))]
cor_out_sample_lasso = cor(y_true_test_lasso, y_pred_test_lasso) 


# Correlation Table: ----------------------------------------------------------
in_sample_cor = c(cor_in_sample_lm, cor_in_sample_KNN, 
                  cor_in_sample_ridge, cor_in_sample_lasso)
out_sample_cor = c(cor_out_sample_lm, cor_out_sample_KNN, 
                   cor_out_sample_ridge, cor_out_sample_lasso)
model_cor = as.data.frame(cbind(in_sample_cor, out_sample_cor))

# Save Data: ------------------------------------------------------------------
save(ridge_lambda_plot, 
     lasso_lambda_plot, 
     model_cor, lmod, 
     lm_corr_plot, 
     trainMSE_KNN,
     testMSE_KNN,
     k_range,
     file = "price_model.RData")

# 79: -------------------------------------------------------------------------