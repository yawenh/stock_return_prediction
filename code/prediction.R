## Final Model Prediction: 
##
## 
##
## Author: Yawen Hu
## Updated: November 17, 2020 

# 79: -------------------------------------------------------------------------
# libraries: ------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(glmnet)

# directories: ----------------------------------------------------------------
setwd("~/Desktop/urop/")

# data: -----------------------------------------------------------------------
foo = load("./data/price_data.RData") 

# function: --------------------------------------------------------------------
# I. Add more predictors to the data set:
fret_new = fret
for(j in c(1, 2, 5)){
  for(i in 1:3){
    fret_new = bret_dataframe(fret_new, i, j)
  }
}

# II. Predictor Selection by LASSO Model:
train_size <- nrow(fret_new)*0.7
train_id <- 1:train_size

train_RL <- fret_new[train_id, ]
test_RL <- fret_new[-train_id, ]
X = model.matrix(Asset_1_FRet_10 ~., fret_new)[, -1]
y = fret_new$Asset_1_FRet_10

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
best_lasso_lambda = lasso_mod$lambda[which.min(MSE_lasso)]

#### use the optimal lambda for the model:
lasso_pred = predict(lasso_mod, s = best_lasso_lambda, newx = X)
lasso_coef = predict(lasso_mod, s = best_lasso_lambda, type = "coefficients")
lasso_coef

# III. Use selected variable to fit the Final Linear Model:
fret_new = fret_new %>%
  select(c(Asset_1_BRet_1, Asset_2_BRet_1, Asset_3_BRet_1, Asset_1_BRet_2, 
           Asset_2_BRet_2, Asset_3_BRet_2, Asset_1_BRet_5, Asset_2_BRet_5,
           Asset_2_BRet_30, Asset_1_BRet_30, Asset_1_BRet_10, Asset_2_BRet_3,
           Asset_1_BRet_3, Asset_1_FRet_10))

train_size = nrow(fret_new)*0.7
train_id = 1:train_size

train_lm <- fret_new[train_id, ]
test_lm <- fret_new[-train_id, ]

### (3) Fit Linear Model - lmod. 
final_mod = lm(Asset_1_FRet_10 ~., train_lm)
summary(final_mod)

#### (4) In-sample and Out-of-sample correlation. 

##### in-sample correlation:
y_true_train_final = train_lm$Asset_1_FRet_10
y_pred_train_final = predict(final_mod, train_lm)
cor_in_sample_final= cor(y_true_train_final, y_pred_train_final) 

##### out-of-sample correlation:
y_true_test_final = test_lm$Asset_1_FRet_10
y_pred_test_final = predict(final_mod, test_lm)
cor_out_sample_final = cor(y_true_test_final, y_pred_test_final) 


save(cor_in_sample_final, cor_out_sample_final, file = "prediction.RData")

# 79: -------------------------------------------------------------------------
