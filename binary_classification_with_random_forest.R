# Helper packages
library(caret)
library(ROSE)
library(rpart.plot)
library(tidyverse)
library(vip)
library(rpart)       # direct engine for decision tree application
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects
library(ipred)       # for fitting bagged decision trees
library(ranger)

# Set working directory
setwd("C:/Personal/Books/MSc. in Applied Statistics/Semester_3/MAS5312_Statistical_Machine_Learning/Project_2")

# Loading data sets
train_df <- read.csv('data_sets/train_prep.csv')
test_df <- read.csv('data_sets/test_prep.csv')

X_test <- as.matrix(select(test_df, -c("Response")))
y_test <- as.numeric(test_df$Response)

# Under sampling approach
train_df_positive <- train_df %>% filter(Response==1)
dim(train_df_positive)[1]
train_df_negative <- train_df %>% filter(Response==0)
dim(train_df_negative)[1]

undersampled_data <- ovun.sample(Response ~ ., data = train_df, method = "under", N = 2* dim(train_df_positive)[1])$data
nrow(undersampled_data)

# train a default random forest model
n_features <- length(setdiff(names(undersampled_data), "Response"))

rf_model <- ranger(
  Response ~ ., 
  data = undersampled_data,
  mtry = floor(sqrt(n_features)),
  classification = TRUE,
  seed = 123
)

# get OOB Error
default_error <-rf_model$prediction.error

rf_pred =predict(rf_model,data=test_df)

table(rf_pred$predictions,y_test)
mean(rf_pred$predictions==y_test)

# re-run model with impurity-based variable importance
rf_model_impurity <- ranger(
  formula = Response ~ ., 
  data = undersampled_data, 
  num.trees = 1000,
  mtry =3,
  min.node.size = 5,
  sample.fraction = .75,
  replace = FALSE,
  importance = "impurity",
  probability = FALSE,
  verbose = FALSE,
  seed  = 1
)

# variable importance
vip(rf_model_impurity, num_features = 8, geom ="point")

#Prediction on test set
rf_pred_impurity = predict(rf_model_impurity, data=test_df)

pred_cls_rf_impurity <- as.numeric(rf_pred_impurity$predictions > 0.5)

table(pred_cls_rf_impurity,y_test)

mean(pred_cls_rf_impurity==y_test)

confusion_matrix <- confusionMatrix(data=as.factor(pred_cls_rf_impurity), # ifelse(rf_pred_impurity > 0.5, "1", "0")
                                    mode = "everything",
                                    positive="1",
                                    reference=as.factor(y_test))

confusion_matrix



