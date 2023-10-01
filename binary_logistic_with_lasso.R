# Helper packages
library(glmnet)
library(caret)
library(ROSE)
library(tidyverse)

# Set working directory
setwd("C:/Personal/Books/MSc. in Applied Statistics/Semester_3/MAS5312_Statistical_Machine_Learning/Project_2")

# Loading data sets
train_df <- read.csv('data_sets/train_prep.csv')
test_df <- read.csv('data_sets/test_prep.csv')

# Label encoding existing categories
train_df <- train_df %>%  mutate(
  Response = as.character(Response),
  Gender = as.character(Gender),
  Driving_License = as.character(Driving_License),
  Previously_Insured = as.character(Previously_Insured),
  Vehicle_Age = as.character(Vehicle_Age),
  Vehicle_Damage = as.character(Vehicle_Damage),
  Region_Category = as.character(Region_Category),
  Sales_Channel_Category = as.character(Sales_Channel_Category),
  Age_Category = as.character(Age_Category),
) 

# Label encoding existing categories
test_df <- test_df %>%  mutate(
  Response = as.character(Response),
  Gender = as.character(Gender),
  Driving_License = as.character(Driving_License),
  Previously_Insured = as.character(Previously_Insured),
  Vehicle_Age = as.character(Vehicle_Age),
  Vehicle_Damage = as.character(Vehicle_Damage),
  Region_Category = as.character(Region_Category),
  Sales_Channel_Category = as.character(Sales_Channel_Category),
  Age_Category = as.character(Age_Category),
) 

str(train_df)

# Under sampling approach
train_df_positive <- train_df %>% filter(Response=='1')
dim(train_df_positive)[1]
train_df_negative <- train_df %>% filter(Response=='0')
dim(train_df_negative)[1]

undersampled_data <- ovun.sample(Response ~ ., 
                                 data = train_df, 
                                 method = "under", 
                                 N = 2* dim(train_df_positive)[1])$data 
nrow(undersampled_data)
undersampled_data %>% filter(Response=='1')

X <- model.matrix( ~ ., dplyr::select(undersampled_data, -Response))[, -1]
y <- undersampled_data$Response
str(X)

X_test <- model.matrix( ~ ., dplyr::select(test_df, -Response))[, -1]
y_test <- test_df$Response


# Fit a model with cross validation with Lasso 
cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = 1)

plot(cv_fit$lambda)
print(cv_fit$lambda.min)

# Fit the final model using the best lambda
lasso_model <- glmnet(X, y, 
                      family = "binomial", 
                      alpha = 1, 
                      lambda = cv_fit$lambda.min)

print(lasso_model$beta)

# Predict on test data
pred_lasso <- predict(lasso_model, newx = X_test, type = "response")

# Convert predictions to binary (0/1) keeping threshold to 0.5
pred_cls_lasso <- as.numeric(pred_lasso > 0.5)

# Calculate accuracy
accuracy_lasso <- sum(pred_cls_lasso == y_test) / length(y_test)
print(accuracy_lasso)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(data=as.factor(ifelse(pred_lasso > 0.5, "1", "0")), 
                                    mode = "everything",
                                    positive="1",
                                    reference=as.factor(y_test))

confusion_matrix

# print(confusion_matrix_u)
#table(pred_cls_redge_u,y_test)

# Use of interaction variables

X_iv <- model.matrix( ~ Gender + Driving_License + Previously_Insured + Vehicle_Age+ Vehicle_Damage + 
                        Annual_Premium+ Vintage + Region_Category+ Sales_Channel_Category + Age_Category+ 
                        Annual_Premium * Vehicle_Age + Annual_Premium * Vehicle_Damage, 
                      dplyr::select(undersampled_data, -Response))[, -1]
y_iv <- undersampled_data$Response
str(X_iv)

X_test <- model.matrix( ~ Gender + Driving_License + Previously_Insured + Vehicle_Age+ Vehicle_Damage + 
                          Annual_Premium+ Vintage + Region_Category+ Sales_Channel_Category + Age_Category+ 
                          Annual_Premium * Vehicle_Age + Annual_Premium * Vehicle_Damage, 
                        dplyr::select(test_df, -Response))[, -1]
y_test <- test_df$Response

# Fit a model with cross validation with Ridge 
cv_fit_iv <- cv.glmnet(X_iv, y_iv, family = "binomial", alpha = 0)

plot(cv_fit_iv$lambda)
print(cv_fit_iv$lambda.min)

# Fit the final model using the best lambda
lasso_model_iv <- glmnet(X_iv, y_iv, 
                         family = "binomial", 
                         alpha = 0, 
                         lambda = cv_fit_iv$lambda.min)

print(lasso_model_iv$beta)

# Predict on test data
pred_lasso_iv <- predict(lasso_model_iv, newx = X_test, type = "response")

# Convert predictions to binary (0/1) keeping threshold to 0.5
pred_cls_lasso_iv <- as.numeric(pred_lasso_iv > 0.5)

# Calculate accuracy
accuracy_lasso_iv <- sum(pred_cls_lasso_iv == y_test) / length(y_test)
print(accuracy_lasso_iv)


# Create a confusion matrix
confusion_matrix_iv <- confusionMatrix(data=as.factor(ifelse(pred_lasso_iv > 0.5, "1", "0")), 
                                       mode = "everything",
                                       positive="1",
                                       reference=as.factor(y_test))

confusion_matrix_iv
