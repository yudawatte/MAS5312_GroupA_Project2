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

# Apply k-nearest neighbor model
set.seed(2)
training_control <- trainControl(method = "repeatedcv",
                                 summaryFunction = defaultSummary,
                                 #classProbs = TRUE,
                                 number = 10,
                                 repeats = 3)


knn_cv_model <- train(Response ~ ., 
                      data = undersampled_data,
                      method = "knn",
                      trControl = training_control,
                      #metric = "Accuracy",
                      #tuneGrid = data.frame(k = c(3,5,7)) # seq(11,85,by = 2)
                      ) 

#Variable Importance
vip(knn_cv_model, 
    num_features = 10, geom ="point")

knn_pred = predict(knn_cv_model, newdata=test_df)

length(knn_pred)
dim(test_df)

knn_pred_cls <- as.numeric(knn_pred > 0.5)

table(knn_pred_cls,y_test)

mean(knn_pred_cls==y_test)

confusion_matrix <- confusionMatrix(data=as.factor(ifelse(knn_pred > 0.5, "1", "0")),
                                    mode = "everything",
                                    positive="1",
                                    reference=as.factor(y_test))

confusion_matrix
