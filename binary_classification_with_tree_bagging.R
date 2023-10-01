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

#make bootstrapping reproducible
#set.seed(1)

#train bagged model using ipred package
bagging_model <- bagging(
  formula = Response~ .,
  data = undersampled_data,
  nbagg = 10,  
  coob = TRUE,
  control = rpart.control(maxdepth = 5,
                          parms=list(split="gini"),
                          method  = "class") #rpart.control(minsplit = 2, cp = 0)
  )

bagging_model

#Prediction on test set
pred_bag = predict(bagging_model,
                   newdata=test_df)

pred_cls_bag <- as.numeric(pred_bag > 0.5)


# Calculate accuracy
accuracy_bag <- sum(pred_cls_bag == y_test) / length(y_test)
print(accuracy_bag)

# Create a confusion matrix
table(pred_cls_bag,y_test)

# Train bagged model using caret package with cross validation
#set.seed(123)

#train bagged model
bagging_model_cv <- train(
  Response ~ .,
  data = undersampled_data,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  nbagg = 50,  
  control = rpart.control(maxdepth = 5,
                          parms=list(split="gini"),
                          method  = "class")
)
#bagging_model_cv

#Variable Importance
#use model trained using caret package to get vip
vip(bagging_model_cv, num_features = 8, geom ="point")

#rpart.plot(bagging_model_cv,type=2) 

#Prediction on test set
pred_bag_cv = predict(bagging_model_cv,
                      newdata=test_df)

pred_cls_bag_cv <- as.numeric(pred_bag_cv > 0.5)


# Calculate accuracy
accuracy_bag_cv <- sum(pred_cls_bag_cv == y_test) / length(y_test)
print(accuracy_bag_cv)

# Create a confusion matrix
table(pred_cls_bag_cv,y_test)

confusion_matrix <- confusionMatrix(data=as.factor(ifelse(pred_bag_cv > 0.5, "1", "0")),
                                    mode = "everything",
                                    positive="1",
                                    reference=as.factor(y_test))

confusion_matrix


