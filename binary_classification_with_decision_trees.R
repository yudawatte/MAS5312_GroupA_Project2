# Helper packages
library(caret)
library(ROSE)
library(rpart.plot)
library(tidyverse)
library(vip)

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

# Under sampling data
undersampled_data <- ovun.sample(Response ~ ., data = train_df, method = "under", N = 2* dim(train_df_positive)[1])$data
nrow(undersampled_data)


# Fitting a classification tree
init_tree_model <- rpart(
  formula = Response ~ .,
  data    = undersampled_data,
  method  = "class",
  control = rpart.control(maxdepth = 5),
  parms=list(split="gini")
)

init_tree_model

# Plotting the tree
rpart.plot(init_tree_model)

# Fine tuning
#caret cross validation results
cp_grid <- data.frame(cp = seq(0.02, .2, .02))
tree_model <- train(
  Response ~ .,
  data = undersampled_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  parms=list(split="gini"),
  tuneGrid = cp_grid,
  tuneLength = 25
)

ggplot(tree_model)
tree_model$bestTune
rpart.plot(tree_model$finalModel,type=2) 

#Variable Importance
vip(tree_model$finalModel, 
    num_features = 10, geom ="point")


#Construct partial dependence plots
# p1<-partial(tree_model$finalModel, 
#             pred.var = "Previously_Insured",
#             type="classification",
#             prob=TRUE,which.class=2L)%>% autoplot() 

str(undersampled_data)


#Prediction on test set
pred_dtree_u = predict(tree_model,type = 'raw',newdata=test_df)
pred_cls_dtree <- as.numeric(pred_dtree_u > 0.5)


# Calculate accuracy
accuracy_dtree_u <- sum(pred_cls_dtree == y_test) / length(y_test)
print(accuracy_dtree_u)

# Create a confusion matrix
table(pred_cls_dtree,y_test)

confusion_matrix <- confusionMatrix(data=as.factor(ifelse(pred_dtree_u > 0.5, "1", "0")),
                                    mode = "everything",
                                    positive="1",
                                    reference=as.factor(y_test))

confusion_matrix
