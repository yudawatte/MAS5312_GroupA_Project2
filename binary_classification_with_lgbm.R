# Helper packages
library(mlexperiments)
library(mllrnrs)

# Set working directory
setwd("C:/Personal/Books/MSc. in Applied Statistics/Semester_3/MAS5312_Statistical_Machine_Learning/Project_2")

# Loading data sets
train_df <- read.csv('data_sets/train_prep.csv')
test_df <- read.csv('data_sets/test_prep.csv')

# Under sampling approach
train_df_positive <- train_df %>% filter(Response==1)
dim(train_df_positive)[1]
train_df_negative <- train_df %>% filter(Response==0)
dim(train_df_negative)[1]

undersampled_data <- ovun.sample(Response ~ ., data = train_df, method = "under", N = 2* dim(train_df_positive)[1])$data
nrow(undersampled_data)

# required learner arguments, not optimized
learner_args <- list(
  max_depth = -1L,
  verbose = -1L,
  objective = "binary",
  metric = "binary_logloss"
)

# set arguments for predict function and performance metric,
# required for mlexperiments::MLCrossValidation and
# mlexperiments::MLNestedCV
predict_args <- NULL
performance_metric <- metric("auc")
performance_metric_args <- list(positive = "1")
return_models <- FALSE

# required for grid search and initialization of bayesian optimization
parameter_grid <- expand.grid(
  bagging_fraction = seq(0.6, 1, .2),
  feature_fraction = seq(0.6, 1, .2),
  min_data_in_leaf = seq(2, 10, 2),
  learning_rate = seq(0.1, 0.2, 0.1),
  num_leaves = seq(2, 20, 4)
)
# reduce to a maximum of 10 rows
if (nrow(parameter_grid) > 10) {
  set.seed(123)
  sample_rows <- sample(seq_len(nrow(parameter_grid)), 10, FALSE)
  parameter_grid <- kdry::mlh_subset(parameter_grid, sample_rows)
}

# required for bayesian optimization
parameter_bounds <- list(
  bagging_fraction = c(0.2, 1),
  feature_fraction = c(0.2, 1),
  min_data_in_leaf = c(2L, 12L),
  learning_rate = c(0.1, 0.2),
  num_leaves =  c(2L, 20L)
)
optim_args <- list(
  iters.n = 1L,
  kappa = 3.5,
  acq = "ucb"
)


tuner <- mlexperiments::MLTuneParameters$new(
  learner = mllrnrs::LearnerLightgbm$new(
    metric_optimization_higher_better = FALSE
  ),
  strategy = "grid",
  ncores = 1L,
  seed = 123
)

tuner$parameter_grid <- parameter_grid
tuner$learner_args <- learner_args
tuner$split_type <- "stratified"

tuner$set_data(
  x = as.matrix(select(undersampled_data, -c("Response"))),
  y = as.numeric(undersampled_data$Response)
)

tuner_results_grid <- tuner$execute(k = 3)
##############################################

# Model fitting 
X_u <- as.matrix(select(undersampled_data, -c("Response")))
y_u <- as.numeric(undersampled_data$Response)

# Fitting a classification tree
dtree_model_u <- rpart(
  formula = Response ~ .,
  data    = undersampled_data,
  method  = "class",
  #control = rpart.control(maxdepth = 5),
  parms=list(split="gini")
)

# Plotting the tree
rpart.plot(dtree_model_u)

#Prediction on test set
pred_dtree_u = predict(dtree_model_u,
                       type = 'class',
                       newdata=test_df)


# Calculate accuracy
accuracy_dtree_u <- sum(pred_dtree_u == y_test) / length(y_test)
print(accuracy_dtree_u)

# Create a confusion matrix
table(pred_dtree_u,y_test)
