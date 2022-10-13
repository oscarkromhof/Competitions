library(tidyverse)
library(xgboost)
library(dplyr)
library(Matrix)
df_train <- read_rds("data/train.rds")
df_test  <- read_rds("data/test.rds")

# Just for reference, here is the mse() function once more
mse <- function(y_true, y_pred) mean((y_true - y_pred)^2)

cv_xgboost <- function(formula, dataset, k) {
  # We can do some error checking before starting the function
  stopifnot(is_formula(formula))       # formula must be a formula
  stopifnot(is.data.frame(dataset))    # dataset must be data frame
  stopifnot(is.integer(as.integer(k))) # k must be convertible to int
  
  # first, add a selection column to the dataset as before
  n_samples  <- nrow(dataset)
  select_vec <- rep(1:k, length.out = n_samples)
  data_split <- dataset %>% mutate(folds = sample(select_vec))
  
  # initialise an output vector of k mse values, which we 
  # will fill by using a _for loop_ going over each fold
  mses <- rep(0, k)
  
  # start the for loop
  for (i in 1:k) {
    # split the data in train and validation set
    data_train <- data_split %>% filter(folds != i)
    data_valid <- data_split %>% filter(folds == i)
    
    # calculate the model on this data
    whole_sparse_matrix <- sparse.model.matrix(score ~ ., data = data_train)
    sparse_matrix <- sparse.model.matrix(score ~ ., data = data_train)[,-1]
    output_vector = whole_sparse_matrix[,31]
    
    #define final training and testing sets
    xgb_train = xgb.DMatrix(data = sparse_matrix, label = output_vector)
    model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)
    
    # Extract the y column name from the formula
    y_column_name <- as.character(formula)[2]
    
    # calculate the mean square error and assign it to mses
    mses[i] <- mse(y_true = data_valid[[y_column_name]],
                   y_pred = predict(model_i, newdata = data_valid))
  }
  
  # now we have a vector of k mse values. All we need is to
  # return the mean mse!
  mean(mses)
}

whole_sparse_matrix <- sparse.model.matrix(score ~ ., data = df_train)
sparse_matrix <- sparse.model.matrix(score ~ ., data = df_train)[,-1]
output_vector = whole_sparse_matrix[,31]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = sparse_matrix, label = output_vector)
model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)

cv_xgboost(score ~ Walc + reason + health + absences + goout + schoolsup + failures + sex, df_train, 1)
