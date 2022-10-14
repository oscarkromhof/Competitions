# Required library
library(tidyverse)
library(randomForest)
library(varImp)
library(caret)


# Reload Train and Test Dataset
df_train <- read_rds("train.rds")
df_test <- read_rds("test.rds")

# MSE Function
mse <- function(y_true, y_pred) mean((y_true - y_pred)^2)

# Cross Validation Function for Random Forest
cv_randomForest <- function(formula, dataset, k, n_tree, m ) {
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
    model_i <- randomForest(formula = formula, data = data_train, ntree = n_tree, mtry = m, importance = TRUE)
    
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

# Check Features that have high contribution to predict score.
simple_Rf <- randomForest(score ~ . , data = df_train, importance = TRUE)
simple_Rf
varImp(simple_Rf, conditional = TRUE)

# Pick features and create model from that
simple_Rf2 <- randomForest(score ~  Walc + reason + health + absences + goout + schoolsup + failures + sex , data = df_train, ntree = 2000, mtry = 2 , importance = TRUE)
simple_Rf2

# Validate model with Cross Validation
Rf_mse2 <- cv_randomForest(score ~ Walc + reason + health + absences + goout + schoolsup + failures + sex , dataset = df_train, k = 6, n_tree = 2000, m = 2)
Rf_mse2

# Predict Test dataset using model
pred_vec <- predict(simple_Rf2, df_test)
pred_vec

# Save result of prediction
write_rds(pred_vec, file = "pred_G3_9.rds")
