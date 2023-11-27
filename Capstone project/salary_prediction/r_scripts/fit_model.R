##' model functions -- > use on training model, create tuning functions
##' evaluation function --> one, use on predicting on test data
##' 
##' 
##' evaluate_model:
##' calculate rmse for all models

linear_model <- function(df){
  lm_model <- lm(salary_in_usd ~ experience_level + job_title + employee_residence + remote_ratio, data = df)
}

tree_model <- function(df, train_control, grid, method) {
  df$experience_level <- as.factor(df$experience_level)
  df$employee_residence <- as.factor(df$employee_residence)
  df$job_title <- as.factor(df$job_title)
  df$remote_ratio <- as.factor(df$remote_ratio)

  model <- train(salary_in_usd ~ ., data = df, method, trControl = train_control, tuneGrid = grid, verbosity = 0)
  
  return(model)
}


model_performance <- function(model){
  results <- model$results
  best <- results[which.min(results$RMSE),]

}
