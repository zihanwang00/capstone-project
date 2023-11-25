##' model functions -- > use on training model, create tuning functions
##' evaluation function --> one, use on predicting on test data
##' 
##' 
##' evaluate_model:
##' calculate rmse for all models



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


test_model <- function(best_model, test_data){
  rf_predictions <- predict(best_model, test_data)
  rf_rmse <- sqrt(mean((rf_predictions - test_data$salary_in_usd)^2))
  
  return(rf_rmse)
}
  
