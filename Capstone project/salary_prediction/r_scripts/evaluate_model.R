library(randomForest)
library(caret)
library(xgboost)

source("./salary_prediction/r_scripts/fit_model.R")

set.seed(123) 
split_index <- createDataPartition(cleaned_jobs$employee_residence, p = 0.7, list = FALSE)
train_data <- cleaned_jobs[split_index, ]
test_data <- cleaned_jobs[-split_index, ]


# model <- lm(salary_in_usd ~ experience_level * job_title + employee_residence + remote_ratio, data = train_data)
# summary(model)
# predictions <- predict(model, test_data)
# rmse <- sqrt(mean((predictions - test_data$salary_in_usd)^2))
# mae <- mean(abs(predictions - test_data$salary_in_usd))

# train control for random forest and gradient boost
train_control <- trainControl(method = "cv", number = 5)

# the grid of hyperparameters to search for random forest
grid_rf <- expand.grid(mtry = c(2, 3, 4))
# the grid of hyperparameters to search for gradient boost
grid_gb <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 0.1, 0.2),
  colsample_bytree = c(0.5, 0.7, 1),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.5, 0.7, 1)
)


rf_model <- tree_model(train_data, train_control_rf, grid_rf, "rf")
gb_model <- tree_model(train_data, train_control_gb, grid_gb, "xgbTree")


best_rf <- model_performance(rf_model)
best_gb <- model_performance(gb_model)

best_rf$RMSE
best_gb$RMSE



rf_predictions <- predict(best_model, test_data)
rf_rmse_test <- sqrt(mean((rf_predictions - test_data$salary_in_usd)^2))


