library(dplyr)
library(stringr)
library(tools)
library(tidyverse)
library(lubridate)
# library(countrycode)
library(randomForest)
library(caret)
library(xgboost)


setwd("~/Documents/GitHub/capstone-project/Capstone project")

source("./r_scripts/clean_data.R")
source("./r_scripts/inflation.R")
source("./r_scripts/fit_model.R")


jobs <- read_csv("./original_data/ds_salaries.csv")

cleaned_jobs <- jobs %>%
  clean_data() %>%
  rowwise() %>%
  mutate(salary_in_usd = adjust_salary(work_year, salary_in_usd, salary_currency)) %>%
  ungroup() %>%
  select(-work_year, -salary_currency)

write_csv(cleaned_jobs, "cleaned_jobs.csv")

############################# Models ############################# 

set.seed(123) 
split_index <- createDataPartition(cleaned_jobs$employee_residence, p = 0.7, list = FALSE)
train_data <- cleaned_jobs[split_index, ]
test_data <- cleaned_jobs[-split_index, ]

######################### Linear Model ############################
lm_model <- lm(salary_in_usd ~ experience_level + job_title + employee_residence + remote_ratio, data = train_data)
summary(lm_model)
predictions <- predict(lm_model, test_data)
lm_rmse <- sqrt(mean((predictions - test_data$salary_in_usd)^2))

######################### Tree Models ############################
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


rf_model <- tree_model(train_data, train_control, grid_rf, "rf")
gb_model <- tree_model(train_data, train_control, grid_gb, "xgbTree")


best_rf <- model_performance(rf_model)
best_gb <- model_performance(gb_model)

best_rf$RMSE
best_gb$RMSE

rf_model$finalModel
final_model <- gb_model$finalModel

rf_predictions <- predict(best_model, test_data)
rf_rmse_test <- sqrt(mean((rf_predictions - test_data$salary_in_usd)^2))




#' Choose job name
#' Choose job country
#' Choose Level
#' 

na_ratio <- jobs %>%
  summarise_all(~sum(is.na(.))/n()*100)


job_title <- cleaned_jobs %>% 
  select(job_title) %>%
  group_by(job_title) %>%
  summarise(n = n()) %>%
  arrange(desc(n))


cleaned_jobs %>% 
  select(remote_ratio) %>%
  group_by(remote_ratio) %>%
  summarise(n = n()) %>%
  arrange(desc(n))



# visualization
salary_plot(cleaned_jobs)
quantile_plot(box_plot)
box_plot(box_plot)
plot_histograms(cleaned_jobs, "remote_ratio")
plot_histograms(cleaned_jobs, "experience_level")


# modeling: knn, random forest
linear <- linear_model(cleaned_jobs)




#' 1. countries with n < 10 to "others"
#' 2. Stratified Sampling to separate train_data and test_data
#' 3. Weighted Sampling: use sample weights to give more importance to underrepresented classes.
#' 4. Random Forest / Gradient Boosting -> more robust to imbalanced data
#' 5. Evaluation Metric: 
#' 
#' Q: Countries in my data are: xxxx. How to visualize the difference of salaries on country (histogram)
#' 
#' 
#' Salary Prediction: (record linkage package)
#' If range is the outcome, just pick the middle point -- 
#' Regression of a Range as a outcome (If the outcome is range)
#' Choose the outcome to be middle point and stand deviation
#' 