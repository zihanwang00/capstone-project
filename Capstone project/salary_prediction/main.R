library(dplyr)
library(stringr)
library(tools)
library(tidyverse)
library(lubridate)
library(randomForest)
library(caret)
library(xgboost)


source("./salary_prediction/r_scripts/clean_data.R")
source("./salary_prediction/r_scripts/inflation.R")
source("./salary_prediction/r_scripts/fit_model.R")


jobs <- read_csv("./original_data/ds_salaries.csv")

cleaned_jobs <- jobs %>%
  clean_data() %>%
  rowwise() %>%
  mutate(salary_in_usd = adjust_salary(work_year, salary_in_usd, salary_currency)) %>%
  ungroup() %>%
  select(-work_year, -salary_currency)

write_csv(cleaned_jobs, "cleaned_jobs.csv")

############################# Models ############################# 

######################### Linear Model ############################
set.seed(123) 
split_index <- createDataPartition(cleaned_jobs$employee_residence, p = 0.7, list = FALSE)
train_data <- cleaned_jobs[split_index, ]
test_data <- cleaned_jobs[-split_index, ]

lm_model <- lm(salary_in_usd ~ experience_level*remote_ratio + employee_residence + job_title, data = train_data)
summary(lm_model)
predictions <- predict(lm_model, test_data)
lm_rmse <- sqrt(mean((predictions - test_data$salary_in_usd)^2))

############ Box Cox Transform ###############
source("http://www.reuningscherer.net/s&ds230/Rfuncs/regJDRS.txt")
lm_transform <- boxCox(lm_model)
d <- lm_transform$x[which.max(lm_transform$y)]

lm_model_transform <- lm(salary_in_usd^d ~ experience_level+ remote_ratio + employee_residence + job_title + experience_level:remote_ratio + employee_residence:remote_ratio + employee_residence:experience_level, 
                         data = train_data)
summary(lm_model_transform)

predictions <- predict(lm_model_transform, test_data, interval = "predict", level = 0.9)^(1/d)
lm_rmse_transform <- sqrt(mean((predictions[,1] - test_data$salary_in_usd)^2))

######################### Tree Models ############################
# train control for random forest and gradient boost
train_control <- trainControl(method = "cv", number = 5)
# the grid of hyperparameters to search for random forest
grid_rf <- expand.grid(mtry = c(2, 3, 4))

rf_model <- tree_model(train_data, train_control, grid_rf, "rf")
best_rf <- model_performance(rf_model)
best_rf$RMSE

# predictions <- predict(rf_model$finalModel, test_data)

saveRDS(lm_model, file = "./lm_model.rds")
saveRDS(lm_model_transform, file = "./final_model.rds")

#' Choose job name
#' Choose job country
#' Choose Level
#' 
# 

table(cleaned_jobs$experience_level)
table(cleaned_jobs$remote_ratio)
table(cleaned_jobs$employee_residence)
table(cleaned_jobs$job_title)


mod1 <- lm(salary_in_usd ~ experience_level*remote_ratio, data = cleaned_jobs)
Anova(mod1, type = 'III')

mod2 <- lm(salary_in_usd ~ employee_residence*remote_ratio, data = cleaned_jobs)
Anova(mod2, type = 'III')

plot(mod1)
# 
# 
# job_title <- cleaned_jobs %>% 
#   select(job_title) %>%
#   group_by(job_title) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# 
# cleaned_jobs %>% 
#   select(remote_ratio) %>%
#   group_by(remote_ratio) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# 
# 
# # visualization
# salary_plot(cleaned_jobs)
# quantile_plot(box_plot)
# box_plot(box_plot)
# plot_histograms(cleaned_jobs, "remote_ratio")
# plot_histograms(cleaned_jobs, "experience_level")
# 
# 




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