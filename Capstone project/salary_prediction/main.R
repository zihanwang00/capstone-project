library(dplyr)
library(stringr)
library(tidyverse)
library(tools)
library(lubridate)
library(randomForest)
library(countrycode)
library(caret)
library(xgboost)


source("../r_scripts/clean_data.R")
source("../r_scripts/inflation.R")
source("../r_scripts/fit_model.R")


jobs <- read_csv("../original_data/ds_salaries.csv")

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

lm_model <- lm(salary_in_usd ~ experience_level*remote_ratio + employee_residence + 
                 job_title, 
               data = train_data)
summary(lm_model)
predictions <- predict(lm_model, test_data)
lm_rmse <- sqrt(mean((predictions - test_data$salary_in_usd)^2))

############ Box Cox Transform ###############
source("http://www.reuningscherer.net/s&ds230/Rfuncs/regJDRS.txt")
lm_transform <- boxCox(lm_model)
d <- lm_transform$x[which.max(lm_transform$y)]

lm_model_transform <- lm(salary_in_usd^d ~ + experience_level*remote_ratio + employee_residence + 
                           job_title, 
                         data = train_data)
summary(lm_model_transform)

predictions <- predict(lm_model_transform, test_data, interval = "predict", level = 0.9)^(1/d)
lm_rmse_transform <- sqrt(mean((predictions[,1] - test_data$salary_in_usd)^2))


################# Save Final Model ######################
saveRDS(lm_model, file = "./lm_model.rds")
saveRDS(lm_model_transform, file = "./final_model.rds")
