library(dplyr)
library(stringr)
library(tools)
library(tidyverse)
library(lubridate)
library(countrycode)

setwd("~/Documents/GitHub/capstone-project/Capstone project")

source("./r_scripts/clean_data.R")
source("./r_scripts/inflation.R")
source("./r_scripts/visualizations.R")
source("./r_scripts/fit_model.R")


jobs <- read_csv("./data/DS_jobs/ds_salaries.csv")

cleaned_jobs <- jobs %>%
  clean_data() %>%
  rowwise() %>%
  mutate(salary_in_usd = adjust_salary(work_year, salary_in_usd, salary_currency)) %>%
  ungroup() %>%
  select(-work_year, -salary_currency)

write_csv(cleaned_jobs, "cleaned_jobs.csv")

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
  select(experience_level) %>%
  group_by(experience_level) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

residence <- cleaned_jobs %>% 
  select(job_title) %>%
  group_by(job_title) %>%
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




#' Salary Prediction: (record linkage package)
#' If range is the outcome, just pick the middle point -- 
#' Regression of a Range as a outcome (If the outcome is range)
#' Choose the outcome to be middle point and stand deviation
#' 
#' Average salary across different countries for specific job
#' 1. What are the trending jobs with the highest number of job postings?
#' 
#' 
#' 2. Which geographic locations are most preferred by employers for specific roles or industries?
#' job_title, company_location
#' 
#' 3. What are the most sought-after skills across various industries?
#' skill_bar (at job_skills.csv)
#' 
#' 
#' time series forecasting (number of job postings)
#' text summarizarion
#' skill analysis
#' salary prediction