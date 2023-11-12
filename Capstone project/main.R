library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(tools)


setwd("~/Documents/GitHub/capstone-project/Capstone project")

source("./r_scripts/clean_data.R")
source("./r_scripts/visualizations.R")


jobs <- read_csv("./data/DS_jobs/ds_salaries.csv")

cleaned_jobs <- clean_data(jobs)
#' Choose job name
#' Choose job country
#' Choose Level
#' 
summary(jobs)

na_ratio <- jobs %>%
  summarise_all(~sum(is.na(.))/n()*100)

str(jobs)

job_title <- cleaned_jobs %>% 
  select(job_title) %>%
  unique()
# visualization

# correlation

# modeling

lm(salary ~ experience_level * job_title * employee_residence * remote_ratio, data = jobs)





job_cleaned %>% count(title) %>% arrange(desc(n))

unique_dates <- job_cleaned %>% select(original_listed_time) %>% arrange(original_listed_time) %>% unique()



job_cleaned <- write_csv(job_cleaned, "cleaned_jobs.csv")


# Choose country in US, post date after 
#' Task 1: What are the trending jobs with the highest number of job postings?
#' Clean Up Job Title, 
#' 
#' animated line graph to show how the number of job postings change over time


#' Salary Prediction: (record linkage package)
#' If range is the outcome, just pick the middle point -- 
#' Regression of a Range as a outcome (If the outcome is range)
#' Choose the outcome to be middle point and stand deviation
#' 
#' 
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