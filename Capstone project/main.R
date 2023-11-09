library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)


setwd("~/Documents/GitHub/capstone-project/Capstone project")

source("./r_scripts/get_data.R")
source("./r_scripts/clean_data.R")
source("./r_scripts/visualizations.R")


jobs <- read_csv("./data/postings/job_postings.csv")
companies <- read_csv("./data/company_details/companies.csv")
job_skill <- read_csv("./data/job_details/job_skills.csv")

job_skill <- job_skill_data(job_skill)
get_job <- get_data(jobs, c("Entry level"))
job_cleaned <- title_cleaning(get_job)

# Data Cleaning
## Check Missing Value
jobs %>%
  select(max_salary,
         min_salary,
         med_salary) %>%
  summarise_all(~sum(is.na(.))/n()*100)


word_cloud(get_job)
line_graph(job_cleaned)



job_cleaned %>% count(title) %>% arrange(desc(n))

unique_dates <- job_cleaned %>% select(original_listed_time) %>% arrange(original_listed_time) %>% unique()



job_cleaned <- write_csv(job_cleaned, "cleaned_jobs.csv")


# Choose country in US, post date after 
#' Task 1: What are the trending jobs with the highest number of job postings?
#' Clean Up Job Title, 
#' 
#' animated line graph to show how the number of job postings change over time

get_job %>%
  select(title) %>%
  group_by(title) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


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