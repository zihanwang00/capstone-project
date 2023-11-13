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

na_ratio <- jobs %>%
  summarise_all(~sum(is.na(.))/n()*100)


job_title <- cleaned_jobs %>% 
  select(job_title) %>%
  unique()

job_cleaned <- write_csv(cleaned_jobs, "cleaned_jobs.csv")

# visualization
salary_plot(cleaned_jobs)
quantile_plot(box_plot)
box_plot(box_plot)
plot_histograms(cleaned_jobs, "remote_ratio")
plot_histograms(cleaned_jobs, "experience_level")


# modeling: knn, random forest


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