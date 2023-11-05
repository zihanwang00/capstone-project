library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)

setwd("~/Documents/GitHub/capstone-project/Capstone project")

source("./r_scripts/get_data.R")
source("./r_scripts/clean_data.R")



jobs <- read_csv("./data/postings/job_postings.csv")
companies <- read_csv("./data/company_details/companies.csv")
job_skill <- read_csv("./data/job_details/job_skills.csv")

job_skill <- job_skill_data(job_skill)
entry_job <- get_data(jobs, c("Entry level"))



# Check Missing Value
entry_job %>%
  summarise_all(~sum(is.na(.))/n()*100)

cleaned_entry <- title_cleaning(entry_job)

# 选country in US, post date after 
#' Task 1: What are the trending jobs with the highest number of job postings?
#' Clean Up Job Title, clean up 后选前100个name
#' animated line graph to show how the number of job postings change over time

entry_job %>%
  select(title) %>%
  group_by(title) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


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