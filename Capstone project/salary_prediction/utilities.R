################ Utilities #######################
setwd("~/Documents/GitHub/capstone-project/Capstone project/salary_prediction")

library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(tools)
library(tidyverse)
library(lubridate)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)

source("./r_scripts/fit_model.R")

data <- read_csv("./cleaned_jobs.csv")

data <- data %>%
  mutate(experience_level = as.factor(experience_level),
         job_title = as.factor(job_title),
         employee_residence = as.factor(employee_residence),
         remote_ratio = as.factor(remote_ratio)
         )
######################## World Map #############################
world_map <- function(df) {
  avg_salary <- df %>%
    # filter(job_title == job_name) %>%
    # filter(experience_level == exp_levels) %>%
    # filter(remote_ratio == remote) %>%
    mutate(employee_residence = countrycode(employee_residence, "country.name", "iso3c")) %>%
    group_by(employee_residence) %>%
    summarize(average_salary = mean(salary_in_usd, na.rm = TRUE))
  
  # Get world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  world_data <- world %>%
    left_join(avg_salary, by = c("iso_a3" = "employee_residence"))
  
  ggplot(world_data) +
    geom_sf(aes(fill = average_salary), color = "white", size = 0.2) +
    scale_fill_continuous(
      low = "lightblue", high = "darkblue",
      na.value = "grey50",  # Color for countries with no data
      name = "Average Salary"
    ) +
    labs(title = paste0("Average Salary of ", job_name) ) +
    theme_void()
}

################### Salary Prediction ##########################
salary_prediction <- function(model, level, title, residence, ratio){
  pre_new <- predict(model, data.frame(experience_level = level, job_title = title, employee_residence = residence, remote_ratio = ratio))
  return(pre_new)
}

######################## Box Plot #############################
box_plot <- function(data, column){
  ggplot(data, aes(x = !!as.symbol(column), y = salary_in_usd)) +
    geom_boxplot(color="darkblue", fill="lightblue", alpha=0.3) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0("Salary Distribution by ", column), 
         x = column, 
         y = "Salary in USD") 
}




