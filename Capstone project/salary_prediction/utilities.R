################ Utilities #######################
setwd("~/Documents/GitHub/capstone-project/Capstone project")

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


data <- read_csv("./cleaned_jobs.csv")
final_model <- readRDS("./final_model.rds")
d <- 0.3030303

data <- data %>%
  mutate(experience_level = as.factor(experience_level),
         job_title = as.factor(job_title),
         employee_residence = as.factor(employee_residence),
         remote_ratio = as.factor(remote_ratio)
         )

################### Salary Prediction ##########################
salary_prediction <- function(model, level, title, residence, ratio){
  pre_new <- predict(model, data.frame(experience_level = level, job_title = title, employee_residence = residence, remote_ratio = ratio))^(1/d)
  return(pre_new)
}

######################## World Map #############################
world_map <- function(data, level=NULL, title=NULL, residence=NULL, ratio=NULL) {
  
  # Dynamic filtering based on provided arguments
  if (!is.null(level)) {
    data <- data %>% filter(experience_level == level)
  }
  if (!is.null(title)) {
    data <- data %>% filter(job_title == title)
  }
  if (!is.null(residence)) {
    data <- data %>% filter(employee_residence == residence)
  }
  if (!is.null(ratio)) {
    data <- data %>% filter(remote_ratio == ratio)
  }
  
  avg_salary <- data %>%
    dplyr::mutate(employee_residence = countrycode(employee_residence, "country.name", "iso3c")) %>%
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
      na.value = "grey30",  # Color for countries with no data
      name = "Average Salary"
    ) +
    theme_void()
}



######################## Box Plot #############################
box_plot <- function(data, column, level=NULL, title=NULL, residence=NULL, ratio=NULL, name){
  # Dynamic filtering based on provided arguments
  if (!is.null(level)) {
    data <- data %>% filter(experience_level == level)
  }
  if (!is.null(title)) {
    data <- data %>% filter(job_title == title)
  }
  if (!is.null(residence)) {
    data <- data %>% filter(employee_residence == residence)
  }
  if (!is.null(ratio)) {
    data <- data %>% filter(remote_ratio == ratio)
  }
  
  ggplot(data, aes(x = !!as.symbol(column), y = salary_in_usd)) +
    geom_boxplot(color="darkblue", fill="lightblue", alpha=0.3) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0("Salary Distribution by ", name), 
         x = name, 
         y = "Salary in USD") 
}

################# Top Salary Plot #############################
top_salary_plot <- function(data, level=NULL, title=NULL, residence=NULL, ratio=NULL){
  # Dynamic filtering based on provided arguments
  if (!is.null(level)) {
    data <- data %>% filter(experience_level == level)
  }
  if (!is.null(title)) {
    data <- data %>% filter(job_title == title)
  }
  if (!is.null(residence)) {
    data <- data %>% filter(employee_residence == residence)
  }
  if (!is.null(ratio)) {
    data <- data %>% filter(remote_ratio == ratio)
  }
  
  # Aggregate data to find the average salary for each combination of factors
  aggregated_data <- data %>%
    group_by(experience_level, job_title, employee_residence, remote_ratio) %>%
    summarize(average_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(average_salary)) %>%
    slice_head(n = 3)  # Get the top 3 combinations
  
  # Create the plot
  ggplot(aggregated_data, 
         aes(x = interaction(experience_level, job_title, employee_residence, remote_ratio), y = average_salary)) +
    geom_col(fill = "lightblue") +
    geom_text(aes(label = round(average_salary, 2)), vjust = 1.5, size = 3.5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Top 3 Salary Combinations", 
         x = "",
         y = "Average Salary in USD")
}

# Example usage
top_salary_plot(data, title = "Data Scientist")
