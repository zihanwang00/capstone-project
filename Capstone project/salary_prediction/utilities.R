################ Utilities #######################

library(readr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

data <- read_csv("cleaned_jobs.csv")

world_map <- function(job_name, exp_levels, remote) {
  avg_salary <- data %>%
    filter(job_title == job_name) %>%
    filter(experience_level == exp_levels) %>%
    filter(remote_ratio == remote) %>%
    group_by(employee_residence) %>%
    summarize(average_salary = mean(salary_in_usd / 1000, na.rm = TRUE))
  
  # Get world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  world_data <- world %>%
    left_join(avg_salary, by = c("iso_a3" = "employee_residence"))
  
  ggplot(world_data) +
    geom_sf(aes(fill = average_salary), color = "white", size = 0.2) +
    scale_fill_continuous(
      low = "lightblue", high = "darkblue",
      na.value = "grey50",  # Color for countries with no data
      name = "Average Salary in thousands"
    ) +
    labs(title = paste0("Average Salary of ", job_name) ) +
    theme_void()
}

salary_dist_plot <- function(data) {
  ggplot(data, aes(x = salary_in_usd)) + 
    geom_histogram(binwidth = 5000, fill = "blue", color = "black") + 
    labs(title = "Salary Distribution", x = "Salary in USD", y = "Frequency")
}