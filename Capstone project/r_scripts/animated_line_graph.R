library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyr)
library(tidyverse)
library(purrr)



get_concurrent_jobs <- function(d) {
  # Prepare a sequence of dates from the earliest listed to the latest expiry
  seq_dates <- seq(min(d$original_listed_time, na.rm = TRUE), as.Date("2023-10-31"), by = "day")
  
  # Initialize an empty data frame to store the counts per date per title
  concurrent_jobs <- tibble(date = seq_dates)
  
  # Count jobs for each title on each date
  for(title in unique(d$title)) {
    # Filter rows per title
    title_rows <- d %>% filter(title == !!title)
    
    # Add a count column for each date considering the current title's date ranges
    concurrent_jobs[[title]] <- map_dbl(seq_dates, function(date) {
      sum(date >= title_rows$original_listed_time & date <= title_rows$expiry, na.rm = TRUE)
    })
  }
  
  # Gather the counts per title into a long format suitable for plotting
  concurrent_jobs_long <- concurrent_jobs %>%
    gather(key = "title", value = "count", -date)
  
  # Replace "NA" with actual NA values to remove them from the plot
  concurrent_jobs_long <- concurrent_jobs_long %>%
    mutate(count = if_else(title == "NA", as.integer(NA), count))
  
  return(concurrent_jobs_long)
}

# Usage of the function assuming 'your_table' contains your job postings with 'title' column
job_cleaned %>%
  select(title,
         original_listed_time,
         expiry) %>%
  get_concurrent_jobs() %>%
  ggplot(aes(x = date, y = count, color = title)) + # Color lines by title
  geom_line() +
  xlab("Date") +
  ylab("Concurrent Job Postings") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 10)) 





animated_plot <- job_data %>%
  get_concurrent_jobs() %>%
  ggplot(aes(x = date, y = count, color = title, group = title)) + # group is important for gganimate
  geom_line() +
  xlab("Date") +
  ylab("Concurrent Job Postings") +
  theme_minimal() +
  transition_reveal(date) + # Reveal the line over time
  labs(title = 'Concurrent Job Postings: {frame_time}', subtitle = "Source: Job Postings Data") +
  theme(legend.position="bottom")


# animate(animated_plot, renderer = gifski_renderer(), height = 400, width = 700, duration = 10)
