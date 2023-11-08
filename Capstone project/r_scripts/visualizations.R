library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(purrr)
library(tm)
library(wordcloud)
library(RColorBrewer)


get_concurrent_jobs <- function(d) {
  seq_dates <- seq(min(d$original_listed_time, na.rm = TRUE), max(d$original_listed_time, na.rm = TRUE), by = "day")

  concurrent_jobs <- tibble(date = seq_dates)

  for(title in unique(d$title)) {
    # Filter rows per title
    title_rows <- d %>% filter(title == !!title)
    
    concurrent_jobs[[title]] <- map_dbl(seq_dates, function(date) {
      sum(date >= title_rows$original_listed_time & date <= title_rows$expiry, na.rm = TRUE)
    })
  }
  
  concurrent_jobs_long <- concurrent_jobs %>%
    gather(key = "title", value = "count", -date)
  
  concurrent_jobs_long <- concurrent_jobs_long %>%
    mutate(count = if_else(title == "NA", as.integer(NA), count))
  
  return(concurrent_jobs_long)
}


line_graph <- function(data) {
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

}

word_cloud <- function(data){
  
  corpus <- Corpus(VectorSource(data$title))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  wordcloud(words = corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  
}


# job_cleaned %>%
#   group_by(date = date(original_listed_time), title) %>%
#   summarize(new_jobs = n(), .groups = 'drop') %>%
#   ggplot(aes(x = date, y = new_jobs, color = title)) + # Color lines by title
#   geom_line() +
#   xlab("Date") +
#   ylab("New Job Postings") +
#   theme_bw() +
#   theme(legend.position="bottom",
#         legend.text = element_text(size = 7), 
#         legend.title = element_text(size = 10)) 


# animated_plot <- job_cleaned %>%
#   get_concurrent_jobs() %>%
#   ggplot(aes(x = date, y = count, color = title, group = title)) + 
#   geom_line() +
#   xlab("Date") +
#   ylab("Concurrent Job Postings") +
#   theme_minimal() +
#   transition_reveal(date) + # Reveal the line over time
#   labs(title = 'Concurrent Job Postings: {frame_time}', subtitle = "Source: Job Postings Data") +
#   theme(legend.position="bottom")
# 
# animate(animated_plot, renderer = gifski_renderer(), height = 400, width = 700, duration = 10)
# 
# # Save the animation
# anim_save("animated_jobs_over_time.mp4")
