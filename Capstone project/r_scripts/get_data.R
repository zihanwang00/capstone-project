# data aggregation
job_skill_data <- function(job_skill) {
  job_skills <- job_skill %>%
    group_by(job_id) %>%
    summarise(skill_abr = paste(skill_abr, collapse = ', ')) %>%
    ungroup()
  return(job_skills)
}


get_data <- function(job_posts, level) {
  
  # job with companies and skills
  job_aggregated <- job_posts %>%
    filter(formatted_experience_level %in% level) %>%
    left_join(companies, by = "company_id") %>% 
    left_join(job_skill, by = "job_id") %>% 
    select(job_id,
           title,                 # job title
           description.x,         # job description
           location,              # job location
           original_listed_time,  # job listed time
           expiry,                # job expiration time
           name,                  # company name
           state,                 # company state
           country,               # company country
           skill_abr) %>%         # job skills
    na.omit()
  
  job_dated <- job_aggregated %>%
    mutate(
      original_listed_time = as.POSIXct(original_listed_time / 1000, origin = "1970-01-01"),
      expiry = as.POSIXct(expiry / 1000, origin = "1970-01-01")
    ) %>%
    # Now convert expiry and original_listed_time to Date format outside ifelse
    mutate(
      original_listed_time = as.Date(original_listed_time),
      expiry = as.Date(expiry)
    )
    
  
  return(job_dated)
}
