

clean_data <- function(jobs_df) {
  cleaned_df <- jobs_df %>%
    select(experience_level,
           employment_type,
           job_title,
           salary_in_usd,
           employee_residence,
           remote_ratio) %>%
    filter(
      employment_type == "FT"
    ) %>%
    mutate(
      experience_level = as.factor(experience_level),
      employment_type = as.factor(employment_type), 
      job_title = str_to_lower(job_title), 
      job_title = if_else(str_detect(job_title, "data") & (str_detect(job_title, "scientist") | str_detect(job_title, "science")), "data scientist",
                     if_else(str_detect(job_title, "data") & (str_detect(job_title, "analyst")| str_detect(job_title, "analytics")), "data analyst",
                     if_else(str_detect(job_title, "data") & str_detect(job_title, "engineer") , "data engineer",
                     if_else(str_detect(job_title, "data") & str_detect(job_title, "architect") , "data architect",
                     if_else(str_detect(job_title, "ai"), "AI programmer",
                     if_else(str_detect(job_title, "machine learning") | str_detect(job_title, "ml"),  "machine learning engineer",
                     if_else((str_detect(job_title, "research") | str_detect(job_title, "applied")) & str_detect(job_title, "scientist"), "research scientist",                                                       job_title))))))), 
      job_title = as.factor(toTitleCase(job_title))
      )
  return(cleaned_df)
}

