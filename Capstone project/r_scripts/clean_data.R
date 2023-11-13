

clean_data <- function(jobs_df) {
  cleaned_df <- jobs_df %>%
    filter(employment_type == "FT") %>%
    mutate(job_title = str_to_lower(job_title)) %>%
    mutate(job_title = case_when(
      str_detect(job_title, "data") & (str_detect(job_title, "scientist") | str_detect(job_title, "science")) ~ "data scientist",
      (str_detect(job_title, "bi") | str_detect(job_title, "business intelligence")) ~ "business intelligence engineer",
      str_detect(job_title, "data") & (str_detect(job_title, "analyst") | str_detect(job_title, "analytics")) ~ "data analyst",
      str_detect(job_title, "data") & str_detect(job_title, "engineer") ~ "data engineer",
      str_detect(job_title, "computer vision") ~ "computer vision engineer",
      str_detect(job_title, "deep learning") | str_detect(job_title, "nlp") ~ "deep learning engineer",
      str_detect(job_title, "data") & str_detect(job_title, "architect") ~ "data architect",
      str_detect(job_title, "ai") ~ "AI programmer",
      str_detect(job_title, "machine learning") | str_detect(job_title, "ml") ~ "machine learning engineer",
      (str_detect(job_title, "research") | str_detect(job_title, "applied")) & str_detect(job_title, "scientist") ~ "research scientist",
      TRUE ~ job_title)) %>%
    group_by(job_title) %>%
    mutate(count = n()) %>%
    filter(count > 10) %>%
    ungroup() %>%
    mutate(
      experience_level = as.factor(experience_level),
      employment_type = as.factor(employment_type),
      employee_residence = as.factor(employee_residence),
      remote_ratio = as.factor(remote_ratio),
      job_title = as.factor(toTitleCase(job_title)),
      employee_residence = countrycode(employee_residence, "iso2c", "iso3c")
    ) %>%
    select(experience_level, 
           job_title, 
           salary_in_usd, 
           employee_residence, 
           remote_ratio,
           work_year, 
           salary_currency)
  
      
  return(cleaned_df)
}

