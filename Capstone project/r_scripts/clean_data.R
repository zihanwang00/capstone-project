## Data Cleaning
title_cleaning  = function(df){
  job_titled <- entry_job %>%
    distinct(title,                 # job title
             description.x,         # job description
             location,              # job location
             original_listed_time,  # job listed time
             expiry,                # job expiration time
             name,                  # company name
             state,                 # company state
             country,               # company country
             skill_abr) %>%         # job skills
    mutate(title = str_to_lower(title), # Convert title to lowercase
           title = if_else(str_detect(title, "sales") & (str_detect(title, "representative")),
                           "sales representative", 
                   if_else(str_detect(title, "sales") & str_detect(title, "associate"), "sales associate", 
                   if_else(str_detect(title, "accountant"), "accountant", 
                   if_else(str_detect(title, "maintenance") & str_detect(title, "technician"),
                           "maintenance technician",
                   if_else(str_detect(title, "data") & 
                             (str_detect(title, "scientist") | str_detect(title, "analyst") | str_detect(title, "engineer")),
                           "data scientist/analyst/engineer",
                   if_else(str_detect(title, "scientist"),  "scientist",
                   if_else(str_detect(title, "analyst"), "analyst",
                   if_else(str_detect(title, "product") & str_detect(title, "manager"),
                           "product manager",
                   if_else((str_detect(title, "project") | str_detect(title, "program")) & str_detect(title, "manager"),
                           "project manager",
                   if_else(str_detect(title, "store") & str_detect(title, "manager"), "store manager",
                   if_else(str_detect(title, "account") & str_detect(title, "manager"), "account manager",
                   if_else(str_detect(title, "software") & str_detect(title, "engineer"), "software engineer",
                   if_else(str_detect(title, "cook"), "cook",
                   if_else(str_detect(title, "cashier"), "cashier",
                   if_else(str_detect(title, "server"), "server",
                           title)
                   ))))))))))))))) %>%
    # filter(str_detect(title, regex("server", ignore_case = TRUE))
    # ) %>% # Filter titles with 'analyst', case insensitive
    # count(title) %>% # Count the number of occurrences for each title
    # arrange(desc(n))
    #   
    group_by(title) %>%
    summarize(n = n()) %>%
    filter(n > 8) %>%
    arrange(desc(n)) 
  
  
  
  return(job_titled)
}

