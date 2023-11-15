linear_model <- function(df) {
  lm_model <- glm(salary_in_usd ~ ., data = df)
  return(lm_model)
}

salary_prediction <- function(m, level, title, residence, ratio){
  pre_new <- predict(model, data.frame(experience_level = level, job_title = title, employee_residence = residence, remote_ratio = ratio))
  return(pre_new)
}