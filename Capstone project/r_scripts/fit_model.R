linear_model <- function(df) {
  lm_model <- glm(salary_in_usd ~ ., data = df)
  return(lm_model)
}

salary_prediction <- function(m, level, title, turn_over, ratio){
  pre_new <- predict(model, data.frame(experience_level = level, job_title = title, employee_residence = residence, remote_ratio = ratio))
}