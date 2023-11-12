linear_model <- function(data, y) {
  lm_model <- glm(salary_in_usd ~ ., data = cleaned_jobs)
  summary(lm_model)
}