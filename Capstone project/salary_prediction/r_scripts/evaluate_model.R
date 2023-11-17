set.seed(123) 
split_index <- sample(1:nrow(cleaned_jobs), size = 0.9 * nrow(cleaned_jobs))
train_data <- cleaned_jobs[split_index, ]
test_data <- cleaned_jobs[-split_index, ]

model <- lm(salary_in_usd ~ experience_level * job_title + employee_residence + remote_ratio, data = train_data)
summary(model)
predictions <- predict(model, test_data)
rmse <- sqrt(mean((predictions - test_data$salary_in_usd)^2))
mae <- mean(abs(predictions - test_data$salary_in_usd))
