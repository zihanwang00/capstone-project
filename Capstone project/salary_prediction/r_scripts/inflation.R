################ Inflation Rate #####################

us_inflation_rates <- c('2019' = 0.0181, '2020' = 0.0123, '2021' = 0.0470, '2022' = 0.065)
global_inflation_rates <- c('2019' = 0.0219, '2020' = 0.0192, '2021' = 0.0350, '2022' = 0.088)

adjust_salary <- function(year, original_salary, currency) {
  if (year == 2023) {
    return(original_salary)
  }
  
  adjusted_salary <- original_salary
  for (y in as.character(year:(2023 - 1))) {
    inflation_rate <- ifelse(currency == 'USD', us_inflation_rates[y], global_inflation_rates[y])
    adjusted_salary <- adjusted_salary * (1 + inflation_rate)
  }
  
  return(adjusted_salary)
}

