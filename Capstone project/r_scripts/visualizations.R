library(ggplot2)

salary_plot <- function(data) {
  ggplot(data, aes(x = salary_in_usd)) + 
    geom_histogram(binwidth = 5000, fill = "blue", color = "black") + 
    labs(title = "Salary Distribution", x = "Salary in USD", y = "Frequency")
}
quantile_plot <- function(data){  
  ggplot(data, aes(sample = salary_in_usd)) + 
    stat_qq() + 
    stat_qq_line() + 
    ggtitle("Normal Q-Q Plot of Salary") +
    xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles")
}

box_plot <- function(data){
  ggplot(data, aes(y = salary_in_usd)) + 
    geom_boxplot(fill = "lightblue", colour = "darkblue") + 
    labs(title = "Boxplot of Salaries", y = "Salary in USD")
}


plot_histograms <- function(data, column) {
  ggplot(cleaned_jobs, aes(x = !!as.symbol(column), y = salary_in_usd, fill = !!as.symbol(column))) + 
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Salary by Job Title", 
         x = column, 
         y = "Salary in USD")
  
}
