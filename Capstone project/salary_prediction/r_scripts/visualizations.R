library(ggplot2)

salary_plot <- function(data) {
  ggplot(data, aes(x = salary_in_usd)) + 
    geom_histogram(binwidth = 5000, fill = "blue", color = "black") + 
    labs(title = "Salary Distribution", x = "Salary in USD", y = "Frequency")
}

quantile_plot <- function(data){  
  ggplot(cleaned_jobs, aes(sample = salary_in_usd^trans_num)) + 
    stat_qq() + 
    stat_qq_line() + 
    ggtitle("Normal Q-Q Plot of Salary") +
    xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles")
}

box_plot <- function(data, column){
  ggplot(data, aes(x = !!as.symbol(column), y = salary_in_usd)) +
    geom_boxplot(color="darkblue", fill="lightblue", alpha=0.3) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0("Salary Distribution by ", column), 
         x = column, 
         y = "Salary in USD") 
}


plot_histograms <- function(data, column) {
  ggplot(data, aes(x = !!as.symbol(column), y = salary_in_usd, fill = !!as.symbol(column))) + 
    geom_histogram() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste0("Salary Distribution by ", column), 
         x = column, 
         y = "Salary in USD")
  
}

ggplot(cleaned_jobs, aes(x = !!as.symbol(column), y = salary_in_usd, fill = !!as.symbol(column))) + 
  geom_histogram() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = paste0("Salary Distribution by ", column), 
       x = column, 
       y = "Salary in USD")


salary_dist_plot <- function(data) {
  ggplot(data, aes(x = salary_in_usd)) + 
    geom_histogram(binwidth = 5000, fill = "lightblue", color = "black") + 
    labs(title = "Salary Distribution", x = "Salary in USD", y = "Frequency")
}
