#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# source("main.R")
source("utilities.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Salary Prediction"),

    # Selection input, select job titles 
    sidebarLayout(
      position = 'left',
      
      sidebarPanel(
        selectInput("job_title", 
                label = h3("Choose a Job Title"),
                choices = c(unique(data$job_title))
                ),
        selectInput("exp_level", 
                    label = h3("Choose Experience Level"),
                    choices = list("Entry" = "EN",
                                   "Mid" = "MI",
                                   "Senior" = "SE",
                                   "Executive" = "EX")
                    ),
        selectInput("remote_rate", 
                    label = h3("Choose Remote Ratio"),
                    choices = list("On-Site" = "0",
                                   "Hybrid" = "50",
                                   "Remote" = "100")
                    ),
        selectInput("emp_residence", 
                    label = h3("Choose Residence of Employement"),
                    choices = c(unique(data$employee_residence))
                    ),
      ),
      
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Salary Prediction", htmlOutput("salary_pred"),
                   tags$head(tags$style("#salary_pred{color: blue;
                                 font-size: 25px;
                                 }")
                              )),
          tabPanel("Salary Distribution", plotOutput("dist_plot")),
          tabPanel("World Map", plotOutput("salary_plot")),
        ),
      )
      
    )
      
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ################## Text of Salary Prediction ############################
  output$salary_pred <- renderUI({
    input_title <- input$job_title
    input_level <- input$exp_level
    input_remote <- input$remote_rate
    input_resid <- input$emp_residence
    predicted_salary <- round(salary_prediction(linear, input_level, input_title, input_resid, input_remote))
    str0 = paste("Job Title:", input_title, "\n\n\n")
    str1 = paste("Experience Level:", input_level, "\n\n\n")
    str2 = paste("Country of Residence:", input_resid, "\n\n\n")
    str3 = paste0("The Predicted Salary is $", predicted_salary, " US dollars.", "\n\n\n")
    HTML(paste(str0, str1, str2, str3, sep = '<br/>'))
    
  })
  
  
  ################## Salary Distribution, Box Plot ########################
  output$dist_plot <- renderPlot({
    data %>%
      filter(job_title == input$job_title) %>%
      filter(experience_level == input$exp_level) %>%
      filter(remote_ratio == input$remote_rate) %>%
      salary_dist_plot()
  })  
  
  ################## World Map ############################################
    output$salary_plot <- renderPlot({
      world_map(input$job_title, input$exp_level, input$remote_rate)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
