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
                choices = c(job_title)
                ),
        selectInput("exp_level", 
                    label = h3("Choose Experience Level"),
                    choices = list("Senior" = "SE",
                                   "Mid" = "MI",
                                   "Entry" = "EN",
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
                    choices = c(unique(cleaned_jobs$employee_residence))
                    ),
      ),
      
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Salary Distribution", plotOutput("dist_plot")),
          tabPanel("World Map", plotOutput("salary_plot")),
        ),
      )
      
    )
      
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ################## Text of Salary Prediction ############################

  
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
