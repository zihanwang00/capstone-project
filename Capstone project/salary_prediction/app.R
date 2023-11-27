#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Salary Prediction: A 'Predict' button that, when clicked, displays the estimated salary based on the input.
# Graphs and charts showing salary distributions across different industries, job titles, experience levels, and locations.

#' compare salaries across different job titles, industries, or locations.
#' Select Factor to compare --> boxplots / histograms of average salary
library(shiny)
source("utilities.R")

ui <- fluidPage(
  # Application title
  titlePanel("Data Science Jobs Salary Prediction"),
  
  # Main panel for tabset
  mainPanel(
    tabsetPanel(
      id = "mainTabset",
      type = "tabs",
      tabPanel("Salary Prediction", 
               fluidRow(
                 column(3, 
                        selectInput("job_title", 
                                    label = h3("Job Title"),
                                    choices = c(unique(data$job_title))
                        )
                 ),
                 column(2, 
                        selectInput("remote_rate", 
                                    label = h3("Work Model"),
                                    choices = list("On-Site" = "0",
                                                   "Hybrid" = "50",
                                                   "Remote" = "100")
                        )
                 ),
                 column(3, 
                        selectInput("exp_level", 
                                    label = h3("Seniority Level"),
                                    choices = list("Entry" = "EN",
                                                   "Mid" = "MI",
                                                   "Senior" = "SE",
                                                   "Executive" = "EX")
                        )
                 ),
                 column(4, 
                        selectInput("emp_residence", 
                                    label = h3("Employee Residence"),
                                    choices = c(unique(data$employee_residence))
                        )
                 )
               ),
               tags$head(tags$style(HTML("
                                 #salary_pred {
                                     color: darkblue;
                                     font-size: 25px;
                                     text-align: center; /* Center text horizontally */
                                     margin-top: 20px; /* Optional: add some space at the top */
                                 }
                                 .shiny-output-error { /* Hide errors initially */
                                     visibility: hidden;
                                 }
                                 .shiny-output-error:before { /* Custom error message */
                                     visibility: visible;
                                     content: 'Error: could not render the prediction.';
                                 }
                             "))),
               htmlOutput("salary_pred", style = "text-align: center;")
      ),
      tabPanel("Explore Job Titles", 
               fluidRow(
               column(10, 
                     selectInput("job_title2", 
                                 label = h3("Select Job Title to Explore"),
                                 choices = c(unique(data$job_title))
                     )
               )), 
               plotOutput("titleCombinedPlot")),
      tabPanel("Explore Seniority Levels", 
               fluidRow(
                 column(10, 
                        selectInput("level2", 
                                    label = h3("Select Seniority Level to Explore"),
                                    choices = list("Entry" = "EN",
                                                   "Mid" = "MI",
                                                   "Senior" = "SE",
                                                   "Executive" = "EX")
                        )
                 )), 
               plotOutput("levelCombinedPlot")),
      tabPanel("Explore Work Models", 
               fluidRow(
                 column(10, 
                        selectInput("ratio2", 
                                    label = h3("Select Work Model to Explore"),
                                    choices = list("On-Site" = "0",
                                                   "Hybrid" = "50",
                                                   "Remote" = "100")
                        )
                 )), 
               plotOutput("remoteCombinedPlot")),
      tabPanel("Explore Countries", 
               fluidRow(
                 column(10, 
                        selectInput("country2", 
                                    label = h3("Select Country to Explore"),
                                    choices = c(unique(data$employee_residence))
                        )
                 )), 
               plotOutput("countryCombinedPlot")),
      # tabPanel("Salary Distribution", plotOutput("dist_plot")),
      # tabPanel("World Map", plotOutput("salary_plot"))
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ################## Text of Salary Prediction ############################
  output$salary_pred <- renderUI({
    input_title <- input$job_title
    input_level <- ifelse(input$exp_level == "EN", "Entry Level",
                          ifelse(input$exp_level == "MI", "Mid Level",
                                 ifelse(input$exp_level == "SE", "Senior Level", "Excuetive Level")))
    input_remote <- ifelse(input$remote_rate == "0", "On-Site", 
                           ifelse(input$remote_rate == "50", "Hybrid", "Fully Remote"))
    input_resid <- input$emp_residence
    predicted_salary <- round(salary_prediction(final_model, input$exp_level, 
                                                input$job_title, input$emp_residence, input$remote_rate))
    
    
    str0 = paste("Job Title:", input_title, "\n\n\n")
    str1 = paste("Work Model:", input_remote, "\n\n\n")
    str2 = paste("Experience Level:", input_level, "\n\n\n")
    str3 = paste("Country of Employment:", input_resid, "\n\n\n")
    str4 = paste0("The Predicted Average Salary is $", predicted_salary, " US dollars.", "\n\n\n")
    HTML(paste(str0, str1, str2, str3, str4, sep = '<br/>'))
    
  })
  
  
  ############### Title Combined Plot ############################################
  output$titleCombinedPlot <- renderPlot({
    p1 <- box_plot(data , "remote_ratio", title = input$job_title2, name = "Remote Ratio")
    p2 <- box_plot(data, "experience_level", title = input$job_title2, name = "Experience Level")
    p3 <- world_map(data, title = input$job_title2)
    p4 <- top_salary_plot(data, title = input$job_title2)
    
    # Combine the plots
    combinedPlot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
    print(combinedPlot)
  },
  width = 980,
  height = 600
  )
  
  ############### Level Combined Plot ####################################
  output$levelCombinedPlot <- renderPlot({
    p1 <- box_plot(data , "remote_ratio", level = input$level2, name = "Remote Ratio")
    p2 <- box_plot(data, "job_title", level = input$level2, name = "Job Title")
    p3 <- world_map(data, level = input$level2)
    p4 <- top_salary_plot(data, level = input$level2)
    
    # Combine the plots
    combinedPlot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
    print(combinedPlot)
  },
  width = 980,
  height = 600
  )
  
  ############### Remote Combined Plot ####################################
  output$remoteCombinedPlot <- renderPlot({
    p1 <- box_plot(data , "experience_level", ratio = input$ratio2, name = "Experience Level")
    p2 <- box_plot(data, "job_title", ratio = input$ratio2, name = "Job Title")
    p3 <- world_map(data, ratio = input$ratio2)
    p4 <- top_salary_plot(data, ratio = input$ratio2)
    
    # Combine the plots
    combinedPlot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
    print(combinedPlot)
  },
  width = 980,
  height = 600
  )
  
  ############### Country Combined Plot ####################################
  output$countryCombinedPlot <- renderPlot({
    p1 <- box_plot(data , "experience_level", ratio = input$ratio2, name = "Experience Level")
    p2 <- box_plot(data , "remote_ratio", level = input$level2, name = "Remote Ratio")
    p3 <- box_plot(data, "job_title", ratio = input$ratio2, name = "Job Title")
    p4 <- top_salary_plot(data, ratio = input$ratio2)
    
    # Combine the plots
    combinedPlot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
    print(combinedPlot)
  },
  width = 980,
  height = 600
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
