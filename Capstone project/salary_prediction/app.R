#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("main.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Salary Prediction"),

    # Selection input, selec job titles 
    sidebarLayout(
      position = 'left',
      
      sidebarPanel(
        selectInput("job_title", 
                label = h3("Choose a Job Title"),
                choices = c(job_title)
                ),
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SaLary Distribution", plotOutput("salary_plot")),
        ),
      )
      
    )
      
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  get_title <- reactive({
    if (!is.null(input$job_title)) {
      cleaned_jobs = cleaned_jobs |> 
        filter(job_title %in% !!input$job_title) 
    }
  })


    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
