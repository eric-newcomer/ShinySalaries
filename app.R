## app.R ##
library(shiny)
library(shinythemes)
library(DT)
library(shinydashboard)
library(tidyverse)

ui <- fluidPage( 
  
    theme = shinytheme("flatly"),
    titlePanel("CareerSight"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Figure out which Majors pay you back."),
            
            selectInput("var", 
                        label = "Sort college majors by...",
                        choices = c("Starting.Median.Salary", 
                                    "Mid.Career.Median.Salary",
                                    "Percent.change.from.Starting.to.Mid.Career.Salary", 
                                    "Mid.Career.10th.Percentile.Salary", 
                                    "Mid.Career.25th.Percentile.Salary",
                                    "Mid.Career.75th.Percentile.Salary", 
                                    "Mid.Career.90th.Percentile.Salary"),
                        selected = "Starting Median Salary"),
        ),
        
        mainPanel(
            h2("Highest Paying College Majors by Category"),  
            textOutput("selected_var"),
            tableOutput("tbl"),
        )
    ),
)

server <- function(input, output) {
    salaries <- read.csv("college-salaries/degrees-that-pay-back.csv")
    
    salTable <- reactive({
        salaries %>% 
            select(Undergraduate.Major, input$var) %>%
            group_by_(input$var) %>%
            arrange_(lazyeval::interp(~desc(var),
                                      var = as.name(input$var)))
    })
    
    output$selected_var <- renderText({ 
        paste("You are sorting by", input$var)
    })
    
    output$tbl <- renderTable({salTable()})
}

shinyApp(ui, server)
