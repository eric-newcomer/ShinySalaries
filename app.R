## app.R ##
library(shiny)
library(shinythemes)
library(DT)
library(shinydashboard)
library(tidyverse)

salaries <- read.csv("college-salaries/degrees-that-pay-back.csv")

card <- function(.img, .species, .sepal.length) {
    HTML(
        paste0(
            '<div class="card">
                  <img src="', .img, '" style="width:100%">
                      <div class="container">
                          <h4><i>', .species, '</i></h4>
                          <hr>
                          <p>Sepal Length: ', .sepal.length, '</p>
                      </div>
              </div>')
    )
}

hi <- "https://www.plant-world-seeds.com/images/item_images/000/007/023/large_square/iris_baby_blue.jpg?1500653527"


ui <- fluidPage( 
    tags$head(tags$style('.card {
                         width: 250px;
                       clear: both;
                       /* Add shadows to create the "card" effect */
                       box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                       transition: 0.3s;
                       }
                       /* On mouse-over, add a deeper shadow */
                       .card:hover {
                       box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
                       }
                       /* Add some padding inside the card container */
                       .container {
                       width: 250px;
                       padding: 2px 16px;
                       }')),
    
    theme = shinytheme("flatly"),
    titlePanel("CareerSight"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Figure out which Majors pay you back."),
            
            selectInput("var", 
                        label = "Sort college majors by...",
                        choices = c("Starting Median Salary", 
                                    "Mid Career Median Salary",
                                    "Percent change from Starting to Mid Career Salary", 
                                    "Mid Career 10th Percentile Salary", 
                                    "Mid Career 25th Percentile Salary",
                                    "Mid Career 75th Percentile Salary", 
                                    "Mid Career 90th Percentile Salary"),
                        selected = "Starting Median Salary"),
            
            sliderInput("range", 
                        label = "Range of interest:",
                        min = 0, max = 100, value = c(0, 100))
        ),
        
        mainPanel(
            h2("Highest Paying College Majors by Category"),  
            textOutput("selected_var"),
            DT::dataTableOutput("mytable"),
        )
    ),
    uiOutput("cards")
)

server <- function(input, output) {
    output$selected_var <- renderText({ 
        paste("You are sorting by", input$var)
    })
    output$cards <- renderUI({
        
        # First make the cards
        args <- lapply(1:4, function(.x) card(hi,
                                              .species = iris[.x, "Species"],
                                              .sepal.length = iris[.x, "Sepal.Length"]))
        
        # Make sure to add other arguments to the list:
        args$cellArgs <- list(
            style = "
        width: auto;
        height: auto;
        margin: 5px;
        ")
        
        # basically the same as flowLayout(cards[[1]], cards[[2]],...)
        do.call(shiny::flowLayout, args)
        
    })
    output$mytable = DT::renderDataTable({
        salaries
    })
    
}

shinyApp(ui, server)
