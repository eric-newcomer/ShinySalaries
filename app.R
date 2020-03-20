## app.R ##
library(shiny)
library(shinythemes)
library(DT)
library(shinydashboard)
library(tidyverse)
library(shinyjs)

# DATA
salaries <- read.csv("college-salaries/degrees-that-pay-back.csv")

ui <- dashboardPage(
  dashboardHeader(title = "CareerSight"),
  dashboardSidebar(
    # Remove the sidebar toggle element
    tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
    sidebarMenu(
      menuItem("At a glance...", tabName = "glance", icon = icon("glasses")),
      menuItem("Focusing in...", tabName = "focus", icon = icon("microscope"))
    )
  ),
  dashboardBody(
    tabItems(
      # FIRST TAB CONTENT
      tabItem(tabName = "glance",
              fluidPage( 
                
                theme = shinytheme("flatly"),
                titlePanel("Viewing Majors at a Glance"),
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("Majors sorted by highest to lowest salary."),
                    
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
            ),
      ),
        
      # SECOND TAB CONTENT
      tabItem(tabName = "focus",
              fluidPage( 
                theme = shinytheme("flatly"),
                titlePanel("Focus on a Specific Major"),
                sidebarLayout(
                  sidebarPanel(
                    helpText("View the statistics of a major of your choice."),
                    
                    selectInput("major", 
                                label = "Select a major:",
                                choices = salaries$Undergraduate.Major,
                                selected = salaries$Undergraduate.Major[1]),
                  ),
                  
                  mainPanel(
                    h2(textOutput("selected_major")),
                    br(),
                    h4(textOutput("major_start_med_sal")),
                    h4(textOutput("major_mid_med_sal")),
                    h4(textOutput("percent_change")),
                    h4(textOutput("mid_10")),
                    h4(textOutput("mid_25")),
                    h4(textOutput("mid_75")),
                    h4(textOutput("mid_90")),
                  )
                ),
              ),
      )
    )
  ),
)


server <- function(input, output) {
    
    # FIRST TAB -----------------------------------------------------
    # Make dynamic table for salary information
    salTable <- reactive({
        salaries %>% 
            select(Undergraduate.Major, input$var) %>%
            group_by_(input$var) %>%
            arrange_(lazyeval::interp(~desc(var),
                                      var = as.name(input$var)))
    })
    
    output$var <- renderText({ 
        paste("You are sorting by ", input$var)
    })
    
    output$tbl <- renderTable({salTable()})
    
    # SECOND TAB -----------------------------------------------------
    
    # Dynamic table for salary info
    majorTable <- reactive({
      salaries %>% 
        filter(Undergraduate.Major == input$major)
    })
    
    output$selected_major <- renderText({ 
      paste("Major: ", input$major)
    })
    output$major_start_med_sal <- renderText({ 
      paste("Starting Median Salary: ", majorTable()$Starting.Median.Salary)
    })
    output$major_mid_med_sal <- renderText({ 
      paste("Starting Median Salary: ", majorTable()$Mid.Career.Median.Salary)
    })
    output$percent_change <- renderText({ 
      paste("Percent change from Starting to Mid Career Salary: ", majorTable()$Percent.change.from.Starting.to.Mid.Career.Salary, "%")
    })
    output$mid_10 <- renderText({ 
      paste("Mid Career 10th Percentile Salary: ", majorTable()$Mid.Career.10th.Percentile.Salary)
    })
    output$mid_25 <- renderText({ 
      paste("Mid Career 25th Percentile Salary: ", majorTable()$Mid.Career.25th.Percentile.Salary)
    })
    output$mid_75 <- renderText({ 
      paste("Mid Career 75th Percentile Salary: ", majorTable()$Mid.Career.75th.Percentile.Salary)
    })
    output$mid_90 <- renderText({ 
      paste("Mid Career 90th Percentile Salary: ", majorTable()$Mid.Career.90th.Percentile.Salary)
    })
    
}

shinyApp(ui, server)
