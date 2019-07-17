######################################
# owl app - ec analysis
# version v1 - pruebas
######################################

library(shiny)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(lubridate)

options(scipen =9999, digits=2, OutDec=",", encoding = 'ISO-8859-1')


source("source.r")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
      EC1_UI("ec1"),
    
    # Show information
    mainPanel(
      EC2_UI("ec1"),
      width=9 
    )
   )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##################################################################################################
  # General data
  ##################################################################################################
  
  
  callModule(EC1, "ec1")
  callModule(EC2, "ec1", reactive({input$buvert}), reactive({input$year}), reactive({input$month}))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

