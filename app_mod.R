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
library(openxlsx)
library(readxl)

options(scipen =9999, digits=2, OutDec=",", encoding = 'ISO-8859-1')


source("source_mod.r")
source("source_mod2.r")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel(div(img(src="descarga.jpg",height=50), img(src="logo.png",height=50))),
  
  # Options to select information
  navbarPage("OWL-EC Control",
      tabPanel("Economic Capital",       
  sidebarLayout(
      EC1_UI("ec1"),
    
    # Show information
    mainPanel(
      EC2_UI("ec1"),
      width=9 
    )
   )
  ),
  tabPanel("EC Base"
  ),
  tabPanel("Modo Simul",
        sidebarLayout(   
           EC3_UI("ec2"),
        
        mainPanel(
           EC4_UI("ec2")
        )   
  )   
           
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
  
  callModule(EC4, "ec2", reactive({input$file1$datapath}), reactive({input$file2$datapath}))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

