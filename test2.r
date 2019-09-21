library(shiny)
library(dplyr)
library(tidyr)
library(DBI)
library(DT)
library(readxl)

options(scipen =9999, digits=2, OutDec=",", encoding = 'ISO-8859-1')
## Only run examples in interactive R sessions

  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Fichero 1"),
        fileInput("file2", "Fichero 2"),
        width=3, style="color: #fff; background-color: #cb3234; border-color: #2e6da4"
        
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Generales",
          h3("Datos Generales"),
          h3(),
          tableOutput("globaltb"),
          h3()
          ),
          tabPanel("Controles",
          h3(),
          h3("Control1 - Numero de RUs"),
          h3(),
          tableOutput("globalct1"),
          h3(),
          h3("Control2 - Correlacion Mayorista"),
          h3(),
          tableOutput("globalct2")
          
        )
      )
    )
    ))
  
  server <- function(input, output) {
    fbase <- reactive( {
      read_excel(path = input$file1$datapath, skip = 4)
    })
    
    fmod <- reactive( {
      read_excel(path = input$file2$datapath, skip = 4)
    })
    
    totdata<- reactive ({
      totdata<- bind_rows(as.data.frame(fbase()),as.data.frame(fmod()))
      totdata<- as.data.frame(totdata)
    })
    
    output$globaltb <- renderTable({
      as.data.frame(totdata()) %>% group_by(ID_SIMULACION) %>% summarise(EC_RTU= sum(EC_DW_RTU_T, na.rm=T)/1000000, EC_RV=sum(REPORTING_V_T, na.rm=T)/1000000,EC_DT= sum(EC_DW_TOTAL_T, na.rm=T)/1000000) 
    })
    
    ###################################################
    # Control
    ###################################################
    
    ct1<- reactive({
      ct1<- totdata() %>% group_by(ID_SIMULACION, TIPO_RIESGO) %>% summarise(ru=n())
      
    })
    
    output$globalct1 <- renderTable({
      as.data.frame(ct1()) %>% spread(ID_SIMULACION, ru)     
    })
    
    
    ct2<- reactive({
      ct2<- totdata() %>% select(ID_SIMULACION, GEOGRAPHY2, EAD, CORR_MAY) %>%
        filter(CORR_MAY!=0) %>%
        group_by(ID_SIMULACION, GEOGRAPHY2) %>% 
        summarise(may_p = sum(EAD*CORR_MAY, na.rm=T)/(sum(EAD, na.rm=T)))
      
    })
    
    output$globalct2 <- renderTable({
      as.data.frame(ct2()) %>% spread(ID_SIMULACION, may_p)     
    })
    
    
  }
  
  shinyApp(ui, server)
