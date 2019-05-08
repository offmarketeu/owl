######################################
# owl app - ec analysis
# version v1
######################################

library(shiny)
library(dplyr)
library(DBI)
library(DT)
options(scipen =9999, digits=2, OutDec=",", encoding = 'ISO-8859-1')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  # Application title
  titlePanel("EC ANALISYS"),
  
  # Options to select information
  sidebarLayout(
    sidebarPanel(
      selectInput("buvert","Unit: ", choices=c("Corporate Activities", "Argentina", "Brazil", "Chile", "Colombia", "Other Europe and Asia", "Spain")),
      selectInput("year","Year: ", c("2018","2019")),
      selectInput("month","Month: ", c("01","02","03","04","05","06","07","08","09","10","11","12")),
      uiOutput("Prueba")
    ),
    
    # Show information
    mainPanel(
      tabsetPanel(
        tabPanel("Activos Materiales",
                 h3(),
                 h3(textOutput("bu_am")),
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("am"),
                 h3(),
                 h3("Capital - Promedio Historico"),
                 h3(),
                 tableOutput("am_h"),
                 h3(),
                 h3("Saldos"),
                 h3(),
                 tableOutput("tec")),
        ##############################################################
        tabPanel("Activos Intangibles",
                 h3(),
                 h3(textOutput("bu_int")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("int"),
                 h3(),
                 h3("Saldos"),
                 h3(),
                 tableOutput("tec1"),
                 h3(),
                 tableOutput("hist")),
        ##############################################################
        tabPanel("ALM",
                 h3(),
                 h3(textOutput("bu_alm")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("alm"),
                 h3(),
                 h3(textOutput("Criteria")),
                 tableOutput("tempo_alm")
                 )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # General data
  con <- dbConnect(RSQLite::SQLite(), "C:/Users/n040485/Documents/owl/owl.db")
  
  fcal<- reactive({
            paste(input$year,"-",input$month, sep="")
         })
  
  
  fhist<- reactive({
    mes=c("01","02","03","04","05","06")
    sm <- "2018-12"
    for (i in mes) {
      if (i< input$month) {
        ff <- paste(input$year,i, sep="-")
        sm <- paste(sm, ff, sep="|")
      } 
    }  
  })
  
  #output$fhisto<- fhist()
  
  ####################################################
  # update select Unit
  ####################################################
  
  tempo_amk<- reactive({res<-dbSendQuery(con, paste0("SELECT BU_VERT FROM ADDON GROUP BY BU_VERT", sep=""))
  result<-dbFetch(res)
  tempo_amk<- unique(result$BU_VERT)
  })
  
  output$Prueba<- renderUI({ selectInput("buvertk", "Unit:", choices=tempo_amk())})
  
  ####################################################
  # Activos Materiales
  # Capital
  ####################################################
  
  output$bu_am<- reactive({input$buvert})
  output$am<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal(), "' AND TIPO_RIESGO='Material Assets' GROUP BY BU_VERT", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
  })
  
  ####################################################
  # Activos Materiales
  # Capital - historico
  ####################################################
  
  #output$am_h<- renderTable({
  #  res<-dbSendQuery(con, paste0("SELECT EC_DW_RTU,  REPORTING_V, EC_DW_TOTAL FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha LIKE '%", finit() ,"%' AND TIPO_RIESGO='Activos Materiales' GROUP BY BU_VERT", sep=""))
  #  result<-dbFetch(res)
  
  #})
  
  
  
  ####################################################
  # Saldos
  ####################################################
  tempo_am<- reactive({res<-dbSendQuery(con, paste0("SELECT RU_ID FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal(), "' AND TIPO_RIESGO='Material Assets' GROUP BY BU_VERT,fecha", sep=""))
  result<-dbFetch(res)
  format(result, big.mark=".")
  tempo_am<- as.data.frame(result)
  })
  
  
  
  tempo1_am<- reactive({res<-dbSendQuery(con, paste0("SELECT U_CdG, Importe_2, COD_DIV1, RU_ID_ACTIVOS_MATERIALES FROM PYG WHERE EPIGRAFE='101104' OR EPIGRAFE='1010901' AND fecha='" , fcal() , "'", sep=""))
  result1<-dbFetch(res)
  format(result1, big.mark=".")
  tempo1_am<- as.data.frame(result1)
  
  })
  
  #output$tec<- renderTable({left_join(tempo_am(), tempo1_am(), by=c("RU_ID"="RU_ID_ACTIVOS_MATERIALES"))})
  
  output$tec<- renderTable({res<-dbSendQuery(con, paste0("SELECT PYG.U_CdG, PYG.EPIGRAFE,sum(PYG.Importe_2), PYG.COD_DIV1, PYG.RU_ID_ACTIVOS_MATERIALES FROM PYG,ADDON WHERE PYG.EPIGRAFE IN('101104','1010901') AND PYG.fecha='" , fcal() , "' AND ADDON.BU_VERT='", input$buvert ,"' AND PYG.RU_ID_ACTIVOS_MATERIALES=ADDON.RU_ID GROUP BY PYG.U_CdG,PYG.EPIGRAFE,PYG.COD_DIV1, PYG.RU_ID_ACTIVOS_INTANGIBLES" , sep=""))
  result2<-dbFetch(res)
  format(result2, big.mark=".")
  })
  
  ####################################################
  # Activos intangibles
  # Capital
  ####################################################
  
  output$bu_int<- reactive({input$buvert})
  output$int<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT SUM(EC_DW_RTU), SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Intangible assets' GROUP BY BU_VERT", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  ####################################################
  # Saldos
  ####################################################
  
  
  tempo_int<- reactive({res<-dbSendQuery(con, paste0("SELECT RU_ID FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal(), "' AND TIPO_RIESGO='Intangible assets' GROUP BY BU_VERT,fecha", sep=""))
  result<-dbFetch(res)
  tempo_int<- as.data.frame(result)
  format(result, big.mark=".")
  })

  
  tempo1_int<- reactive({res<-dbSendQuery(con, paste0("SELECT U_CdG, Importe_2, COD_DIV1, RU_ID_ACTIVOS_INTANGIBLES FROM PYG WHERE EPIGRAFE='907' OR EPIGRAFE='1010902' AND fecha='" , fcal() , "'", sep=""))
  result1<-dbFetch(res)
  format(result1, big.mark=".")
  tempo1_int<- as.data.frame(result1)
  })
  
  output$tec1<- renderTable({res<-dbSendQuery(con, paste0("SELECT PYG.U_CdG, PYG.EPIGRAFE,sum(PYG.Importe_2), PYG.COD_DIV1, PYG.RU_ID_ACTIVOS_INTANGIBLES FROM PYG,ADDON WHERE PYG.EPIGRAFE IN('907','1010902') AND PYG.fecha='" , fcal() , "' AND ADDON.BU_VERT='", input$buvert ,"' AND PYG.RU_ID_ACTIVOS_INTANGIBLES=ADDON.RU_ID GROUP BY PYG.U_CdG,PYG.EPIGRAFE,PYG.COD_DIV1, PYG.RU_ID_ACTIVOS_INTANGIBLES" , sep=""))
  result2<-dbFetch(res)
  format(result2, big.mark=".", align="right")
  
  })
  
  
  
  #output$tec1<- renderTable({left_join(tempo_int(), tempo2_int(), by=c("RU_ID"="RU_ID_ACTIVOS_INTANGIBLES"))})
  
  ####################################################
  # ALM
  # Capital
  ####################################################
  
  output$bu_alm<- reactive({input$buvert})
  
  output$alm<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT SUM(EC_DW_RTU), SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='ALM' GROUP BY BU_VERT", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  
  ####################################################
  # RU vs Criteria
  ####################################################
  
  output$tempo_alm<- renderTable({
  res<-dbSendQuery(con, paste0("SELECT Criterias.CRITERIA FROM Criterias, ADDON WHERE ADDON.BU_VERT='", input$buvert , "' AND ADDON.RU_ID=Criterias.RU_ID AND ADDON.TIPO_RIESGO='ALM' GROUP BY Criterias.CRITERIA", sep=""))
  result<-dbFetch(res)
  tempo_alm<- as.data.frame(result)
  format(result, big.mark=".")
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

