######################################
# owl app - ec analysis
# version v1
######################################

library(shiny)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(lubridate)

options(scipen =9999, digits=2, OutDec=",", encoding = 'ISO-8859-1')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  headerPanel(
    list(img(src="descarga.jpg",height=50),
    img(src="logo.png",height=50))
  ),
  titlePanel("OWL-EC Control"),
  
  
  # Options to select information
  sidebarLayout(
    sidebarPanel(
      uiOutput("Prueba"),
      #selectInput("buvert","Unit: ", choices=c("Corporate Activities", "Argentina", "Brazil", "Chile", "Colombia", "Other Europe and Asia", "Spain")),
      selectInput("year","Year: ", c("2018","2019"), "2019"),
      selectInput("month","Month: ", c("01","02","03","04","05","06","07","08","09","10","11","12")),
      fileInput("db", "DataBase:"),
      width=3
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
                 h3("Capital - Historico"),
                 h3(),
                 tableOutput("am_h"),
                 h3(),
                 actionButton("ma_avg", "Media"),
                 h3(),
                 tableOutput("am_avg"),
                 h3(),
                 h3("Saldos"),
                 h3(),
                 tableOutput("tec")
                 ),
        ##############################################################
        tabPanel("Activos Intangibles",
                 h3(),
                 h3(textOutput("bu_int")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("int"),
                 h3(),
                 h3("Capital - Historico"),
                 h3(),
                 tableOutput("int_h"),
                 h3(),
                 h3("Saldos"),
                 h3(),
                 tableOutput("tec1")),
        ##############################################################
        tabPanel("ALM",
                 h3(),
                 h3(textOutput("bu_alm")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("alm"),
                 h3(),
                 h3("Capital - Historico"),
                 h3(),
                 tableOutput("alm_h"),
                 h3(),
                 h3("P&L"),
                 h3(),
                 h3(textOutput("Criteria")),
                 h3(),
                 plotOutput("plot_alm1")
                 
                 ),
        ##############################################################
        tabPanel("Credito",
                 h3(),
                 h3(textOutput("bu_cre")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("cre"),
                 h3(),
                 h3("Parametros"),
                 h3(),
                 tableOutput("cre1")
                 
                 
        ),
        ##############################################################
        tabPanel("DTAs",
                 h3(),
                 h3(textOutput("bu_dta")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("dta"),
                 h3(),
                 h3("Capital - Historico"),
                 h3(),
                 tableOutput("dta_h"),
                 h3(),
                 h3("Saldo Balance"),
                 h3(),
                 tableOutput("dta1")
                 
                 
        ), 
        ##############################################################
        tabPanel("Fondo Comercio",
                 h3(),
                 h3(textOutput("bu_gw")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("gw"),
                 h3(),
                 h3("Capital - Historico"),
                 h3(),
                 tableOutput("gw_h"),
                 h3(),
                 h3("Parametros"),
                 h3(),
                 tableOutput("gw1")
                 
                 
        ),
        ##############################################################
        tabPanel("Mercado",
                 h3(),
                 h3(textOutput("bu_mkt")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("mkt"),
                 h3(),
                 h3("Capital - Historico"),
                 h3(),
                 tableOutput("mkt_h"),
                 h3(),
                 h3("P&L"),
                 h3(),
                 #uiOutput("mkt2"),
                 h3(),
                 plotOutput("mkt2")
                 
                 
        ),
        ##############################################################
        tabPanel("Negocio",
                 h3(),
                 h3(textOutput("bu_ng")),
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("ng"),
                 h3(),
                 h3("Capital - Historico"),
                 h3(),
                 tableOutput("ng_h"),
                 h3(),
                 h3("Saldos"),
                 h3(),
                 tableOutput("neg1")),
        ##############################################################
        tabPanel("Pensiones",
                 h3(),
                 h3(textOutput("bu_pen")), 
                 h3(),
                 h3("Capital"),
                 h3(),
                 tableOutput("pen"),
                 h3(),
                 h3("Capital - Historico"),
                 h3(),
                 tableOutput("pen_h"),
                 h3())
        
      ),
      width=9
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
  
  mest<- reactive({
    as.numeric(input$month)
  })
  
  yeart<- reactive({
    as.numeric(input$year)
  })
  
  fhisto<- reactive({
    sm <- "'2018-12'"
    for (i in 1:12) {
      if (i< mest()) {
        if (i<10) {
           i_f<- paste("0", i,sep="")
           
        } else
        {
          i_f<- i
        }  
        ff <- paste("'",paste(yeart(),i_f, sep="-"),"'",sep="")
        sm <- paste(sm, ff, sep=",")
      } 
    }
    fhisto<- as.character(sm)
  })
  
  
  
  
  ####################################################
  # update select Unit
  ####################################################
  
  tempo_amk<- reactive({res<-dbSendQuery(con, paste0("SELECT BU_VERT FROM ADDON GROUP BY BU_VERT", sep=""))
  result<-dbFetch(res)
  tempo_amk<- unique(result$BU_VERT)
  })
  
  output$Prueba<- renderUI({ selectInput("buvert", "Unit:", choices=tempo_amk())})
  
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
  
  output$am_h<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Material Assets' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
  
  })
  
  ####################################################
  # Activos Materiales
  # Capital - historico
  ####################################################
  
  am_avgk<- eventReactive(input$ma_avg, {
      res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Material Assets' GROUP BY BU_VERT, fecha", sep=""))
      result<-dbFetch(res)
      result<- lapply(result[, 2:4], mean, na.rm = TRUE)
      format(result, big.mark=".", justify =c("right"))
  })
  
  output$am_avg<- renderTable({t(am_avgk())})
  
  output$fhistok <- reactive ({
    paste0("SELECT SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Material Assets' GROUP BY BU_VERT", sep="")
  })
  
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
  # Activos Intangibles
  # Capital - historico
  ####################################################
  
  output$int_h<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Intangible assets' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
    
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
  # Activos Materiales
  # Capital - historico
  ####################################################
  
  output$alm_h<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='ALM' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
    
  })
  
  
  ####################################################
  # RU vs Criteria
  ####################################################
  
  
  
  
  tempo_alm<- reactive({
    
    res<-dbSendQuery(con, paste0("SELECT VECTORES.* FROM VECTORES, Criterias, ADDON WHERE VECTORES.CRITERIA=Criterias.CRITERIA AND ADDON.BU_VERT='", input$buvert, "' AND VECTORES.fecha='", fcal() ,"' AND ADDON.RU_ID=Criterias.RU_ID AND ADDON.TIPO_RIESGO='ALM' GROUP BY fdata", sep=""))
    result<-dbFetch(res)
    tempo_alm<- as.data.frame(result)
  })
  
  output$plot_alm1 <-  renderPlot({
    ggplot(tempo_alm(), aes(x=fdata, y=PNL,  group = 1))+ geom_line(color = "red")
  
  
  })
  
  
  
  ####################################################
  # CREDITO
  # Capital
  ####################################################
  
  output$bu_cre<- reactive({input$buvert})
  
  output$cre<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT INSTRUMENTO_LOCAL, SUM(EC_DW_RTU), SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Credit' GROUP BY BU_VERT, INSTRUMENTO_LOCAL", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  
  ####################################################
  # Parametros
  ####################################################
  
  output$cre1<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT INSTRUMENTO_LOCAL, SUM(EAD) AS EAD, SUM(PD_SB*EAD)/SUM(EAD) AS PD_MED, SUM(PD_SB*EAD*LGD)/SUM(PD_SB*EAD) AS LGD_MED, AVG(CORR_MAY), AVG(CORR_MIN), AVG(PLAZO)   FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Credit' GROUP BY BU_VERT, INSTRUMENTO_LOCAL", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  
  ####################################################
  # DTAs
  # Capital
  ####################################################
  
  output$bu_dta<- reactive({input$buvert})
  
  output$dta<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT INSTRUMENTO_LOCAL, SUM(EC_DW_RTU), SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='DTAs' GROUP BY BU_VERT, INSTRUMENTO_LOCAL", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  ####################################################
  # DTA
  # Capital - historico
  ####################################################
  
  output$dta_h<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='DTAs' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
    
  })
  
  
  ####################################################
  # Saldo
  ####################################################
  
  output$dta1<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT sum(SALDO_BALANCE) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='DTAs' GROUP BY BU_VERT, INSTRUMENTO_LOCAL", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  ####################################################
  # Fondo de Comercio
  # Capital
  ####################################################
  
  output$bu_gw<- reactive({input$buvert})
  
  output$gw<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT INSTRUMENTO_LOCAL, SUM(EC_DW_RTU), SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Gooodwill' GROUP BY BU_VERT, INSTRUMENTO_LOCAL", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  
  ####################################################
  # Fondo de comercio
  # Capital - historico
  ####################################################
  
  output$gw_h<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Gooodwill' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
    
  })
  
  
  ####################################################
  # Saldo
  ####################################################
  
  output$gw1<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT sum(SALDO_BALANCE) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Gooodwill' GROUP BY BU_VERT, INSTRUMENTO_LOCAL", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  ####################################################
  # Mercado
  # Capital
  ####################################################
  
  output$bu_mkt<- reactive({input$buvert})
  
  output$mkt<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT INSTRUMENTO_LOCAL, SUM(EC_DW_RTU), SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Market' GROUP BY BU_VERT, INSTRUMENTO_LOCAL", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  
  ####################################################
  # Mercado
  # Capital - historico
  ####################################################
  
  output$mkt_h<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Market' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
    
  })
  
  
  ####################################################
  # RU vs Criteria
  ####################################################
  
  
  
  
  tempo_mkt<- reactive({
    res<-dbSendQuery(con, paste0("SELECT VECTORES.* FROM VECTORES, Criterias, ADDON WHERE VECTORES.CRITERIA=Criterias.CRITERIA AND ADDON.BU_VERT='", input$buvert, "' AND VECTORES.fecha='", fcal() ,"' AND ADDON.RU_ID=Criterias.RU_ID AND ADDON.TIPO_RIESGO='Market' GROUP BY fdata", sep=""))
    result<-dbFetch(res)
    tempo_mkt<- as.data.frame(result)
  })
  
  #output$mkt2 <-  renderUI({
  #  crit<- unique(tempo_mkt()$Criterias)
  #  for (i in crit) {
  #    tempo_mkt2<- tempo_mkt()[Criterias==i,]  
  #    ggplot(tempo_mkt2, aes(x=fdata, y=PNL,  group = 1))+ geom_line(color = "red")
  #  }
  #})
  
  output$mkt2<- renderPlot({
    ggplot(tempo_mkt(), aes(x=fdata, y=PNL,colour=CRITERIA,group=2))+ geom_line()
    
  })
  
  
  ####################################################
  # Negocio
  # Capital
  ####################################################
  
  output$bu_ng<- reactive({input$buvert})
  
  output$ng<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal(), "' AND TIPO_RIESGO='Business' GROUP BY BU_VERT", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
  })
  
  
  ####################################################
  # Activos Materiales
  # Capital - historico
  ####################################################
  
  output$ng_h<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Business' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
    
  })
  
  
  
  ####################################################
  # Saldos
  ####################################################
  tempo_am<- reactive({res<-dbSendQuery(con, paste0("SELECT RU_ID FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal(), "' AND TIPO_RIESGO='Business' GROUP BY BU_VERT,fecha", sep=""))
  result<-dbFetch(res)
  format(result, big.mark=".")
  tempo_am<- as.data.frame(result)
  })
  
  
  
  tempo1_am<- reactive({res<-dbSendQuery(con, paste0("SELECT U_CdG, Importe_2, COD_DIV1, RU_ID_NEGOCIO FROM PYG WHERE EPIGRAFE='101104' OR EPIGRAFE='1010901' AND fecha='" , fcal() , "'", sep=""))
  result1<-dbFetch(res)
  format(result1, big.mark=".")
  tempo1_am<- as.data.frame(result1)
  
  })
  
  #output$tec<- renderTable({left_join(tempo_am(), tempo1_am(), by=c("RU_ID"="RU_ID_ACTIVOS_MATERIALES"))})
  
  output$tec<- renderTable({res<-dbSendQuery(con, paste0("SELECT PYG.U_CdG, PYG.EPIGRAFE,sum(PYG.Importe_2), PYG.COD_DIV1, PYG.RU_ID_NEGOCIO FROM PYG,ADDON WHERE PYG.EPIGRAFE IN('101104','1010901') AND PYG.fecha='" , fcal() , "' AND ADDON.BU_VERT='", input$buvert ,"' AND PYG.RU_ID_ACTIVOS_MATERIALES=ADDON.RU_ID GROUP BY PYG.U_CdG,PYG.EPIGRAFE,PYG.COD_DIV1, PYG.RU_ID_NEGOCIO" , sep=""))
  result2<-dbFetch(res)
  format(result2, big.mark=".")
  })
  
  ####################################################
  # Pensiones
  # Capital
  ####################################################
  
  output$bu_pen<- reactive({input$buvert})
  
  output$pen<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT SUM(EC_DW_RTU), SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Pensions' GROUP BY BU_VERT", sep=""))
    result<-dbFetch(res)          
    format(result, big.mark=".")
  })
  
  ####################################################
  # Pensiones
  # Capital - historico
  ####################################################
  
  output$pen_h<- renderTable({
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Pensiones' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

