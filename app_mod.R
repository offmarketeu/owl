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
  
  
  headerPanel(
    list(img(src="descarga.jpg",height=50),
    img(src="logo.png",height=50))
  ),
  titlePanel("OWL-EC Control"),
  
  # Options to select information
  navbarPage("Version 2.0 reduce-module",
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
  tabPanel("Modo Simul"
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
  

  
  ####################################################
  # Activos Materiales
  # Capital - historico
  ####################################################
  
  #output$am_h<- renderTable({
  #  as.data.frame(gen_table1(input$buvert, fhisto(), 'Material Assets'))
  #}) 
  
  ####################################################
  # Activos Materiales
  # Capital - historico promedio
  ####################################################
  
  am_avgk<- eventReactive(input$ma_avg, {
      result<- as.data.frame(gen_table1(bu_am1(), fhisto(), 'Material Assets'))
      result<- lapply(result[, 2:4], mean, na.rm = TRUE)
      format(result, big.mark=".", justify =c("right"))
  })
  
  output$am_avg<- renderTable({t(am_avgk())})
  
  
  
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
  # Activos intangibles - Intangible assets
  # Capital
  ####################################################
  
  output$bu_int<- reactive({input$buvert})
  
  output$int<- renderTable({
    as.data.frame(gen_table(input$buvert, fcal(), 'Intangible assets'))
  })

  
  ####################################################
  # Activos Intangibles
  # Capital - historico
  ####################################################
  
  output$int_h<- renderTable({
    as.data.frame(gen_table1(bu_am1(), fhisto(), 'Intangible assets'))
  })
  
  
  ####################################################
  # Activos Intangibles
  # Capital - historico promedio
  ####################################################
  
  int_avgk<- eventReactive(input$tni_avg, {
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", input$buvert , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Intangible assets' GROUP BY BU_VERT, fecha", sep=""))
    result<-dbFetch(res)
    result<- lapply(result[, 2:4], mean, na.rm = TRUE)
    format(result, big.mark=".", justify =c("right"))
  })
  
  output$int_avg<- renderTable({t(int_avgk())})
  
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
    as.data.frame(gen_table(input$buvert, fcal(), 'ALM'))
  })
  
  
  ####################################################
  # Activos Materiales
  # Capital - historico
  ####################################################
  
  output$alm_h<- renderTable({
    as.data.frame(gen_table1(input$buvert, fcal(), 'ALM'))
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
  # CREDITO - Credit
  # Capital
  ####################################################
  
  output$bu_cre<- reactive({input$buvert})
  
  output$cre<- renderTable({
    as.data.frame(gen_table(input$buvert, fcal(), 'Credit'))
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
    as.data.frame(gen_table(input$buvert, fcal(), 'DTAs'))
  })
  
  ####################################################
  # DTA - DTAs
  # Capital - historico
  ####################################################
  
  output$dta_h<- renderTable({
    as.data.frame(gen_table1(input$buvert, fhisto(), 'DTAs'))
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
  # Fondo de Comercio - Gooodwill
  # Capital
  ####################################################
  
  output$bu_gw<- reactive({input$buvert})
  
  output$gw<- renderTable({
    as.data.frame(gen_table(input$buvert, fcal(), 'Gooodwill'))
  })
  
  
  ####################################################
  # Fondo de comercio
  # Capital - historico
  ####################################################
  
  output$gw_h<- renderTable({
    as.data.frame(gen_table1(input$buvert, fhisto(), 'Gooodwill'))
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
  # Mercado - Market
  # Capital
  ####################################################
  
  output$bu_mkt<- reactive({input$buvert})
  
  output$mkt<- renderTable({
    as.data.frame(gen_table(input$buvert, fcal(), 'Market'))
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
  # Negocio - Business
  # Capital
  ####################################################
  
  output$bu_ng<- reactive({input$buvert})
  
  output$ng<- renderTable({
    as.data.frame(gen_table(input$buvert, fcal(), 'Business'))
  })
  
  
  ####################################################
  # Negocio
  # Capital - historico
  ####################################################
  
  output$ng_h<- renderTable({
    as.data.frame(gen_table1(input$buvert, fhisto(), 'Business'))
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
  # Pensiones - Pensions
  # Capital
  ####################################################
  
  output$bu_pen<- reactive({input$buvert})
  
  output$pen<- renderTable({
    as.data.frame(gen_table(input$buvert, fcal(), 'Pensions'))
  })
  
  ####################################################
  # Pensiones
  # Capital - historico
  ####################################################
  
  output$pen_h<- renderTable({
    as.data.frame(gen_table1(input$buvert, fhisto(), 'Pensions'))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

