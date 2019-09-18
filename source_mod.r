###############################################################
# Menu 1
###############################################################
EC1_UI <- function (id) {
  ns <- NS(id)
  sidebarPanel(
    uiOutput(ns("Prueba")),
    selectInput(ns("year"),"Year: ", c("2018","2019"), "2019"),
    selectInput(ns("month"),"Month: ", c("01","02","03","04","05","06","07","08","09","10","11","12")),
    width=3, style="color: #fff; background-color: #cb3234; border-color: #2e6da4"
  )
}

###############################################################
# Menu 1.2
###############################################################


EC2_UI <- function (id) {
  nss <- NS(id)
  tabsetPanel(
   
    tabPanel("Activos Materiales",
             tagList(
              h3(),
              h3(textOutput(nss("bu_am"))),
              h3(),
              h3("Capital"),
              h3(),
              tableOutput(nss("am")),
              h3(),
              h3("Capital historico"),
              h3(),
              tableOutput(nss("am_h")),
              h3(),
              actionButton(nss("ma_avg"), "Media",style="color: #fff; background-color: #cb3234; border-color: #2e6da4"),
              h3(),
              tableOutput(nss("am_avg")),
              h3(),
              h3("Saldos"),
              h3(),
              tableOutput(nss("tec"))
              )
              ),
              ##############################################################
              tabPanel("Activos Intangibles",
                tagList(       
                       h3(),
                       h3(textOutput(nss("bu_int"))), 
                       h3(),
                       h3("Capital"),
                       h3(),
                       tableOutput(nss("int")),
                       h3(),
                       h3("Capital - Historico"),
                       h3(),
                       tableOutput(nss("int_h")),
                       h3(),
                       actionButton(nss("tni_avg"), "Media",style="color: #fff; background-color: #cb3234; border-color: #2e6da4"),
                       h3(),
                       tableOutput(nss("int_avg")),
                       h3("Saldos"),
                       h3(),
                       tableOutput(nss(("tec1")))
                )       
            ),
    
    ##############################################################
    tabPanel("ALM",
             h3(),
             h3(textOutput(nss("bu_alm"))), 
             h3(),
             h3("Capital"),
             h3(),
             tableOutput(nss("alm")),
             h3(),
             h3("Capital - Historico"),
             h3(),
             tableOutput(nss("alm_h")),
             h3(),
             h3("P&L"),
             h3(),
             h3(textOutput(nss("Criteria"))),
             h3(),
             plotOutput(nss("plot_alm1"))
             
    ),

  ##############################################################
  tabPanel("Credito",
           h3(),
           h3(textOutput(nss("bu_cre"))), 
           h3(),
           h3("Capital"),
           h3(),
           tableOutput(nss("cre")),
           h3(),
           h3("Parametros"),
           h3(),
           tableOutput(nss("cre1"))
           
           
  ) ,
  ##############################################################
  tabPanel("DTAs",
           h3(),
           h3(textOutput(nss("bu_dta"))), 
           h3(),
           h3("Capital"),
           h3(),
           tableOutput(nss("dta")),
           h3(),
           h3("Capital - Historico"),
           h3(),
           tableOutput(nss("dta_h")),
           h3(),
           h3("Saldo Balance"),
           h3(),
           tableOutput(nss("dta1"))
  ),
  ##############################################################
  tabPanel("Fondo Comercio",
           h3(),
           h3(textOutput(nss("bu_gw"))), 
           h3(),
           h3("Capital"),
           h3(),
           tableOutput(nss("gw")),
           h3(),
           h3("Capital - Historico"),
           h3(),
           tableOutput(nss("gw_h")),
           h3(),
           h3("Parametros"),
           h3(),
           tableOutput(nss("gw1"))
  ),
  ##############################################################
  tabPanel("Mercado",
           h3(),
           h3(textOutput(nss("bu_mkt"))), 
           h3(),
           h3("Capital"),
           h3(),
           tableOutput(nss("mkt")),
           h3(),
           h3("Capital - Historico"),
           h3(),
           tableOutput(nss("mkt_h")),
           h3(),
           h3("P&L"),
           h3(),
           #uiOutput("mkt2"),
           h3(),
           plotOutput(nss("mkt2"))
  ),
  ##############################################################
  tabPanel("Negocio",
           h3(),
           h3(textOutput(nss("bu_ng"))),
           h3(),
           h3("Capital"),
           h3(),
           tableOutput(nss("ng")),
           h3(),
           h3("Capital - Historico"),
           h3(),
           tableOutput(nss("ng_h")),
           h3(),
           h3("Saldos"),
           h3(),
           tableOutput(nss("neg1"))
  ),
  ##############################################################
  tabPanel("Pensiones",
           h3(),
           h3(textOutput(nss("bu_pen"))), 
           h3(),
           h3("Capital"),
           h3(),
           tableOutput(nss("pen")),
           h3(),
           h3("Capital - Historico"),
           h3(),
           tableOutput(nss("pen_h")),
           h3()
  
  )
  )


}


###############################################################
# Menu 1s
###############################################################

EC1 <- function (input, output, session){
  
  con <- dbConnect(RSQLite::SQLite(), "C:/Users/n040485/Documents/owl/owl.db")
  
  tempo_amk<- reactive({res<-dbSendQuery(con, paste0("SELECT GEOGRAPHY2 FROM ADDON GROUP BY GEOGRAPHY2", sep=""))
  result<-dbFetch(res)
  tempo_amk<- unique(result$GEOGRAPHY2)
  })
  
  output$Prueba<- renderUI({ selectInput("buvert", "Unit:", choices=tempo_amk())})
  
  
  
}

###############################################################
# Menu 1.2s
###############################################################


EC2 <- function (input, output, session, action, action2, action3){
  
  con <- dbConnect(RSQLite::SQLite(), "C:/Users/n040485/Documents/owl/owl.db")
  
  #############################################################################################
  # General function
  #############################################################################################
  
  gen_table <- function(Unit, Fdate, Risk) {
    
    res<-dbSendQuery(con, paste0("SELECT SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE GEOGRAPHY2='", Unit , "' AND  fecha='", Fdate, "' AND TIPO_RIESGO='", Risk, "' GROUP BY GEOGRAPHY2", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
  }
  
  gen_table1 <- function(Unit, Fdate, Risk) {
    
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE GEOGRAPHY2='", Unit , "' AND fecha IN (", Fdate ,") AND TIPO_RIESGO='" , Risk, "' GROUP BY GEOGRAPHY2, fecha", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
  }
  
  #############################################################################################
  # Auxiliary variables
  #############################################################################################
  
  
  fcal<- reactive({paste0(input$year,"-",input$month, sep="")})
  
  mest<- reactive({
    as.numeric(input$month)
    })
  
  yeart<- reactive({
    as.numeric(input$year)
  })
  
  
  
  fhisto<- reactive({
    sm_ <- "'2018-12'"
    for (i in 1:12) {
      if (i< mest()) {
        if (i<10) {
          i_f<- paste("0", i,sep="")
          
        } else
        {
          i_f<- i
        }  
        ff <- paste("'",paste(yeart(),i_f, sep="-"),"'",sep="")
        sm_ <- paste(sm_, ff, sep=",")
      } 
    }
    fhisto<- as.character(sm_)
  })
  
  
  
  #############################################################################################
  # Capital - Activos Materiales   
  #############################################################################################
  
  output$am <- renderTable({
    as.data.frame(gen_table(action(), fcal() , 'Material Assets'))
  })
  
  ####################################################
  # Activos Materiales
  # Capital - historico
  ####################################################
  
  output$am_h<- renderTable({
    as.data.frame(gen_table1(action(), fhisto(), 'Material Assets'))
  })  
  
  
  ####################################################
  # Activos Materiales
  # Capital - historico promedio
  ####################################################
  
  am_avgk<- eventReactive(input$ma_avg, {
    result<- as.data.frame(gen_table1(action(), fhisto(), 'Material Assets'))
    result<- lapply(result[, 2:4], mean, na.rm = TRUE)
    format(result, big.mark=".", justify =c("right"))
  })
  
  output$am_avg<- renderTable({t(am_avgk())})
  
  
  
  ####################################################
  # Saldos
  ####################################################
  
  tempo_am<- reactive({res<-dbSendQuery(con, paste0("SELECT RU_ID FROM ADDON WHERE GEOGRAPHY2='", action() , "' AND fecha='", fcal(), "' AND TIPO_RIESGO='Material Assets' GROUP BY GEOGRAPHY2,fecha", sep=""))
  result<-dbFetch(res)
  format(result, big.mark=".")
  tempo_am<- as.data.frame(result)
  })
  
  tempo1_am<- reactive({res<-dbSendQuery(con, paste0("SELECT U_CdG, Importe_2, COD_DIV1, RU_ID_ACTIVOS_MATERIALES FROM PYG WHERE EPIGRAFE='101104' OR EPIGRAFE='1010901' AND fecha='" , fcal() , "'", sep=""))
  result1<-dbFetch(res)
  format(result1, big.mark=".")
  tempo1_am<- as.data.frame(result1)
  
  })
  
  
  output$tec<- renderTable({res<-dbSendQuery(con, paste0("SELECT PYG.U_CdG, PYG.EPIGRAFE,sum(PYG.Importe_2), PYG.COD_DIV1, PYG.RU_ID_ACTIVOS_MATERIALES FROM PYG,ADDON WHERE PYG.EPIGRAFE IN('101104','1010901') AND PYG.fecha='" , fcal() , "' AND ADDON.GEOGRAPHY2='", action() ,"' AND PYG.RU_ID_ACTIVOS_MATERIALES=ADDON.RU_ID GROUP BY PYG.U_CdG,PYG.EPIGRAFE,PYG.COD_DIV1, PYG.RU_ID_ACTIVOS_INTANGIBLES" , sep=""))
  result2<-dbFetch(res)
  format(result2, big.mark=".")
  })
  
 
  
####################################################
# Activos intangibles - Intangible assets
# Capital
####################################################

output$bu_int<- reactive({action()})

output$int<- renderTable({
  as.data.frame(gen_table(action(), fcal(), 'Intangible assets'))
})


####################################################
# Activos Intangibles
# Capital - historico
####################################################

output$int_h<- renderTable({
  as.data.frame(gen_table1(action(), fhisto(), 'Intangible assets'))
})


####################################################
# Activos Intangibles
# Capital - historico promedio
####################################################

int_avgk<- eventReactive(input$tni_avg, {
  res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE GEOGRAPHY2='", action() , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Intangible assets' GROUP BY GEOGRAPHY2, fecha", sep=""))
  result<-dbFetch(res)
  result<- lapply(result[, 2:4], mean, na.rm = TRUE)
  format(result, big.mark=".", justify =c("right"))
})

output$int_avg<- renderTable({t(int_avgk())})

####################################################
# Saldos
####################################################


tempo_int<- reactive({res<-dbSendQuery(con, paste0("SELECT RU_ID FROM ADDON WHERE GEOGRAPHY2='", action() , "' AND fecha='", fcal(), "' AND TIPO_RIESGO='Intangible assets' GROUP BY GEOGRAPHY2,fecha", sep=""))
result<-dbFetch(res)
tempo_int<- as.data.frame(result)
format(result, big.mark=".")
})


tempo1_int<- reactive({res<-dbSendQuery(con, paste0("SELECT U_CdG, Importe_2, COD_DIV1, RU_ID_ACTIVOS_INTANGIBLES FROM PYG WHERE EPIGRAFE='907' OR EPIGRAFE='1010902' AND fecha='" , fcal() , "'", sep=""))
result1<-dbFetch(res)
format(result1, big.mark=".")
tempo1_int<- as.data.frame(result1)
})

output$tec1<- renderTable({res<-dbSendQuery(con, paste0("SELECT PYG.U_CdG, PYG.EPIGRAFE,sum(PYG.Importe_2), PYG.COD_DIV1, PYG.RU_ID_ACTIVOS_INTANGIBLES FROM PYG,ADDON WHERE PYG.EPIGRAFE IN('907','1010902') AND PYG.fecha='" , fcal() , "' AND ADDON.GEOGRAPHY2='", action() ,"' AND PYG.RU_ID_ACTIVOS_INTANGIBLES=ADDON.RU_ID GROUP BY PYG.U_CdG,PYG.EPIGRAFE,PYG.COD_DIV1, PYG.RU_ID_ACTIVOS_INTANGIBLES" , sep=""))
result2<-dbFetch(res)
format(result2, big.mark=".", align="right")

})

####################################################
# ALM
# Capital
####################################################

output$bu_alm<- reactive({action()})

output$alm<- renderTable({
  as.data.frame(gen_table(action(), fcal(), 'ALM'))
})


####################################################
# ALM
# Capital - historico
####################################################

output$alm_h<- renderTable({
  as.data.frame(gen_table1(action(), fhisto(), 'ALM'))
})


####################################################
# RU vs Criteria
####################################################


tempo_alm<- reactive({
  
  res<-dbSendQuery(con, paste0("SELECT VECTORES.* FROM VECTORES, Criterias, ADDON WHERE VECTORES.CRITERIA=Criterias.CRITERIA AND ADDON.GEOGRAPHY2='", action(), "' AND VECTORES.fecha='", fcal() ,"' AND ADDON.RU_ID=Criterias.RU_ID AND ADDON.TIPO_RIESGO='ALM' GROUP BY fdata", sep=""))
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

output$bu_cre<- reactive({action()})

output$cre<- renderTable({
  as.data.frame(gen_table(action(), fcal(), 'Credit'))
})


####################################################
# Parametros
####################################################

output$cre1<- renderTable({
  res<-dbSendQuery(con, paste0("SELECT INSTRUMENTO_LOCAL, SUM(EAD) AS EAD, SUM(PD_SB*EAD)/SUM(EAD) AS PD_MED, SUM(PD_SB*EAD*LGD)/SUM(PD_SB*EAD) AS LGD_MED, AVG(CORR_MAY), AVG(CORR_MIN), AVG(PLAZO)   FROM ADDON WHERE GEOGRAPHY2='", action() , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Credit' GROUP BY GEOGRAPHY2, INSTRUMENTO_LOCAL", sep=""))
  result<-dbFetch(res)          
  format(result, big.mark=".")
})


####################################################
# DTAs
# Capital
####################################################

output$bu_dta<- reactive({action()})

output$dta<- renderTable({
  as.data.frame(gen_table(action(), fcal(), 'DTAs'))
})

####################################################
# DTA - DTAs
# Capital - historico
####################################################

output$dta_h<- renderTable({
  as.data.frame(gen_table1(action(), fhisto(), 'DTAs'))
})


####################################################
# Saldo
####################################################

output$dta1<- renderTable({
  res<-dbSendQuery(con, paste0("SELECT sum(SALDO_BALANCE) FROM ADDON WHERE GEOGRAPHY2='", action() , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='DTAs' GROUP BY GEOGRAPHY2, INSTRUMENTO_LOCAL", sep=""))
  result<-dbFetch(res)          
  format(result, big.mark=".")
})


####################################################
# Fondo de Comercio - Gooodwill
# Capital
####################################################

output$bu_gw<- reactive({action()})

output$gw<- renderTable({
  as.data.frame(gen_table(action(), fcal(), 'Gooodwill'))
})


####################################################
# Fondo de comercio
# Capital - historico
####################################################

output$gw_h<- renderTable({
  as.data.frame(gen_table1(action(), fhisto(), 'Gooodwill'))
})


####################################################
# Saldo
####################################################

output$gw1<- renderTable({
  res<-dbSendQuery(con, paste0("SELECT sum(SALDO_BALANCE) FROM ADDON WHERE GEOGRAPHY2='", action() , "' AND fecha='", fcal() , "' AND TIPO_RIESGO='Gooodwill' GROUP BY GEOGRAPHY2, INSTRUMENTO_LOCAL", sep=""))
  result<-dbFetch(res)          
  format(result, big.mark=".")
})

####################################################
# Mercado - Market
# Capital
####################################################

output$bu_mkt<- reactive({action()})

output$mkt<- renderTable({
  as.data.frame(gen_table(action(), fcal(), 'Market'))
})


####################################################
# Mercado
# Capital - historico
####################################################

output$mkt_h<- renderTable({
  res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE GEOGRAPHY2='", action() , "' AND fecha IN (", fhisto() ,") AND TIPO_RIESGO='Market' GROUP BY GEOGRAPHY2, fecha", sep=""))
  result<-dbFetch(res)
  format(result, big.mark=".", justify =c("right"))
  
})


####################################################
# RU vs Criteria
####################################################




tempo_mkt<- reactive({
  res<-dbSendQuery(con, paste0("SELECT VECTORES.* FROM VECTORES, Criterias, ADDON WHERE VECTORES.CRITERIA=Criterias.CRITERIA AND ADDON.GEOGRAPHY2='", action(), "' AND VECTORES.fecha='", fcal() ,"' AND ADDON.RU_ID=Criterias.RU_ID AND ADDON.TIPO_RIESGO='Market' GROUP BY fdata", sep=""))
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

output$bu_ng<- reactive({action()})

output$ng<- renderTable({
  as.data.frame(gen_table(action(), fcal(), 'Business'))
})


####################################################
# Negocio
# Capital - historico
####################################################

output$ng_h<- renderTable({
  as.data.frame(gen_table1(action(), fhisto(), 'Business'))
})



####################################################
# Saldos
####################################################
tempo_am<- reactive({res<-dbSendQuery(con, paste0("SELECT RU_ID FROM ADDON WHERE GEOGRAPHY2='", action() , "' AND fecha='", fcal(), "' AND TIPO_RIESGO='Business' GROUP BY GEOGRAPHY2,fecha", sep=""))
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

output$tec<- renderTable({res<-dbSendQuery(con, paste0("SELECT PYG.U_CdG, PYG.EPIGRAFE,sum(PYG.Importe_2), PYG.COD_DIV1, PYG.RU_ID_NEGOCIO FROM PYG,ADDON WHERE PYG.EPIGRAFE IN('101104','1010901') AND PYG.fecha='" , fcal() , "' AND ADDON.GEOGRAPHY2='", action() ,"' AND PYG.RU_ID_ACTIVOS_MATERIALES=ADDON.RU_ID GROUP BY PYG.U_CdG,PYG.EPIGRAFE,PYG.COD_DIV1, PYG.RU_ID_NEGOCIO" , sep=""))
result2<-dbFetch(res)
format(result2, big.mark=".")
})

####################################################
# Pensiones - Pensions
# Capital
####################################################

output$bu_pen<- reactive({action()})

output$pen<- renderTable({
  as.data.frame(gen_table(action(), fcal(), 'Pensions'))
})

####################################################
# Pensiones
# Capital - historico
####################################################

output$pen_h<- renderTable({
  as.data.frame(gen_table1(action(), fhisto(), 'Pensions'))
})



}
