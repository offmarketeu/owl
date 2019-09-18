###############################################################
# Menu 3
###############################################################
EC3_UI <- function (id) {
  ns <- NS(id)
  sidebarPanel(
    fileInput("file1", "Fichero 1"),
    fileInput("file2", "Fichero 2"),
    width=3, style="color: #fff; background-color: #cb3234; border-color: #2e6da4"
  )
}



###############################################################
# Menu 1.2
###############################################################


EC4_UI <- function (id) {
  nss <- NS(id)
  tabsetPanel(
    
    tabPanel("Control",
             tagList(
               h3("Control1 - Numero de RUs"),
               h3(),
               tableOutput(nss("globalct1")),
               h3(),
               h3("Control2 - Correlacion Mayorista"),
               h3(),
               tableOutput(nss("globalct2")),
               h3(),
               h3("Control3 - Correlacion Minorista"),
               h3(),
               tableOutput(nss("globalct2"))
             )
             ),
    
    tabPanel("General ",
             tagList(
               h3("Datos Generales"),
               h3(),
               tableOutput(nss("globaltb"))
               
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


EC4 <- function (input, output, session, act1, act2){
  
  fbase <- reactive( {
    read_excel(path = act1(), skip = 4)
  })
  
  fmod <- reactive( {
    read_excel(path = act2(), skip = 4)
  })
  
  totdata<- reactive ({
    totdata<- bind_rows(as.data.frame(fbase()),as.data.frame(fmod()))
    
  })
 
  output$globaltb <- renderTable({
    totdata() %>% group_by(ID_SIMULACION) %>% summarise(EC_RTU= sum(EC_DW_RTU_T, na.rm=T)/1000000, EC_RV=sum(REPORTING_V_T, na.rm=T)/1000000,EC_DT= sum(EC_DW_TOTAL_T, na.rm=T)/1000000) 
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
     
     ct3<- reactive({
       ct3<- totdata() %>% select(ID_SIMULACION, GEOGRAPHY2, EAD, CORR_MIN) %>%
         filter(CORR_MIN!=0) %>%
         group_by(ID_SIMULACION, GEOGRAPHY2) %>% 
         summarise(min_p = sum(EAD*CORR_MAY, na.rm=T)/(sum(EAD, na.rm=T)))
       
     })
     
     output$globalct3 <- renderTable({
       as.data.frame(ct3()) %>% spread(ID_SIMULACION, min_p)     
     })
     
}