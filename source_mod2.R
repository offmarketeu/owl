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
    
    tabPanel("General",
             tagList(
               
               h3("Datos Generales"),
               h3(),
               tableOutput(nss("globaltb")),
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


EC4 <- function (input, output, session, act1, act2){
  
  
  fbase1 <- reactive( {
    fbase1<-read_excel(as.character(act1()), skip=4, col_names=TRUE, na = "")
    fbase1[is.na(fbase1)]<-0
    fbase1<-fbase1[,c(1:4, 70:78)]
    #fmod1$GEOGRAPHY2[(fmod1$TIPO_RIESGO)=="Goodwill"]<- "Corporate Activities"
  })
  
  fbase2 <- reactive( {
    fbase2<-read_excel(as.character(act2()), skip=4, col_names=TRUE, col_types="numeric",na = "")
  })
  
  
  fbase <- reactive({
    fbase<- bind_cols(as.data.frame(fbase1()),as.data.frame(fbase2()))
    as.data.frame(fbase)
    fbase<- fbase[, - c(14:17,83:91)]
    fbase %>% mutate_if(is.numeric , replace_na, replace = 0)
    fbase %>% mutate(GEOGRAPHY2=ifelse(TIPO_RIESGO=="Goodwill", "Corporate Activities", GEOGRAPHY2))
    
  })
  
  
  
  
  
  # Proceso de fusion mod
  
  fmod1 <- reactive( {
    fmod1<-read_excel(as.character(act2()), skip=4, col_names=TRUE, na = "")
    fmod1[is.na(fmod1)]<-0
    fmod1<-fmod1[,c(1:4, 70:78)]
    #fmod1$GEOGRAPHY2[(fmod1$TIPO_RIESGO)=="Goodwill"]<- "Corporate Activities"
  })
  
  fmod2 <- reactive( {
    fmod2<-read_excel(as.character(act2()), skip=4, col_names=TRUE, col_types="numeric",na = "")
  })
  
  
  fmod <- reactive({
    fmod<- bind_cols(as.data.frame(fmod1()),as.data.frame(fmod2()))
    as.data.frame(fmod)
    fmod<- fmod[, - c(14:17,83:91)]
    fmod %>% mutate_if(is.numeric , replace_na, replace = 0)
    #fmod$GEOGRAPHY2[(fmod$TIPO_RIESGO)=="Goodwill"]<- "Corporate Activities"
    fmod %>% mutate(GEOGRAPHY2=ifelse(TIPO_RIESGO=="Goodwill", "Corporate Activities", GEOGRAPHY2))
    
  })
  
  
  totdata<- reactive ({
    fmod<- bind_rows(as.data.frame(fbase()),as.data.frame(fmod()))
    
  })
  
  
  #output$globaltb <- renderTable ({
  #  as.data.frame(fmod())
  #})
  
  output$globaltb <- reactive ({
    totdata() %>% group_by(ID_SIMULACION)%>%
    summarise(EC_DT=sum(EC_DW_TOTAL+EC_DW_TOTAL_CVA)/1000000)
  })
  
    
  
  #output$globaltb <- reactive ({
  #   as.data.frame(totdata)
  #})  
  
}
