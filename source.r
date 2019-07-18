###############################################################
#
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
#
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
              h3()
              
             )
    )
  )
}


###############################################################
#
###############################################################

EC1 <- function (input, output, session){
  
  con <- dbConnect(RSQLite::SQLite(), "C:/Users/n040485/Documents/owl/owl.db")
  
  tempo_amk<- reactive({res<-dbSendQuery(con, paste0("SELECT BU_VERT FROM ADDON GROUP BY BU_VERT", sep=""))
  result<-dbFetch(res)
  tempo_amk<- unique(result$BU_VERT)
  })
  
  output$Prueba<- renderUI({ selectInput("buvert", "Unit:", choices=tempo_amk())})
  
  
  
}

###############################################################
#
###############################################################


EC2 <- function (input, output, session, action, action2, action3){
  
  con <- dbConnect(RSQLite::SQLite(), "C:/Users/n040485/Documents/owl/owl.db")
  
  #############################################################################################
  # General function
  #############################################################################################
  
  gen_table <- function(Unit, Fdate, Risk) {
    
    res<-dbSendQuery(con, paste0("SELECT SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", Unit , "' AND  fecha='", Fdate, "' AND TIPO_RIESGO='", Risk, "' GROUP BY BU_VERT", sep=""))
    result<-dbFetch(res)
    format(result, big.mark=".", justify =c("right"))
  }
  
  gen_table1 <- function(Unit, Fdate, Risk) {
    
    res<-dbSendQuery(con, paste0("SELECT fecha, SUM(EC_DW_RTU),  SUM(REPORTING_V), SUM(EC_DW_TOTAL) FROM ADDON WHERE BU_VERT='", Unit , "' AND fecha IN (", Fdate ,") AND TIPO_RIESGO='" , Risk, "' GROUP BY BU_VERT, fecha", sep=""))
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
    as.data.frame(gen_table1(input$buvert, fhisto(), 'Material Assets'))
  })  
  
} 

###############################################################
#
###############################################################

simul_Input <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  sidebarPanel(
    tagList(
    selectInput(ns("year1"),"Year: ", c("2018","2019"), "2019"),
    selectInput(ns("month1"),"Month: ", c("01","02","03","04","05","06","07","08","09","10","11","12"))
    ),
  width=3, style="color: #fff; background-color: #cb3234; border-color: #2e6da4"
  )
  
  
}


###############################################################
#
###############################################################

simul_Tab_UI <- function(id) {
  ns <- NS(id)
  mainPanel(
    tagList(
    tabsetPanel(
      tabPanel("Activos Materiales1",
               h3(),
               h3(textOutput(ns("bu_am125"))),
               h3()
              )
    )
  )
  )
  
  
}


EC2222 <- function (input, output, session, action){
  
  
  
  
  ####################################################
  # Activos Materiales
  # Capital - historico
  ####################################################
  
  output$am_h<- renderTable({
    as.data.frame(gen_table1(input$buvert, fhisto(), 'Material Assets'))
  })  
  
}