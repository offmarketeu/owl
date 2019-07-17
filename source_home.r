###############################################################
#
###############################################################
EC1_UI <- function (id) {
  ns <- NS(id)
  sidebarPanel(
    tagList(
    uiOutput(ns("Prueba")),
    selectInput(ns("year"),"Year: ", c("2018","2019"), "2019"),
    selectInput(ns("month"),"Month: ", c("01","02","03","04","05","06","07","08","09","10","11","12"))
    ),
    width=3, style="color: #fff; background-color: #cb3234; border-color: #2e6da4"
    )
}

###############################################################
#
###############################################################


EC2_UI <- function (id) {
  nss <- NS(id)
             tagList(
              h3(),
              h3(textOutput(nss("bu_am"))),
              h3(),
              h3("Capital"),
              h3(),
              tableOutput(nss("am")),
              h3(),
              h3(textOutput(nss("am_h")))
             
      )
    

}


###############################################################
#
###############################################################

EC1 <- function (input, output, session){
  
  con <- dbConnect(RSQLite::SQLite(), "C:/Users/Pedro/Documents/test_module/owl.db")
  
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
  
  con <- dbConnect(RSQLite::SQLite(), "C:/Users/Pedro/Documents/test_module/owl.db")
  
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
  
  
      fcal<- reactive({paste0(input$year,"-",input$month, sep="")})
  
  output$am <- renderTable({
    as.data.frame(gen_table(action(), fcal() , 'Material Assets'))
  })
  
  output$am_h <- renderText({fcal()})
  
} 

