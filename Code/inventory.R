#---------------------------------------------------------------------------------------------#
# Title: Inventory Management
# Authors: Shivangi Tak
#---------------------------------------------------------------------------------------------#

#Import the following libraries.
library(shinydashboard)
library(ggplot2)
library(s20x)
library(dplyr)
library(ggvis)
library(reshape2)


#Set the header
header <- dashboardHeader(title = "Sales BI Tool")  

#Sidebar navigator menu items
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data", icon = icon("bar-chart-o"), tabName = "data"),
    menuItem("Analysis", icon = icon("bar-chart-o"), tabName = "analysis",
             menuItem("Inventory Management", icon = icon("bar-chart-o"), tabName = "inventmgt")
             
  )
))


#Define the UI's body
body <- dashboardBody(
  tabItems(
    tabItem("data",
            fluidRow(
              
              fileInput('file1', 'Choose the file containing data for analysis',
                        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
            
            fluidPage(fluidRow(column(12,offset=0, style='overflow-x: scroll',tableOutput('table'))))
            
    ),
    
    
    tabItem("inventmgt",
            fluidPage(
              titlePanel(fluidRow(column(9, offset=2))),
              
              box(title = "Attribute Selection", status = "primary", solidHeader = T, width = 12,
                  fluidPage(
                    fluidRow(
                      
                      column(3,offset = 0, style='padding:10px;',
                             selectInput("CP1","Cost Price",choices = NULL)),
                      column(3,offset = 1,
                             style='padding:10px;',selectInput("Quantity1","P1_Quantity",choices = NULL)),
                      column(3, offset = 1,
                             style='padding:10px;',selectInput("SP1","P1_SP",choices = NULL )),
                      column(3, offset = 0,
                             style='padding:10px;',selectInput("Time","Time",choices = NULL )),
                      column(3, offset = 1,
                             style='padding:10px;',selectInput("SP2","P2_SP",choices = NULL )),
                      column(3, offset = 1,
                             style='padding:10px;',selectInput("Quantity2","P2_Quantity",choices = NULL ))
                    ))),
              
              
              fluidRow(  valueBoxOutput("curr_q1")
                         ,valueBoxOutput("curr_q2")
                         ,valueBoxOutput("curr_if")
                         ,valueBoxOutput("pred_q1")
                         ,valueBoxOutput("pred_q2")
                         ,valueBoxOutput("pred_if")
              )
            ))
))

shinyApp(
  ui = dashboardPage(header, sidebar, body, skin='red'),
  server = function(input, output,session) {
    
    contentsrea <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath,header=T)
    })
    #----------------------------------------INVENTORY MANAGEMENT---------------------------------------#
    
    observe({
      
      
      df<-contentsrea()
      
      output$table<-renderTable({ head(df,n =5)},bordered = TRUE,
                                spacing = 's', width = 'auto', align = 'c',
                                rownames = TRUE, colnames = TRUE,style='overflow-x: scroll;border: 1px solid #000;')
      
      
      cp<-input$CP1
      sp1<-input$SP1
      q1<-input$Quantity1
      month<-input$Time
      sp2<-input$SP2
      q2<-input$Quantity2
      
      a<-grep(cp, colnames(df))
      b<-grep(sp1, colnames(df))
      c<-grep(q1, colnames(df))
      d<-grep(month, colnames(df))
      e<-grep(sp2, colnames(df))
      f<-grep(q2, colnames(df))
      
      if((length(a)+length(b)+length(c)+length(d)+length(e)+length(f))==6)
      {
        cp<-colnames(df)[a]
        sp1<-colnames(df)[b]
        q1<-colnames(df)[c]
        month<-colnames(df)[d]
        sp2<-colnames(df)[e]
        q2<-colnames(df)[f]
        
        print(df[,a])
        
        m3 <- lm(df[c(q1,month)])
        print(summary(m3))
        
        intercept <-as.numeric(m3$coefficients["(Intercept)"] )
        
        
        month_coeff1 <-as.numeric(m3$coefficients[month] )
        
        m<- as.numeric("1") 
        inputData <- data.frame(month=m)
        
        #------------------------PRODUCT 1 ANALYSIS-----------------------------------------
        qsales_1 <- predict(m3,inputData,interval="p")
        
        
        output$curr_q1 <- renderValueBox({
          valueBox(
            formatC(mean(df[,c]), format="d", big.mark=','),
            paste('--CURRENT DEMAND FOR PRODUCT 1: ',format(round(mean(df[,c]), 2), nsmall = 2))
            ,icon = icon("stats",lib='glyphicon')
            ,color = "navy")
          
        })
        
        output$pred_q1 <- renderValueBox({
          valueBox(
            formatC(qsales_1, format="d", big.mark=',')
            ,paste('--EXPECTED DEMAND OF PRODUCT 1 IN JANUARY: ',format(round(qsales_1, 2), nsmall = 2))
            ,icon = icon("stats",lib='glyphicon')
            ,color = "maroon")
          
        })
        
        
        
        #-----------------------------PRODUCT 2 ANALYSIS------------------------------------------
        
        m4 <- lm(df[c(q2,month)])
        print(summary(m4))
        
        intercept <-as.numeric(m4$coefficients["(Intercept)"] )
        
        
        month_coeff1 <-as.numeric(m4$coefficients[month] )
        
        m<- as.numeric("1") 
        inputData <- data.frame(month=m)
        
        
        qsales_2 <- predict(m4,inputData,interval="p")
        print(qsales_2)
        
        output$curr_q2 <- renderValueBox({
          valueBox(
            formatC(mean(df[,f]), format="d", big.mark=','),
            paste('--CURRENT DEMAND FOR PRODUCT 2 : ',format(round(mean(df[,f]), 2), nsmall = 2))
            ,icon = icon("stats",lib='glyphicon')
            ,color = "navy")
          
        })
        
        output$pred_q2 <- renderValueBox({
          valueBox(
            formatC(qsales_2, format="d", big.mark=',')
            ,paste('--EXPECTED DEMAND OF PRODUCT 2 IN JANUARY :',format(round(qsales_2, 2), nsmall = 2))
            ,icon = icon("stats",lib='glyphicon')
            ,color = "maroon")
          
        })
        
        #---------------------------------CASH INFLOW ANALYSIS------------------------------------------
        
        c_inflow <- (mean(df[,c])*mean(df[,b])) + (mean(df[,f])*mean(df[,e]))
        p_inflow <- (qsales_1*mean(df[,b])) + (qsales_2*mean(df[,e]))
        output$curr_if <- renderValueBox({
          valueBox(
            formatC(c_inflow, format="d", big.mark=',')
            ,paste('--CURRENT CASH INFLOW I:',format(round(c_inflow, 2), nsmall = 2))
            ,icon = icon("gbp",lib='glyphicon')
            ,color = "navy")
          
        })
        
        output$pred_if <- renderValueBox({
          valueBox(
            formatC(p_inflow, format="d", big.mark=',')
            ,paste('--PREDICTED CASH INFLOW FOR NEXT YEAR:',format(round(p_inflow, 2), nsmall = 2))
            ,icon = icon("gbp",lib='glyphicon')
            ,color = "maroon")
          
        })
      }
    }) 
    observe({     
      updateSelectInput(session, "Quantity1", choices = names(data.frame(contentsrea())),selected='NULL')
    })
    observe({
      updateSelectInput(session, "SP1", choices = names(data.frame(contentsrea())),selected='NULL')
    })
    observe({
      updateSelectInput(session, "Time", choices = names(data.frame(contentsrea())),selected='NULL')
    })
    observe({
      updateSelectInput(session, "SP2", choices = names(data.frame(contentsrea())),selected='NULL')
    })
    observe({
      updateSelectInput(session, "Quantity2", choices = names(data.frame(contentsrea())),selected='NULL')
      
    })
    observe({
      updateSelectInput(session, "CP1", choices = names(data.frame(contentsrea())),selected='NULL')
      
    })
    #----------------------------------------INVENTORY MANAGEMENT ENDS---------------------------------------#
    
})

    
