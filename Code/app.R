#-----------------------------------------------------------------------------------------------------
# Authors: Shivangi Tak, Shivam Ratnakar
#-----------------------------------------------------------------------------------------------------
#Import the following libraries.
library(shinydashboard)
library(dplyr)



#Set the header
header <- dashboardHeader(title = "Sales BI Tool")  

#Sidebar navigator menu items
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data", icon = icon("bar-chart-o"), tabName = "data"),
    menuItem("Attribute Selection", icon = icon("bar-chart-o"), tabName = "attribute_s")
    
  )
)

#Define the UI's body
body <- dashboardBody(
  tabItems(
    tabItem("data",
            fluidRow(
              
              fileInput('file1', 'Choose the file containing data for analysis',
                        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
            
            fluidPage(fluidRow(column(12,offset=0, style='overflow-x: scroll',tableOutput('table'))))
            ),
    
    tabItem("attribute_s",
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
                    )))))
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
    
    observe({
       df<-contentsrea()
      output$table<-renderTable({ head(df,n =5)},bordered = TRUE,
                                spacing = 's', width = 'auto', align = 'c',
                                rownames = TRUE, colnames = TRUE,style='overflow-x: scroll;border: 1px solid #000;')
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
   
  })

