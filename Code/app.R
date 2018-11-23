#-----------------------------------------------------------------------------------------------------
# Title: Data upload Functionality
# Author: Shivangi Tak
#-----------------------------------------------------------------------------------------------------
#Import the following libraries.
library(shinydashboard)
library(dplyr)



#Set the header
header <- dashboardHeader(title = "Sales BI Tool")  

#Sidebar navigator menu items
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data", icon = icon("bar-chart-o"), tabName = "data")
    
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
            )
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
   
  })

