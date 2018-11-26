#---------------------------------------------------------------------------------------------#
# Title: Profit Maximization
# Authors: Shivam Ratnakar
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
             menuItem("Inventory Management", icon = icon("bar-chart-o"), tabName = "inventmgt"),
             menuItem("Profit Maximization", icon = icon("bar-chart-o"), tabName = "profitmax"))
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
              ),
              box(
                title = "MONTHLY SALES FORECAST"
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,height = "500px"
                ,width = "300px"
                ,ggvisOutput("plot1")
              )
            )),
    
    tabItem("profitmax",
            fluidPage(
              titlePanel(fluidRow(column(9, offset=3))),
              
              box(title = "Attribute Selection", status = "primary", solidHeader = T, width = 12,    
                  column(3,offset = 0, style='padding:10px;',
                         selectInput("CP","Cost Price",choices = NULL)),
                  column(3,offset = 1,
                         style='padding:10px;',selectInput("SP","Selling Price",choices = NULL)),
                  column(3, offset = 1,
                         style='padding:10px;',selectInput("ad_type","Adv_info",choices = NULL )),
                  column(3, offset = 0,
                         style='padding:10px;',selectInput("Product_sales","Sales",choices = NULL )),
                  column(3, offset = 1,
                         style='padding:10px;',selectInput("Month","Month",choices = NULL )),
                  column(3, offset = 1,
                         style='padding:10px;',selectInput("Another_Product_SP","SP of other product",choices = NULL ))
              )),
            
            fluidRow(  valueBoxOutput("current_price")
                       ,valueBoxOutput("current_profit")
                       ,valueBoxOutput("current_sales")
                       ,valueBoxOutput("Optimum_price")
                       ,valueBoxOutput("new_profit")
                       ,valueBoxOutput("new_sales")
            ),
            
            box(
              title = "Old Vs New Profit per Store"
              ,status = "primary"
              ,solidHeader = TRUE 
              ,collapsible = TRUE
              ,height = "500px" 
              ,width = "300px"
              # ,plotOutput("profitbyRegion", height = "300px")
              ,ggvisOutput("plot")
              
            )
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
        if(a==b || b==c || c==d || d==e || e==f || f==a || a==c || a==d || a==e || b==d || b==e || b==f || c==e || c==f || d==f)
          showNotification("All six inputs must be unique!", type="error", duration = 10)
        else
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
        #----------------------------------------GRAPH------------------------------------
        
        month_data <- matrix(1:12, nrow=12,ncol=1)
        month_data<-data.frame(month_data)
        colnames(month_data) <- c("month")
        print(month_data)
        
        qgsales_2 <- predict(m4,month_data,interval="p")
        
        qgsales_1 <- predict(m3,month_data,interval="p")
        
        s1 <- data.frame(qgsales_1[,1])
        s2 <- data.frame(qgsales_2[,1])
        
        graph_data <- data.frame(cbind(month_data,s1,s2))
        colnames(graph_data) <- c("Month","Product_1","Product_2")
        
        # profit_graph %>% ggvis(x= ~Stores) %>%
        DT2 <- melt(graph_data, 'Month', c('Product_1','Product_2'))
        DT2 %>% ggvis(~Month, ~value, stroke=~variable) %>% set_options(height = 460, width = 920) %>% layer_lines() %>%
          add_axis("x", subdivide = 1, values = 1:12) %>%
          add_axis( "y", title = "Monthly sales forecast")%>%
          bind_shiny("plot1")
        
        }
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
    
    #----------------------------------------PROFIT MAXIMIZATION STARTS---------------------------------------#
    
    observe({
      df<-contentsrea()
      
      qty<-input$Product_sales
      sale_price<-input$SP
      adv_type<-input$ad_type
      tshirt_sp<-input$Another_Product_SP
      cost_price<-input$CP
      month<-input$Month
      
      a<-grep(qty, colnames(df))
      b<-grep(sale_price, colnames(df))
      c<-grep(adv_type, colnames(df))
      d<-grep(tshirt_sp, colnames(df))
      e<-grep(cost_price, colnames(df))
      f<-grep(month, colnames(df))
      if((length(a)+length(b)+length(c)+length(d)+length(e)+length(f))==6)
      {
        print(a)
        print(b)
        print(c)
        print(d)
        print(e)
        print(f)
        if(a==b || b==c || c==d || d==e || e==f || f==a || a==c || a==d || a==e || b==d || b==e || b==f || c==e || c==f || d==f)
           showNotification("All six inputs must be unique!", type="error", duration = 10)
        else
        {
          qty<-colnames(df)[a]
          sale_price<-colnames(df)[b]
          adv_type<-colnames(df)[c]
          tshirt_sp<-colnames(df)[d]
          cost_price<-colnames(df)[e]
          month<-colnames(df)[f]
          
          
          m3 <- lm(df[c(qty,sale_price,adv_type,tshirt_sp)])
          # setnames(df,c("sales",qty),c("price",prce)
          print(summary(m3))
          intercept <-as.numeric(m3$coefficients["(Intercept)"] )
          price_coeff <-as.numeric(m3$coefficients[sale_price] )
          factor1_coeff <-as.numeric(m3$coefficients[adv_type] )
          factor2_coeff <-as.numeric(m3$coefficients[tshirt_sp] )
          const <- as.numeric((factor1_coeff*mean(df[,c]))+(factor2_coeff*mean(df[,d])))
          const2 <- as.numeric(intercept + const)
          coeff_pricesq <- as.numeric(price_coeff)
          coeff_price <- as.numeric((-mean(df[,e])*price_coeff)+ const2)
          const_eq <- as.numeric(-mean(df[,e])*const2)
          print(coeff_pricesq)
          print(coeff_price)
          print(const_eq)
          f1 = function(x) (coeff_pricesq)*x^2 + (coeff_price)* x + const_eq
          res <- optimize(f1,lower=0,upper=20,maximum=TRUE)
          print(res)
          
          dfr <- data.frame(df)
          dfr[,b]<-res$maximum
          print(dfr[,b])
          
          inputData <- data.frame(mean(dfr[,b]),mean(df[,c]),mean(df[,d]))
          colnames(inputData) <- c(colnames(df)[b],colnames(df)[c],colnames(df)[d])
          
          
          qsales <- predict(m3,inputData,interval="p")
          psales <- predict(m3,dfr,interval="p")
          dfr[,a] <- psales[,1]
          
          stores <- data.frame(df[,f])  
          
          old_profit <- data.frame((df[,b] - df[,e])*df[,a] )
          old_avg_profit <- (mean(df[,b])-mean(df[,e]))*mean(df[,a])
          new_profit <- data.frame((dfr[,b] - dfr[,e])*dfr[,a])
          old_avg_SP <- mean(df[,b])
          
          profit_graph <- data.frame(bind_cols(stores, old_profit, new_profit))
          colnames(profit_graph) <- c( "Month","Old_Profit","New_Profit")
          print(profit_graph)
          print(const)
          print(const2)
          print(mean(df[,c])) 
          
          output$Optimum_price <- renderValueBox({
            
            valueBox(
              formatC(res$maximum, format="d", big.mark=','),
              paste('Optimum Price for max profit:',format(round(res$maximum, 2), nsmall = 2))
              ,icon = icon("gbp",lib='glyphicon')
              ,color = "green")
            
          })
          
          output$new_profit <- renderValueBox({
            valueBox(
              formatC(res$objective, format="d", big.mark=',')
              ,paste('Expected Profit at optimum price:',format(round(res$objective, 2), nsmall = 2))
              ,icon = icon("stats",lib='glyphicon')
              ,color = "purple")
            
            
          })
          
          output$new_sales <- renderValueBox({
            
            valueBox(
              formatC(qsales[,1], format="d", big.mark=','),
              paste('Expected sales at optimum price:',format(round(qsales[,1], 2), nsmall = 2))
              ,icon = icon("menu-hamburger",lib='glyphicon')
              ,color = "yellow")
            
          })
          output$current_price <- renderValueBox({
            
            valueBox(
              formatC(old_avg_SP, format="d", big.mark=','),
              paste('Current average selling price:',format(round(old_avg_SP, 2), nsmall = 2))
              ,icon = icon("gbp",lib='glyphicon')
              ,color = "green")
            
          })
          
          #creating the valueBoxOutput content
          output$current_profit <- renderValueBox({
            valueBox(
              formatC(old_avg_profit, format="d", big.mark=',')
              ,paste('Current Average Profit:',old_avg_profit)
              ,icon = icon("stats",lib='glyphicon')
              ,color = "purple")
            
            
          })
          
          output$current_sales <- renderValueBox({
            
            valueBox(
              formatC(mean(df[,a]), format="d", big.mark=','),
              paste('Current Average Sales:',mean(df[,a]))
              ,icon = icon("menu-hamburger",lib='glyphicon')
              ,color = "yellow")
            
          })
          
          # ----------------------------Graph----------------------------------
          DT2 <- melt(profit_graph, 'Month', c('Old_Profit','New_Profit'))
          DT2 %>% ggvis(~Month, ~value, stroke=~variable) %>% set_options(height = 460, width = 920) %>% layer_lines() %>%
            add_axis("x", subdivide = 1, values = 1:nrow(profit_graph)) %>%
            add_axis( "y", title = "Profit earned")%>%
            bind_shiny("plot")
        }
        
      }
      
    })
    
    observe({ updateSelectInput(session, "SP", choices = names(data.frame(contentsrea())), selected=input$SP1)
    })
    observe({
      updateSelectInput(session, "CP", choices = names(data.frame(contentsrea())), selected = input$CP1)
    })
    observe({
      updateSelectInput(session, "ad_type", choices = names(data.frame(contentsrea())), selected ='NULL')
    })
    observe({
      updateSelectInput(session, "Product_sales", choices = names(data.frame(contentsrea())), selected =input$Quantity1)
    })
    observe({
      updateSelectInput(session, "Month", choices = names(data.frame(contentsrea())), selected ='NULL')
      
    })
    observe({
      updateSelectInput(session, "Another_Product_SP", choices = names(data.frame(contentsrea())), selected = input$SP2)
      
    })
    
    #----------------------------------------PROFIT MAXIMIZATION ENDS---------------------------------------#
  })
