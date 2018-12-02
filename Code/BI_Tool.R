#---------------------------------------------------------------------------------------------#
# Title: Inventory Management
# Authors: Shivangi Tak, Shivam Ratnakar
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
                             style='padding:10px;',selectInput("Ad_type","Ad Type",choices = NULL ))
                    ))),
              
              
              fluidRow(  valueBoxOutput("curr_q1", width=6)
                         ,valueBoxOutput("pred_q1", width=6)
                         , valueBoxOutput("price_q1", width=6)
                         ,valueBoxOutput("price_q2", width=6)
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
            ),
            fluidRow(
              box(title = "Enter the values against which you wich to predict sales", status = "primary", solidHeader = T, width = 5,
                  textInput("sales","Enter value of sales"),
                  textInput("adtype","Enter value of adtype"),
                  textInput("tshirt_p","Enter value of tshirt price"),
                  actionButton("submit", "Submit"))
              
              ,fluidRow(valueBoxOutput("price_op", width=5)))
            
            
    ),
    
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
    #-------------------------------------------- ERROR CHECKING ------------------------------------------------#
    observe({
      
      
      df<-contentsrea()
      
      cp<-input$CP1
      sp1<-input$SP1
      q1<-input$Quantity1
      month<-input$Time
      sp2<-input$SP2
      ad_type1=input$Ad_type
      
      a<-grep(cp, colnames(df))
      b<-grep(sp1, colnames(df))
      c<-grep(q1, colnames(df))
      d<-grep(month, colnames(df))
      e<-grep(sp2, colnames(df))
      f<-grep(ad_type1, colnames(df))
      
      
  
      
      if((length(a)+length(b)+length(c)+length(d)+length(e)+length(f))==6)
      {
        
        if((a==b || b==c || c==d || d==e || e==f || f==a || a==c || a==d || a==e || b==d || b==e || b==f || c==e || c==f || d==f))
        {
          
          showNotification("All six inputs must be unique!", type="error", duration = 10)
          
        }
    
      }
      
  })
    #----------------------------------------ERROR CHECKING---------------------------------------------#
   
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
      ad_type1=input$Ad_type
      
      a<-grep(cp, colnames(df))
      b<-grep(sp1, colnames(df))
      c<-grep(q1, colnames(df))
      d<-grep(month, colnames(df))
      e<-grep(sp2, colnames(df))
      f<-grep(ad_type1, colnames(df))
      
      
      if((length(a)+length(b)+length(c)+length(d)+length(e)+length(f))==6)
      {
        
        if(a!=b && b!=c && c!=d && d!=e && e!=f && f!=a && a!=c && a!=d && a!=e && b!=d && b!=e && b!=f && c!=e && c!=f && d!=f)
        { 
          
          cp=colnames(df)[a]
          sp1=colnames(df)[b]
          q1=colnames(df)[c]
          month=colnames(df)[d]
          sp2=colnames(df)[e]
          ad_type1=colnames(df)[f]
          
          
          #------------------------PRODUCT 1 ANALYSIS-----------------------------------------
          
          # library("tseries")
          # adf.test(diff(log(data_final)), alternative="stationary", k=0)
          library(forecast)
          library(tseries)
          data_final=ts(df[,c])
          fit=auto.arima(data_final,seasonal=FALSE)
          fcast=forecast(fit, h=6)
          values_pred=print(fcast)
          
          print(mean(df[,c]))
          output$curr_q1 <- renderValueBox({
            valueBox(
              formatC(mean(df[,c]), format="d", big.mark=','),
              paste('--CURRENT AVERAGE DEMAND FOR PRODUCT 1: ',format(mean(df[,c]), nsmall = 2))
              ,icon = icon("stats",lib='glyphicon')
              ,color = "navy")
            
          })
          
          output$price_q1 <- renderValueBox({
            valueBox(
              formatC(mean(df[,b]), format="d", big.mark=',')
              ,paste('--SELLING PRICE: ',format((mean(df[,b])), nsmall = 2))
              ,icon = icon("stats",lib='glyphicon')
              ,color = "maroon")
            
          })
          
          output$pred_q1 <- renderValueBox({
            valueBox(
              formatC(values_pred[1,1], format="d", big.mark=',')
              ,paste('--EXPECTED DEMAND OF PRODUCT 1 IN COMING MONTH: ',format((values_pred[1,1]), nsmall = 2))
              ,icon = icon("stats",lib='glyphicon')
              ,color = "navy")
            
          })
          
          output$price_q2 <- renderValueBox({
            valueBox(
              formatC(mean(df[,b]), format="d", big.mark=',')
              ,paste('--SELLING PRICE: ',format((mean(df[,b])), nsmall = 2))
              ,icon = icon("stats",lib='glyphicon')
              ,color = "maroon")
            
          })
          
          # ----------------------------------------GRAPH------------------------------------
          
          pred_demand=as.data.frame(append(df[,c],values_pred[1:6,1]))
          time=as.data.frame(c(1:36))
          
          demand_graph <- data.frame(bind_cols(time, pred_demand))
          colnames(demand_graph) <- c( "Month","Predicted_Demand")
          
          DT2 <- melt(demand_graph, 'Month', 'Predicted_Demand')
          DT2 %>% ggvis(~Month, ~value, stroke=~variable) %>% set_options(height = 460, width = 920)%>%
            layer_points() %>% 
            add_tooltip(function(data){paste0("Demand: ", data$value)}, "hover") %>% layer_lines() %>%
            add_axis("x", subdivide = 1, values = 1:nrow(demand_graph)) %>%
            add_axis( "y", title = "Predicted Demand")%>%
            bind_shiny("plot1")
          
          
          #-------------------------------------Sales prediction------------------------------
          
          model=lm(df[c(q1,sp1,ad_type1,sp2)])
          
          intercept=as.numeric(model$coefficients["(Intercept)"] )
          price_coeff=as.numeric(model$coefficients[sp1] )
          ad_coeff=as.numeric(model$coefficients[ad_type1] )
          tshirt_coeff=as.numeric(model$coefficients[sp2] )
          
          observeEvent( input$submit, 
                        {
                          sales_inp=as.numeric(input$sales) 
                          adtype_inp=as.numeric(input$adtype)
                          tshirt_inp=as.numeric(input$tshirt_p)
                          
                          price=(sales_inp-((ad_coeff*adtype_inp)+(tshirt_coeff*tshirt_inp)+intercept))/price_coeff
                          
                          
                          output$price_op <- renderValueBox({
                            valueBox(
                              formatC(price, format="d", big.mark=',')
                              ,paste('--REQUIRED SELLING PRICE OF THE PRODUCT: ',format(price), nsmall = 2)
                              ,icon = icon("stats",lib='glyphicon')
                              ,color = "yellow")
                            
                          })
                        })
          
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
      updateSelectInput(session, "CP1", choices = names(data.frame(contentsrea())),selected='NULL')
      
    })
    observe({
      updateSelectInput(session, "Ad_type", choices = names(data.frame(contentsrea())),selected='NULL')
      
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
        if(a!=b && b!=c && c!=d && d!=e && e!=f && f!=a && a!=c && a!=d && a!=e && b!=d && b!=e && b!=f && c!=e && c!=f && d!=f)
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
      updateSelectInput(session, "ad_type", choices = names(data.frame(contentsrea())), selected =input$Ad_type)
    })
    observe({
      updateSelectInput(session, "Product_sales", choices = names(data.frame(contentsrea())), selected =input$Quantity1)
    })
    observe({
      updateSelectInput(session, "Month", choices = names(data.frame(contentsrea())), selected =input$Time)
      
    })
    observe({
      updateSelectInput(session, "Another_Product_SP", choices = names(data.frame(contentsrea())), selected = input$SP2)
      
    })
    
    #----------------------------------------PROFIT MAXIMIZATION ENDS---------------------------------------#
  })
