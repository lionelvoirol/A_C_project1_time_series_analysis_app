#Define working directorysetwd("~/SUISSE_2015-19/STATISTICS_PROGRAMMING/github_repo/A_C_project1_time_series_analysis_app")

#Load libraries
library(shiny)
library(quantmod)
library(ggplot2)
library(shinyTime)
library('tseries')
library(dsa)
library(quantmod)
library(forecast)

#load symbols, hashed as comment
#my_symbols = stockSymbols()

# Define UI
ui = shinyUI(fluidPage(
  titlePanel("Stock prices"),
  
  helpText("Display selected stocks and enable comparing different forecasting methods"),
  
  #Stock, domain, method and confidence interval selection
  sidebarPanel(
    width = 3,
    selectizeInput('stock_name', 'Stock', c('Select stock', my_symbols[,1]), selected = 'Select stock'),
    textOutput('selected_stock'),
    textOutput('sector'),
    textOutput('industry'),
    dateInput('start_time', 'Start Date', value = Sys.Date()- 365 *5),
    dateInput('end_time', 'End Date'),
    selectizeInput('forecasting_method', 'Method', c('Select method', 'MA', 'ETS'))
    #method to include  c('Select method', 'MA', 'ARIMA', 'ETS', 'GARCH', 'LSTM')

  ),
  #Parameters for MA method
  
  conditionalPanel(
    condition = "input.forecasting_method == 'MA'",
    absolutePanel(
      width = 250,
      top = 510, left = 50, 
      sliderInput('ma_order', "Number of days", min = 0, max =  365, 20, value = 10)
    )
  ),

  #Parameters for ARIMA method
  conditionalPanel(
    condition = "input.forecasting_method == 'ARIMA'",
    selectInput("smoothMethod", "Method",
                list("lm", "glm", "gam", "loess", "rlm"))
  ),
  
  #Parameters for ETS method
  conditionalPanel(
    condition = "input.forecasting_method == 'ETS'",
    absolutePanel(
      width = 250,
      top = 510, left = 50, 
      selectizeInput('ets_e', 'Error', c('A', 'M', 'N', 'Z'), selected = 'Z'),
      selectizeInput('ets_t', 'Trend', c('A', 'M', 'N','Z'), selected = 'Z'),
      selectizeInput('ets_s', 'Seasonality', c('A', 'M', 'N','Z'), selected = 'Z'),
      checkboxInput('ets_damped', 'Damped'),
      textOutput('selected_ets_model')
    )
  ),
  #Parameters for GARCH method
  conditionalPanel(
    condition = "input.forecasting_method == 'GARCH'",
    selectInput("smoothMethod", "Method",
                list("lm", "glm", "gam", "loess", "rlm"))
  ),
  
  #main plot
  mainPanel(
    plotOutput("my_plot")
  )
  
))

# Define server logic 
server = shinyServer(function(input, output){
  output$my_plot = renderPlot({
    
    #No selected stock
    if(input$stock_name == 'Select stock'){
    }

    #No method selected
    if(input$forecasting_method == 'Select method' && input$stock_name != 'Select stock'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      plot(myts2, ylab = 'Value')
    }
    
    #MA
    if(input$forecasting_method == 'MA' && input$stock_name != 'Select stock'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      plot(myts2)
      lines(ma(myts2, order=input$ma_order), col = 'blue')
    }

    #ETS
    if(input$forecasting_method == 'ETS' && input$stock_name != 'Select stock'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      ets_model = as.character(paste(input$ets_e, input$ets_t, input$ets_s, sep =''))
      fit = ets(myts2, model = ets_model, damped = input$ets_damped)
      plot(forecast(fit))
      #autoplot(my_ts, geom = 'line')  
      #autoplot(my_ts)
      
    }
    
  })
  output$selected_stock = renderText({
    my_symbols[my_symbols$Symbol == input$stock_name, 2]
  })
  output$sector = renderText({
    my_symbols[my_symbols$Symbol == input$stock_name, 6]
  })
  output$industry = renderText({
    my_symbols[my_symbols$Symbol == input$stock_name, 7]
  })
  output$initial_date = renderText({
    index(getSymbols(input$stock_name, auto.assign = F))[1]
  })
  output$selected_ets_model = renderText({
    as.character(paste(input$ets_e, input$ets_t, input$ets_s, sep =''))
  })
})

#Run app
shiny::shinyApp(ui,server)


