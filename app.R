setwd("~/R tests")

#Load libraries
library(shiny)
library(quantmod)
library(ggplot2)
library(shinyTime)
library(tseries)
library(dsa)
library(quantmod)
library(forecast)

library(smooth)
library(Mcomp)

source("ma_function.R")


#load symbols, hashed as comment
# my_symbols = stockSymbols()

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
      sliderInput('ma_order', "Number of days", min = 5, max =  365, 5, value = 10),
      selectizeInput('ma_type', 'Type', c('simple', 'exponential'))
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
      selectizeInput('ets_e', 'Error', c('A', 'M', 'N', 'Z')),
      selectizeInput('ets_t', 'Trend', c('A', 'M', 'N','Z')),
      selectizeInput('ets_s', 'Seasonality', c('A', 'M', 'N','Z')),
      checkboxInput('ets_damped', 'Damped')
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
    
    #No method selected
    if(input$forecasting_method == 'Select method'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      plot(myts2, ylab = 'Value')
    }
    # MA # Moving Average function from "ma_function.R"
    if(input$forecasting_method == 'MA'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      plot(myts2)
      # arguments: (series, type, period)
      m_a <- moving_average(series = myts2, type = input$ma_type, period = input$ma_order)
      m_a <- ts(m_a,  
                start = start(myts2), end = end(myts2), frequency = 364.25)
      lines(m_a, col = "purple")
    }
    
    #ETS
    if(input$forecasting_method == 'ETS'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      ets_model = as.character(c(input$ets_e, input$ets_t, input$ets_s))
      fit = ets(myts2, model = 'AAN', damped = input$ets_damped)
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
})

#Run app
shiny::shinyApp(ui,server)
