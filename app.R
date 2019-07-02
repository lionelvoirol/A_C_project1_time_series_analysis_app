#set wd Victor
setwd("~/A_C_project1_time_series_analysis_app")

#set wd LV
#setwd("~/SUISSE_2015-19/STATISTICS_PROGRAMMING/github_repo/A_C_project1_time_series_analysis_app")


#Load libraries
library(shiny)
library(quantmod)
library(ggplot2)
library(shinyTime)
library(tseries)
library(dsa)
library(forecast)
library(ggfortify)
library(smooth)
library(Mcomp)
source("ma_function.R")
source("ra_functions.R")

#load symbols, hashed as comment
my_symbols = stockSymbols()

# Define UI
ui = shinyUI(fluidPage(
  titlePanel("Stock prices"),
  
  helpText("Display selected stocks and enable comparing different forecasting methods"),
  
  #Stock, domain, method, forecast and confidence interval selection
  sidebarPanel(
    width = 3,
    selectizeInput('stock_name', 'Stock', c('Select stock', my_symbols[,1]), selected = 'Select stock'),
    textOutput('selected_stock'),
    textOutput('sector'),
    textOutput('industry'),
    dateInput('start_time', 'Start Date', value = Sys.Date()- 365 *5),
    dateInput('end_time', 'End Date'),
    selectizeInput('forecasting_method', 'Method', c('Select method','Return Tendencies','Naive', 'Mean', 'Seasonal Naive', 'MA', 'ETS'))
    #method to include  c('Select method', 'MA', 'ARIMA', 'ETS', 'GARCH', 'LSTM')
    
  ),
  
  #Parameters for forecasting
  conditionalPanel(
    condition = "input.forecasting_method != 'Select method'",
    absolutePanel(
      width = 250,
      top = 530, left = 50, 
      sliderInput('days_forecast', 'Days to forecast', min = 0, max =  365, value = 15),
      selectizeInput('pred_interval', 'Prediction Interval', c('99', '95', '90', '80','70'), selected = '95')
      
    )
  ),
  
  #Parameters for "Return Tendencies"
  
  conditionalPanel(
    condition = "input.forecasting_method == 'Return Tendencies'",
    absolutePanel(
      width = 250,
      selectizeInput('return_method', 'method', c('percent return','return'))
    )
  ),
  
  #Parameters for MA method
  
  conditionalPanel(
    condition = "input.forecasting_method == 'MA'",
    absolutePanel(
      width = 250,
      top = 700, left = 50, 
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
      top = 700, left = 50, 
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
      print(autoplot(myts2))
    }
    #Mean
    if(input$forecasting_method == 'Mean' && input$stock_name != 'Selected stock'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      print(autoplot(myts2) +
              autolayer(meanf(myts2, h=input$days_forecast),
                        series="Mean", PI=FALSE) +
              xlab("Year") + ylab("Price") +
              guides(colour=guide_legend(title="Forecast")))
    }
    #Naive
    if(input$forecasting_method == 'Naive' && input$stock_name != 'Selected stock'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      print(autoplot(myts2) +
              autolayer(naive(myts2, h=input$days_forecast),
                        series = "Naive", PI=FALSE)+
              xlab("Year") + ylab("Price") +
              guides(colour=guide_legend(title="Forecast")))
    }
    #Seasonal Naive
    if(input$forecasting_method == 'Seasonal Naive' && input$stock_name != 'Selected stock'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      print(autoplot(myts2) +
              autolayer(snaive(myts2, h=input$days_forecast),
                        series="Seasonal Naive", PI=FALSE) +
              xlab("Year") + ylab("Price") +
              guides(colour=guide_legend(title="Forecast")))
    }
    
    # Return Tendencies
    if(input$forecasting_method == 'Return Tendencies' && input$stock_name != 'Select stock'){
      
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts_1 = my_ts[,4]
      myts2 = xts2ts(my_ts_1, freq = 364.25)
      par(mfrow=c(2,2))
      plot(myts2)
      
      days_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
      barplot(week_day_return(my_ts, input$return_method), main = "Return average by day of the week",
              xlab = "Weekday", col = c("darkviolet", "khaki"),
              names.arg = days_names)
      months_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                        "October", "November", "December")
      barplot(monthly_return(my_ts, input$return_method), main = "Return average by month of the year",
              xlab = "Month", col = c("darkviolet", "khaki"),
              names.arg = months_names)
      par(mfrow=c(1,1))
    }
    
    #MA
    if(input$forecasting_method == 'MA' && input$stock_name != 'Select stock'){

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
    if(input$forecasting_method == 'ETS' && input$stock_name != 'Select stock'){
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time
      )
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      ets_model = as.character(paste(input$ets_e, input$ets_t, input$ets_s, sep =''))
      fit = ets(myts2, model = ets_model, damped = input$ets_damped)
      print(autoplot(forecast(fit)))
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

