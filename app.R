#set wd Victor
#setwd("~/A_C_project1_time_series_analysis_app")

#set wd LV
setwd("~/SUISSE_2015-19/STATISTICS_PROGRAMMING/github_repo/A_C_project1_time_series_analysis_app")

#wd thib
#setwd("~/Desktop/GithubRepo/A_C_project1_time_series_analysis_app")

#set wd DI
#setwd("C:/Users/blast/Documents/GitHub/A_C_project1_time_series_analysis_app")

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
library(rugarch)
library(tseries)
source("ma_function.R")
source("ra_functions.R")
source("armagarch_functions.R")

#load symbols, hashed as comment
#my_symbols = stockSymbols()

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
    selectizeInput('forecasting_method', 'Method', 
                   c('Select method','Return Tendencies',
                     'Naive', 'Mean', 'Seasonal Naive', 'MA', 'ETS',
                     'Arima-Garch'))
    #method to include  c('Select method', 'MA', 'ARIMA', 'ETS', 'GARCH', 'LSTM')
    
  ),
  
  #Parameters for "Return Tendencies"
  
  conditionalPanel(
    condition = "input.forecasting_method == 'Return Tendencies'",
    absolutePanel(
      width = 250,
      top = 500, left = 50, 
      selectizeInput('return_method', 'method', c('percent return','return'))
    )
  ),

  #Parameters for Naive method
  
  conditionalPanel(
    condition = "input.forecasting_method == 'Naive'",
    absolutePanel(
      width = 250,
      top = 500, left = 50, 
      sliderInput('days_forecast_naive', 'Days to forecast', min = 0, max =  365, value = 15),
      selectizeInput('pred_interval_naive', 'Prediction Interval', c('99', '95', '90', '80', '70','60', '50', '40', '30', '20', '10'), selected = '95')
      
    )
  ),

#Parameters for Mean method

conditionalPanel(
  condition = "input.forecasting_method == 'Mean'",
  absolutePanel(
    width = 250,
    top = 500, left = 50, 
    sliderInput('days_forecast_mean', 'Days to forecast', min = 0, max =  365, value = 15),
    selectizeInput('pred_interval_mean', 'Prediction Interval', c('99', '95', '90', '80', '70','60', '50', '40', '30', '20', '10'), selected = '95')
    
  )
),

#Parameters for Seasonal Naive method

conditionalPanel(
  condition = "input.forecasting_method == 'Seasonal Naive'",
  absolutePanel(
    width = 250,
    top = 500, left = 50, 
    sliderInput('days_forecast_seasonal_naive', 'Days to forecast', min = 0, max =  365, value = 15),
    selectizeInput('pred_interval_seasonal_naive', 'Prediction Interval', c('99', '95', '90', '80', '70','60', '50', '40', '30', '20', '10'), selected = '95')
    
  )
),
  
  #Parameters for MA method
  
  conditionalPanel(
    condition = "input.forecasting_method == 'MA'",
    absolutePanel(
      width = 250,
      top = 500, left = 50, 
      sliderInput('ma_order', "Number of days", min = 5, max =  365, 5, value = 10),
      selectizeInput('ma_type', 'Type', c('simple', 'exponential'))
    )
  ),
  
  #Parameters for ARIMA-GARCH method
  
  conditionalPanel(
    condition = "input.forecasting_method == 'Arima-Garch'",
    absolutePanel(
      width = 250,
      top = 500, left = 50,
      selectInput("AR_para", "AR (p)", 
                  choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4), selected = 2), 
      selectInput("MA_para", "MA (q)", 
                  choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4), selected = 2),  
      selectInput("lag_variance_para", "G (p)", 
                  choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4), selected = 2), 
      selectInput("lag_res_para", label = "ARCH (q)", 
                  choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4), selected = 2),
      radioButtons("transform", label = "Transformation Selection",
                   choices = list("Close Price" = "Close", 
                                  "Returns (1st differences)" = "Returns", 
                                  "Log Returns" = "Log Returns"), 
                   selected = "Log Returns")
      
    )
  ),
  
  #Parameters for ETS method
  conditionalPanel(
    condition = "input.forecasting_method == 'ETS'",
    absolutePanel(
      width = 250,
      top = 500, left = 50, 
      sliderInput('days_forecast', 'Days to forecast', min = 0, max =  365, value = 15),
      selectizeInput('pred_interval_ets', 'Prediction Interval', c('99', '95', '90', '80', '70','60', '50', '40', '30', '20', '10'), selected = '95'),
      selectizeInput('ets_e', 'Error', c('A', 'M', 'N', 'Z'), selected = 'Z'),
      selectizeInput('ets_t', 'Trend', c('A', 'M', 'N','Z'), selected = 'Z'),
      selectizeInput('ets_s', 'Seasonality', c('A', 'M', 'N','Z'), selected = 'Z'),
      checkboxInput('ets_damped', 'Damped'),
      textOutput('selected_ets_model')
    )
  ),
  
  #main plot
  mainPanel(
    plotOutput("my_plot"),
    
  #table output
    tableOutput("view"),
    tableOutput("view2")
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
              autolayer(meanf(myts2, h=input$days_forecast_mean, level = as.numeric(input$pred_interval_mean)),
                        series="Mean", PI=T) +
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
              autolayer(naive(myts2, h=input$days_forecast_naive, level = as.numeric(input$pred_interval_naive)),
                        series = "Naive", PI=T)+
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
              autolayer(snaive(myts2, h=input$days_forecast_seasonal_naive, level = as.numeric(input$pred_interval_seasonal_naive)),
                        series="Seasonal Naive", PI=T) +
              xlab("Year") + ylab("Price") +
              guides(colour=guide_legend(title="Forecast")))
    }
    
    # Return Tendencies
    if(input$forecasting_method == 'Return Tendencies' && input$stock_name != 'Select stock'){
      
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time)
      my_ts_1 = my_ts[,4]
      myts2 = xts2ts(my_ts_1, freq = 364.25)
      par(mfrow=c(2,2))
      plot(myts2)
      
      color_for_returns <- ifelse(week_day_return(my_ts, input$return_method)>0,"green","red")
      color_for_variance <- "darkslateblue"
      
      days_names <- c("Monday", "", "Tuesday", "","Wednesday", "","Thursday", "","Friday", "")
      
      barplot(week_day_return(my_ts, input$return_method), main = "Day of the week analysis",
              xlab = "Weekday", col=c(color_for_returns[1], color_for_variance, 
                                      color_for_returns[3], color_for_variance, 
                                      color_for_returns[5], color_for_variance,
                                      color_for_returns[7], color_for_variance,
                                      color_for_returns[9], color_for_variance),
              names.arg = days_names,
              space=c(2,0,2,0,2,0,2,0,2,0),
              ylim = c(min(week_day_return(my_ts, input$return_method)), 
                       max(week_day_return(my_ts, input$return_method)) * 2))
              
              legend("top", legend = c("positive return", "negative return", "variance"), 
                     inset=c(-0.2,0), xpd = TRUE,
                     fill = c("green", "red", "darkslateblue"))
              
      
      color_for_returns <- ifelse(monthly_return(my_ts, input$return_method)>0,"green","red")
              
      months_names <- c("Jan", "", "Feb", "", "Mar", "", "Apr", "", "May", "", "Jun", "", "Jul",
                        "", "Aug", "", "Sep", "", "Oct",  "","Nov",  "","Dec", "")
                        
      barplot(monthly_return(my_ts, input$return_method), main = "Month of the year analysis",
              xlab = "Month", col=c(color_for_returns[1], color_for_variance, 
                                    color_for_returns[3], color_for_variance, 
                                    color_for_returns[5], color_for_variance,
                                    color_for_returns[7], color_for_variance,
                                    color_for_returns[9], color_for_variance,
                                    color_for_returns[11], color_for_variance,
                                    color_for_returns[13], color_for_variance, 
                                    color_for_returns[15], color_for_variance, 
                                    color_for_returns[17], color_for_variance,
                                    color_for_returns[19], color_for_variance,
                                    color_for_returns[21], color_for_variance,
                                    color_for_returns[23], color_for_variance),
              names.arg = months_names,
              space=c(2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0),
              ylim = c(min(monthly_return(my_ts, input$return_method)), 
                       max(monthly_return(my_ts, input$return_method)) * 2 ))
      legend("top", legend = c("positive return", "negative return", "variance"), 
             inset=c(-0.2,0), xpd = TRUE,
             fill = c("green", "red", "darkslateblue"))
      
      names <- c(input$stock_name, "" , "S&P 500","", "Gold", "", "Bitcoin $", "")
      barplot(alternative_assets(my_ts, input$start_time, input$end_time, input$return_method), 
              main = "Comparison to other assets",
              names.arg = names,
              col=c("purple","khaki"),
              space=c(2,0,2,0,2,0,2,0))
              legend("top", legend = c("average return", "average variance"), 
              fill = c("purple", "khaki"))
              
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
      print(autoplot(forecast(fit, h= input$days_forecast, level = as.numeric(input$pred_interval_ets))))
      #autoplot(my_ts, geom = 'line')  
      #autoplot(my_ts)
    }
    
    #ARIMA-GARCH
    if(input$forecasting_method == 'Arima-Garch' && input$stock_name != 'Select stock'){
      
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time)
      
      my_ts = my_ts[,4]
      myts2 = xts2ts(my_ts, freq = 364.25)
      
      # Adf - Test Table Info
      serie_variants <- c("Close","Returns","Log Returns")
      table_complete <- cbind(serie_variants, stationarity_test(my_ts))
      colnames(table_complete) <- c("Transformation of the series", "p-value of the ADF test")
    
      # Arima-Garch Table Info
      my_ts = getSymbols.yahoo(input$stock_name, auto.assign = F,
                               from = input$start_time, to = input$end_time)
      if(input$transform == "Close"){
        my_ts_for_garch <- my_ts[,4]
      } else if(input$transform =="Returns"){
        my_ts_for_garch <- na.omit(diff(my_ts[,4]))
      } else if(input$transform =="Log Returns"){
        my_ts_for_garch <- na.omit(diff(log(my_ts[,4])))
      }
      # (GLD[,4] , "1" , "1", "1", "1", "90", "Returns", 3914)
      mat <- garma_model(my_ts_for_garch, input$AR_para, input$MA_para,
                         input$lag_variance_para, input$lag_res_para,
                         input$pred_interval, input$transform, Cl(my_ts[length(my_ts_for_garch)]),
                         input$days_forecast)
      
      # Adf - Test Table Creation
      output$view <- renderTable({
        table_complete
      })
      
      # Arima-Garch forecast Table Creation
      output$view2 <- renderTable({
        na.omit(mat)
      })
      
      priceandempty <- c(as.numeric(Cl(my_ts)), rep(NA, input$days_forecast))
      
      plot(priceandempty, type="l")
      lines(mat[,2], col ="purple", lty = 2)
      lines(mat[,1], col ="red", lty = 2)
      lines(mat[,3], col ="red", lty = 2)
     
     
    
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

