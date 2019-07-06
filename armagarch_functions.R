# Arima-Garch forecast function


GLD <- getSymbols.yahoo("GLD", auto.assign = F)



stationarity_test <- function(series){
  options(warn=-1)
  
  close <- as.numeric(Cl(series))
  test_1 <- adf.test(close)
  returns <- na.omit(diff(close))
  test_2 <- adf.test(returns)
  log_returns <- diff(log(close))
  test_3 <- adf.test(log_returns)
  
  results <- c(round(test_1$p.value, digits = 3), 
               round(test_2$p.value, digits = 3), 
               round(test_3$p.value, digits = 3)) 
  
  options(warn=0)
  return(results)
}

garma_model <- function(series, ar, ma, g, arch, interval, transformation, first_close){
  
  
  
  #### SPECIFICATIONS OF THE MODEL #### 
  ugarchspecs <- ugarchspec(variance.model=list(garchOrder = c(g,arch)),
                            mean.model=list(armaOrder=c(ar,ma),
                            arfima = FALSE), 
                            distribution="std")
  #### ESTIMATION OF MODEL ####
  ugarch <- ugarchfit(ugarchspecs,series)
  # Squared Residuals (Epsilon squared)
  resid2 <- (ugarch@fit$residuals)^2
  resid <- (ugarch@fit$residuals)
  # Variance (Sigma squared)
  var <- ugarch@fit$var
  # Yt model
  Yt <- ugarch@fit$fitted.values
  #### FORECASTS ####
  # Forecast duration
  fcd <- 5 
  
  vtf <- length(series) # Visualization time frame
  
  vtf_b <- vtf + 1 # Forecast begin time
  vtf_e <- vtf + fcd # Forecast end time
  
  # Forecast object creation
  fc <- ugarchforecast(ugarch, n.ahead = fcd)
  # forecast of the sigma (vol)
  fc.sig <- fc@forecast$sigmaFor
  # forecast of the series (could be price, returns, whatever is input)
  fc.ser <- fc@forecast$seriesFor
  # forecast strucuture so we can put it in the plot
  fc_struc <- c(tail(Yt,Vtf),rep(NA,Vtf_d))
  fc3 <- c(rep(NA,Vtf),(fc.ser))
  
  # Different intervals to be displayed
  
  alpha <- 1-(interval/100)
  
  upper <- c(rep(NA,vtf), fc3[vtf_b:vtf_e] + qt(alpha/2, Inf, lower.tail = F) * (fc.sig))
  lower <- c(rep(NA,vtf), fc3[vtf_b:vtf_e] - qt(alpha/2, Inf, lower.tail = F) * (fc.sig))
  
  # Restoration to price - for prediction and intervals
  if(transformation == "Close"){
    fcf <- fc.ser
    upper_f <- upper
    lower_f <- lower
  } else if(transformation == "Returns"){
    fc[1] <- first_close * fc[1] # first value replaced with actual price
    fcf <- cumprod(fc.ret) # cumulative product of returns is price
    upper[1] <- first_close * upper[1] ; lower[1] <- first_close * lower[1]
    upper_f <- cumprod(upper_f); lower_f <- cumprod(lower_f)
  } else if(transformation == "Log Returns"){
    fc <- exp(fc.ser) # remove log 
    fc[1] <- first_close * fc[1] # first value replaced with actual price 
    fcf <- cumprod(fc.ret) # cumulative product of returns is price
    upper <- exp(upper[vtf_b:vtf_e]) ; lower_f <- exp(lower.60[vtf_b:vtf_e]) 
    upper[1] <- first_close * upper[1] ; lower[1] <- first_close * lower[1]
    upper_f <- cumprod(upper_f); lower_f <- cumprod(lower_f)
    
    
  }
  # Place NAs in prior slots to display the forecast correctly
  upper_f <- c(rep(NA,vtf), upper_f); lower_f <- c(rep(NA,vtf), lower_f)
  fcf <- c(rep(NA,vtf), fcf)
  
  result_frame <- data.frame(fcf,upper_f,lower_f)
  return(result_matrix)
}
