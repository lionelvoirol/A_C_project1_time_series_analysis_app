# Arima-Garch forecast function


GLD <- getSymbols.yahoo("GLD", auto.assign = F)



stationarity_test <- function(series){
  close <- as.numeric(Cl(series))
  test_1 <- adf.test(close)
  returns <- na.omit(diff(close))
  test_2 <- adf.test(returns)
  log_returns <- diff(log(close))
  test_3 <- adf.test(log_returns)
  
  results <- c(test_1$p.value, test_2$p.value , test_3$p.value) 
  
  return(results)
}


