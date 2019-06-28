# Moving Average function



moving_average <- function(series, type, period){
  
  today <- length(series) 
  start_calc <- today - period
  ma <- vector(mode = "numeric", length = today)
  
  if(type == "simple"){
    ma[1:period] = NA
    for(i in period:today){
      ma[i] <- mean(series[i:(i-period+1)]) 
      
    }
  } else if(type == "exponential"){
    multiplier <- (2 / (period + 1))
    ma[period] <- mean(series[period:1])
    for(i in (period+1):today){
      ma[i] <- (series[i] - ma[i-1]) * multiplier + ma[i-1]
      ma[1:(period-1)] = NA    
    }
  } 
    return(ma)
}


