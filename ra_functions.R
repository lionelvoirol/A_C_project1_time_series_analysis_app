#week day return

week_day_return <- function(series, method){
  
  close <- as.numeric(Cl(series))
  return_p <- matrix(ncol=1, nrow=nrow(series)) 
  #method percent return
  if (method == "percent return"){
    for (i in 2:length(close)){
      return_p[i] <- (close[i] - close[i-1]) / close[i-1]
    }
  } 
  #method return
  else if (method == "return"){
    for (i in 2:length(close)){
      return_p[i] <- (close[i] - close[i-1]) 
    }
  }
  
  series$return_p <- return_p
  series$return_p[1] <- 0
  series <- as.data.frame(series)
  series$day_week <- weekdays(as.Date(rownames((series))))
  
  #get the week day expressed in the user system language
  my_dates = c('2007-01-08', '2007-01-09', '2007-01-10', '2007-01-11', '2007-01-12')
  my_days = weekdays(as.Date(my_dates))
  mon <- c(mean(as.numeric(series$return_p[(which(series$day_week == my_days[1]))])), 
           var(as.numeric(series$return_p[(which(series$day_week == my_days[1]))])))
  tue <- c(mean(as.numeric(series$return_p[(which(series$day_week == my_days[2]))])),
           var(as.numeric(series$return_p[(which(series$day_week == my_days[2]))])))
  wed <- c(mean(as.numeric(series$return_p[(which(series$day_week == my_days[3]))])),
           var(as.numeric(series$return_p[(which(series$day_week == my_days[3]))])))
  thu <- c(mean(as.numeric(series$return_p[(which(series$day_week == my_days[4]))])),
           var(as.numeric(series$return_p[(which(series$day_week == my_days[4]))])))
  fri <- c(mean(as.numeric(series$return_p[(which(series$day_week == my_days[5]))])),
           var(as.numeric(series$return_p[(which(series$day_week == my_days[5]))])))
  days <- c(mon,tue,wed,thu,fri) * 100
  
  return(days)
}

#### Monthly Return Function ####
monthly_return <- function(series, method){
  
  close <- as.numeric(Cl(series))
  
  return_p <- matrix(ncol=1, nrow=nrow(series)) 
  #method percent return
  if (method == "percent return"){
    for (i in 2:length(close)){
      return_p[i] <- (close[i] - close[i-1]) / close[i-1]
    }
  } 
  #method return
  else if (method == "return"){
    for (i in 2:length(close)){
      return_p[i] <- (close[i] - close[i-1]) 
    }
  }
  
  series$return_p <- return_p
  series$return_p[1] <- 0
  series <- as.data.frame(series)
  series$month <- months(as.Date(rownames((series))))
  
  my_dates = c('2007-01-08', '2007-02-09', '2007-03-10', '2007-04-11', '2007-05-12', 
               '2007-06-08', '2007-07-08', '2007-08-08', '2007-09-08', '2007-10-08',
               '2007-11-08','2007-12-08')
  
  my_months = months(as.Date(my_dates))
  
  jan <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[1]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[1]))])))
  feb <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[2]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[2]))])))
  mar <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[3]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[3]))])))
  apr <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[4]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[4]))])))
  may <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[5]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[5]))])))
  jun <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[6]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[6]))])))
  jul <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[7]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[7]))])))
  aug <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[8]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[8]))])))
  sep <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[9]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[9]))])))
  oct <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[10]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[10]))])))
  nov <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[11]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[11]))])))
  dec <- c(mean(as.numeric(series$return_p[(which(series$month == my_months[12]))])), 
           var(as.numeric(series$return_p[(which(series$month == my_months[12]))])))
  months <- c(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
  
  return(months) * 100
}

alternative_assets <- function(series, start, finish, method){
  if (method == "percent return"){
    SPX <- getSymbols("^GSPC",auto.assign = FALSE, from = start, to = finish)
    mean_SPX <- mean(na.omit(exp(diff(log(Cl(SPX))))-1))
    var_SPX <- var(na.omit(exp(diff(log(Cl(SPX))))-1))
    GLD <- getSymbols("GLD",auto.assign = FALSE, from = start, to = finish)
    mean_GLD <- mean(na.omit(exp(diff(log(Cl(GLD))))-1))
    var_GLD <- var(na.omit(exp(diff(log(Cl(GLD))))-1))
    BTC <- getSymbols("BTC-USD",auto.assign = FALSE, from = start, to = finish)
    mean_BTC <- mean(na.omit(exp(diff(log(Cl(BTC))))-1))
    var_BTC <- var(na.omit(exp(diff(log(Cl(BTC))))-1))
    
    mean_series <- mean(na.omit(exp(diff(log(Cl(series))))-1))
    var_series <- var(na.omit(exp(diff(log(Cl(series))))-1))
  } else if (method == "return"){
    SPX <- getSymbols("^GSPC",auto.assign = FALSE, from = start, to = finish)
    mean_SPX <- mean(na.omit(diff(Cl(SPX))))
    var_SPX <- var(na.omit(diff(Cl(SPX))))
    GLD <- getSymbols("GLD",auto.assign = FALSE, from = start, to = finish)
    mean_GLD <- mean(na.omit(diff(Cl(GLD))))
    var_GLD <- var(na.omit(diff(Cl(GLD))))
    BTC <- getSymbols("BTC-USD",auto.assign = FALSE, from = start, to = finish)
    mean_BTC <- mean(na.omit(diff(Cl(BTC))))
    var_BTC <- var(na.omit(diff(Cl(BTC))))
    
    mean_series <- mean(na.omit(diff(Cl(series))))
    var_series <- var(na.omit(diff(Cl(series))))
  }
  data <- c(mean_series, var_series, mean_SPX, var_SPX, mean_GLD, var_GLD, mean_BTC, var_BTC)
  return(data)
}
  