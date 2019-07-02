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
  mon <- mean(as.numeric(series$return_p[(which(series$day_week == my_days[1]))]))
  tue <- mean(as.numeric(series$return_p[(which(series$day_week == my_days[2]))]))
  wed <- mean(as.numeric(series$return_p[(which(series$day_week == my_days[3]))]))
  thu <- mean(as.numeric(series$return_p[(which(series$day_week == my_days[4]))]))
  fri <- mean(as.numeric(series$return_p[(which(series$day_week == my_days[5]))]))
  days <- c(mon,tue,wed,thu,fri)
  
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
  
  jan <- mean(as.numeric(series$return_p[(which(series$month == my_months[1]))]))
  feb <- mean(as.numeric(series$return_p[(which(series$month == my_months[2]))]))
  mar <- mean(as.numeric(series$return_p[(which(series$month == my_months[3]))]))
  apr <- mean(as.numeric(series$return_p[(which(series$month == my_months[4]))]))
  may <- mean(as.numeric(series$return_p[(which(series$month == my_months[5]))]))
  jun <- mean(as.numeric(series$return_p[(which(series$month == my_months[6]))]))
  jul <- mean(as.numeric(series$return_p[(which(series$month == my_months[7]))]))
  aug <- mean(as.numeric(series$return_p[(which(series$month == my_months[8]))]))
  sep <- mean(as.numeric(series$return_p[(which(series$month == my_months[9]))]))
  oct <- mean(as.numeric(series$return_p[(which(series$month == my_months[10]))]))
  nov <- mean(as.numeric(series$return_p[(which(series$month == my_months[11]))]))
  dec <- mean(as.numeric(series$return_p[(which(series$month == my_months[12]))]))
  months <- c(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
  
  return(months)
}