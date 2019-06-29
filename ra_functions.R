# Stock tendencies and characteristics

#### day of the week return function ####
week_day_return <- function(series, method){

close <- as.numeric(Cl(series))

return_p <- matrix(ncol=1, nrow=nrow(series)) 
  if (method == "percent return"){
    for (i in 2:length(close)){
      return_p[i] <- (close[i] - close[i-1]) / close[i-1]
    }
  } else if (method == "return"){
    for (i in 2:length(close)){
      return_p[i] <- (close[i] - close[i-1]) 
  }
}
head(return_p)
series$return_p <- return_p
series$return_p[1] <- 0
series <- as.data.frame(series)
series$day_week <- weekdays(as.Date(rownames((series))))


mon <- mean(as.numeric(series$return_p[(which(series$day_week == "Monday"))]))
tue <- mean(as.numeric(series$return_p[(which(series$day_week == "Tuesday"))]))
wed <- mean(as.numeric(series$return_p[(which(series$day_week == "Wednesday"))]))
thu <- mean(as.numeric(series$return_p[(which(series$day_week == "Thursday"))]))
fri <- mean(as.numeric(series$return_p[(which(series$day_week == "Friday"))]))
days <- c(mon,tue,wed,thu,fri)

return(days)
}

#### Monthly Return Function ####
monthly_return <- function(series, method){
  
close <- as.numeric(Cl(series))

return_p <- matrix(ncol=1, nrow=nrow(series)) 
if (method == "percent return"){
  for (i in 2:length(close)){
    return_p[i] <- (close[i] - close[i-1]) / close[i-1]
  }
} else if (method == "return"){
  for (i in 2:length(close)){
    return_p[i] <- (close[i] - close[i-1]) 
  }
}
  
head(return_p)
series$return_p <- return_p
series$return_p[1] <- 0
series <- as.data.frame(series)
series$month <- months(as.Date(rownames((series))))


jan <- mean(as.numeric(series$return_p[(which(series$month == "January"))]))
feb <- mean(as.numeric(series$return_p[(which(series$month == "February"))]))
mar <- mean(as.numeric(series$return_p[(which(series$month == "March"))]))
apr <- mean(as.numeric(series$return_p[(which(series$month == "April"))]))
may <- mean(as.numeric(series$return_p[(which(series$month == "May"))]))
jun <- mean(as.numeric(series$return_p[(which(series$month == "June"))]))
jul <- mean(as.numeric(series$return_p[(which(series$month == "July"))]))
aug <- mean(as.numeric(series$return_p[(which(series$month == "August"))]))
sep <- mean(as.numeric(series$return_p[(which(series$month == "September"))]))
oct <- mean(as.numeric(series$return_p[(which(series$month == "October"))]))
nov <- mean(as.numeric(series$return_p[(which(series$month == "November"))]))
dec <- mean(as.numeric(series$return_p[(which(series$month == "December"))]))
months <- c(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)

return(months)
}




