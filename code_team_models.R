#### PACKAGES ####
rm(list=ls())
require(forecast)
require(rugarch)
require(quantmod)


#### RETRIEVAL OF DATA ####
startDate <- as.Date("2010-01-01")
endDate <- as.Date(as.POSIXlt(Sys.Date()))
getSymbols("AAPL",src="yahoo",from=startDate, to=endDate, periodicity = "daily")
stock <- na.omit(AAPL)

Close = Cl(stock)

candleChart(stock, subset='last 50 days')
plot(Close)

# Cl function retrieves close from the OHLC, RETS gives us the returns
Rets = diff( log( Cl( AAPL ) ) ) 
Close = as.ts(Cl(AAPL))
Rets <- as.ts(na.omit(Rets))

ar_term <- 2
ma_term <- 2
arch_term <- 2
garch_term <- 2

#### SPECIFICATIONS OF THE MODEL #### 
ugarchspecs <- ugarchspec(variance.model=list(garchOrder = c(arch_term,garch_term)),mean.model=list(
  armaOrder=c(ar_term,ma_term),arfima = FALSE), distribution="std")

#### ESTIMATION OF MODEL ####
ugarch <- ugarchfit(ugarchspecs,Rets)
infocriteria(ugarch)
# Squared Residuals (Epsilon squared)
resid2 <- (ugarch@fit$residuals)^2
resid <- (ugarch@fit$residuals)
# Variance (Sigma squared)
var <- ugarch@fit$var
# Yt model
Yt <- ugarch@fit$fitted.values

#### VISUALIZATION: STOCK ####
#### FORECASTS ####
Vtf <- 50 # Visualization time frame
Vtf_d <- 5 # Forecast duration

Vtf_fb <- Vtf + 1 # Forecast begin 
Vtf_fe <- Vtf + Vtf_d # Forecast end 

fc <- ugarchforecast(ugarch, n.ahead = Vtf_d)
fc
fc.sig <- fc@forecast$sigmaFor
fc.ser <- fc@forecast$seriesFor
plot(fc.sig, type = "l") # Forecast of volatility 
plot(fc.ser, type = "l") # Forecast of the process

# get the last x observations for the variance (Sigma squared)
var.tail <- c(tail(var,Vtf),rep(NA,Vtf_d))  
# get the last x observations for residuals (Epsilon squared)
# plot of the volatility, its forecast versus the residuals of the model
resid2.tail <- c(tail(resid2,Vtf),rep(NA,Vtf_d))  
fc2 <- c(rep(NA,Vtf),(fc.sig)^2)
plot(resid2.tail, type="l")
lines(fc2, col="purple")
lines(var.tail, col="brown")
# series (returns) forecast
Yt.tail <- c(tail(Yt,Vtf),rep(NA,Vtf_d))
Tail.Ret.tail <- c(tail(Close,Vtf),rep(NA,Vtf_d))
fc3 <- c(rep(NA,Vtf),(fc.ser))

# Different intervals to be displayed

upper.60 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] + 0.842 * (fc.sig)) # 0.842 is crit for 60% C.I
lower.60 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] - 0.842 * (fc.sig))

upper.70 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] + 1.036 * (fc.sig)) # 1.036 -> 70% C.I
lower.70 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] - 1.036 * (fc.sig))

upper.80 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] + 1.282 * (fc.sig)) # 1.282 -> 80% C.I
lower.80 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] - 1.282 * (fc.sig))

upper.90 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] + 1.645 * (fc.sig)) # 1.282 -> 90% C.I
lower.90 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] - 1.645 * (fc.sig))

# Series forecast transformed back to daily close
# forecast price
# Remove the "logness"
fc.ret <- exp(fc.ser) 
# Feed it the first real price
fc.ret[1] <- Close[length(Close)] * fc.ret[1] 
# Translation from Returns to Price
fcp <- cumprod(fc.ret) 

# Forecast prices bounds confidence intervals
## 60
# Remove the "logness"
fc.ret.upper.60 <- exp(upper.60[Vtf_fb:Vtf_fe]) ; fc.ret.lower.60 <- exp(lower.60[Vtf_fb:Vtf_fe]) 
# Feed it the first real upper/lower interval
fc.ret.upper.60[1] <- Close[length(Close)] * fc.ret.upper.60[1] ; fc.ret.lower.60[1] <- Close[length(Close)] * fc.ret.lower.60[1]
# Translation from Returns to Price, compounding the returns
fcp.upper.60 <- cumprod(fc.ret.upper.60); fcp.lower.60 <- cumprod(fc.ret.lower.60)
# Place NAs in prior slots to display the forecast correctly
fcp.upper.60 <- c(rep(NA,Vtf), fcp.upper.60); fcp.lower.60 <- c(rep(NA,Vtf), fcp.lower.60)

## 70
# Remove the "logness"
fc.ret.upper.70 <- exp(upper.70[Vtf_fb:Vtf_fe]) ; fc.ret.lower.70 <- exp(lower.70[Vtf_fb:Vtf_fe]) 
# Feed it the first real upper/lower interval
fc.ret.upper.70[1] <- Close[length(Close)] * fc.ret.upper.70[1] ; fc.ret.lower.70[1] <- Close[length(Close)] * fc.ret.lower.70[1]
# Translation from Returns to Price, compounding the returns
fcp.upper.70 <- cumprod(fc.ret.upper.70); fcp.lower.70 <- cumprod(fc.ret.lower.70)
# Place NAs in prior slots to display the forecast correctly
fcp.upper.70 <- c(rep(NA,Vtf), fcp.upper.70); fcp.lower.70 <- c(rep(NA,Vtf), fcp.lower.70)

## 80
# Remove the "logness"
fc.ret.upper.80 <- exp(upper.80[Vtf_fb:Vtf_fe]) ; fc.ret.lower.80 <- exp(lower.80[Vtf_fb:Vtf_fe]) 
# Feed it the first real upper/lower interval
fc.ret.upper.80[1] <- Close[length(Close)] * fc.ret.upper.80[1] ; fc.ret.lower.80[1] <- Close[length(Close)] * fc.ret.lower.80[1]
# Translation from Returns to Price, compounding the returns
fcp.upper.80 <- cumprod(fc.ret.upper.80); fcp.lower.80 <- cumprod(fc.ret.lower.80)
# Place NAs in prior slots to display the forecast correctly
fcp.upper.80 <- c(rep(NA,Vtf), fcp.upper.80); fcp.lower.80 <- c(rep(NA,Vtf), fcp.lower.80)

## 90
# Remove the "logness"
fc.ret.upper.90 <- exp(upper.90[Vtf_fb:Vtf_fe]) ; fc.ret.lower.90 <- exp(lower.90[Vtf_fb:Vtf_fe]) 
# Feed it the first real upper/lower interval
fc.ret.upper.90[1] <- Close[length(Close)] * fc.ret.upper.90[1] ; fc.ret.lower.90[1] <- Close[length(Close)] * fc.ret.lower.90[1]
# Translation from Returns to Price, compounding the returns
fcp.upper.90 <- cumprod(fc.ret.upper.90); fcp.lower.90 <- cumprod(fc.ret.lower.90)
# Place NAs in prior slots to display the forecast correctly
fcp.upper.90 <- c(rep(NA,Vtf), fcp.upper.90); fcp.lower.90 <- c(rep(NA,Vtf), fcp.lower.90)

fcp <- cumprod(fc.ret)
fcp <- c(rep(NA,Vtf), fcp)
Tail.Cl.tail <- c(tail(Close,Vtf),rep(NA,Vtf_d))
plot(Tail.Cl.tail, type="l",
     ylim=c( min(min(na.omit(Tail.Cl.tail),min(na.omit(fcp.lower.90)))) , max(max(na.omit((Tail.Cl.tail)),max(na.omit(fcp.upper.90))))),
     main = "Different Prediction Intervals",
     ylab = "Stock Price",
     xlab = "Trading Days since 22nd of October 2018")
legend(x = "bottomleft",  legend = c("Forecast","60%","70%","80%","90%"),col=c("purple","red","darkcyan","green","blue"), lty=2)
lines(fcp, col ="purple", lty = 2)
lines(fcp.upper.60, col ="red", lty = 2)
lines(fcp.lower.60, col ="red", lty = 2)
lines(fcp.upper.70, col ="darkcyan", lty = 2)
lines(fcp.lower.70, col ="darkcyan", lty = 2)
lines(fcp.upper.80, col ="green", lty = 2)
lines(fcp.lower.80, col ="green", lty = 2)
lines(fcp.upper.90, col ="blue", lty = 2)
lines(fcp.lower.90, col ="blue", lty = 2)
