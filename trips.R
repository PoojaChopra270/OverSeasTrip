library(tseries)
library(readxl)
library(fpp2)

######################################################################################
OverseasTrips <- read.csv("E:/Stats/OverseasTrips.csv")
dim (OverseasTrips)
str(OverseasTrips)
tail(OverseasTrips)

OverseasTrips = ts(OverseasTrips[,2],start = 2012,frequency = 4)
plot(OverseasTrips, xlab="Time", ylab="OverseasTrips",  pch=19,  cex.lab = 2, lwd = 3, col = "black" )
axis(side = 2, font = 2)
axis(side = 1, font = 2)
#Plot the series and discuss the main features of the data.
autoplot(OverseasTrips)
seasonplot(OverseasTrips, col=rainbow(12), year.labels=TRUE, lwd = 3, year.labels.left = TRUE) +
  ggtitle("Seasonal plot")


?seasonplot

ggsubseriesplot(OverseasTrips)
axis(side = 2, font = 2)

autoplot(OverseasTrips) +
  autolayer(ma(OverseasTrips, 2)) +
  autolayer(ma(OverseasTrips, 3)) +
  autolayer(ma(OverseasTrips, 5)) +
  autolayer(ma(OverseasTrips, 9)) +
  autolayer(ma(OverseasTrips, 11))


ggtsdisplay(OverseasTrips)


#Seasonal decomposition using decompose() - additive [Not appropriate here]
fit.decadd<-decompose(OverseasTrips, type = "additive")
fit.decadd
summary(fit.decadd)
plot(fit.decadd)

#Seasonal decomposition using decompose() - multiplicative
fit.decmult<-decompose(OverseasTrips,type = "multiplicative")
fit.decmult
summary(fit.decmult)
plot(fit.decmult)

OverseasTrips.hw<- hw(OverseasTrips, seasonal = "additive", h=3)
OverseasTrips.hw
plot(OverseasTrips.hw)
summary(OverseasTrips.hw)

OverseasTrips.hw<- hw(OverseasTrips, seasonal = "multiplicative", h=3)
OverseasTrips.hw
plot(OverseasTrips.hw)
summary(OverseasTrips.hw)

fit1 <- hw(OverseasTrips, seasonal = "additive")
fit2 <- hw(OverseasTrips, seasonal = "multiplicative")
autoplot(OverseasTrips)+
  autolayer(fit1, series = "additive")+
  autolayer(fit1, series = "multiplication")

###Mean
fcast.mean <- meanf(OverseasTrips,h=3)
summary(fcast.mean)
plot(fcast.mean)


#naive model
fcast.naive<-naive(OverseasTrips,h=3)
summary(fcast.naive)
plot(fcast.naive)

#snaive model
fcast.snaive<-snaive(OverseasTrips,h=3)
summary(fcast.snaive)
plot(fcast.snaive)

#### Simple smoothing
nhfit<- ses(OverseasTrips,h=3)
nhfit
round(accuracy(nhfit),2) 
autoplot(nhfit)

##simple
nhfit <- ets(y=OverseasTrips,model="ANN")
forecast(nhfit,3)
summary(nhfit)

##Double
nhfit <- ets(y=OverseasTrips,model="AAN")
forecast(nhfit,3)
summary(nhfit)
res<-round(accuracy(nhfit),3) 

#
nhfit <- ets(y=OverseasTrips,model="AAA")
forecast(nhfit,3)
summary(nhfit)
res<-round(accuracy(nhfit),3) 

##Auto
nhfit <- ets(y=OverseasTrips,model="ZZZ",h=3)
f.nhfit <- forecast(nhfit,3)
summary(nhfit)
plot(f.nhfit)
autoplot(f.nhfit)
round(accuracy(nhfit),3)  #RMSE=7395.984

fcast.holt <-holt(Expenditure, h=3)
plot(fcast.holt)


# Starting Arima model here
#Check the order of difference required
ndiffs(OverseasTrips)
nsdiffs(OverseasTrips)


#Plot the differenced Nile Time Series
data_d <- diff(diff(log(OverseasTrips)))
plot(data_d)
plot(OverseasTrips)


#Assess stationarity of the differenced series
adf.test(OverseasTrips)


#ACF/PACF plots. Choosing p and q
Acf(data_d) ##
Pacf(data_d) ## 

#auto ARIMA function
auto_fit <- auto.arima(OverseasTrips)
auto_fit
summary(auto_fit) 
Box.test(auto_fit$residuals, type="Ljung-Box")
checkresiduals(auto_fit)
accuracy(auto_fit)



