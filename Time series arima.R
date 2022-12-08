library(fpp2)
library(forecast)
#Load Air passengers data
data("AirPassengers")
frequency(AirPassengers)
#Plot the data
ts.plot(AirPassengers)

#This will fit in line
abline(reg=lm(AirPassengers~time(AirPassengers)))

#ACF
acf(AirPassengers)

#Fitting the AR(1) Model to the time series
AR=arima(AirPassengers,order=c(1,0,0))
print(AR)

#plotting the series along with fitted values
ts.plot(AirPassengers)
AR_fit=AirPassengers-residuals(AR)
points(AR_fit,type="l",col=2,lty=2)

#Using predict() to make 1 step forecast
predict_AR=prdict(AR)
#Obtain

#Fitting the  MA model to AIR Pasengers
MA=arima(AirPassengers,order=c(0,0,1))
print(MA)

#plotting  the series along with MA fitted values
ts.plot(AirPassengers)
MA_fit=AirPassengers-resid(MA)
points(MA_fit,type="l",col=2,lty=2)


#A comprehensive model ARIMA(p,d,q)
arimaAP=auto.arima(AirPassengers)
arimaAP

