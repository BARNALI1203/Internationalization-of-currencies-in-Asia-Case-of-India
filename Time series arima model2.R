#set random number seed
set.seed(123)
#Length of time series 
m=200
#initialize {yt} and {wt}
w=rnorm(n=m,mean=0,sd=1)
y=w
for(t in 2:m)
{
  y[t]=y[t-1]+w[t]
}
plot(y,type="l")
plot.ts(y)

library(forecast)
library(fpp2)
rw=arima.sim(model=list(order=c(0,1,0)),n=200)
autoplot(rw)
acf(rw)   
pacf(rw)

#Alternatively 
RW=arima.sim(model=list(order=c(0,1,0)),n=200)
autoplot(RW)
acf(RW)   
pacf(RW)
diff1=diff(RW)
autoplot(diff1)
mean(diff1)
sd(diff1)
#Random walk wanders up and  and down around the mean
RW_drift=arima.sim(model=list(order=c(0,1,0)),n=200,mean=1,sd=5)
autoplot(RW_drift,main="random walk with drift")

library(tseries)
adf.test(RW)
kpss.test(RW)

#Use Google stock price data
library(urca)
ts.plot(goog)
goog%>%ur.kpss()%>%summary()
#Alternatively
kpss.test(goog)
#Use differenced series
goog%>%diff()%>%ur.kpss()%>%summary()
ts.plot(diff(goog))

#Can aslo be triec with air Passengers data

#US Consumption
autoplot(uschange[,"Consumption"])+xlab("Year")+ylab("Quarterly percentage change")
fit=auto.arima(uschange[,"Consumption"])
fit


fit %>% forecast(h=30) %>% autoplot
ggAcf(uschange[,"Consumption"],main="ACF plot for US Consumption")
ggPacf(uschange[,"Consumption"],main="PACF plot for US Consumption")
