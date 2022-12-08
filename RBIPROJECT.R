#PLots for the project
#Understanding the curriencies : Dataset 1
#Graph 1- Composition of foreign exchange reserve-pie chart

#loading the data by saving it into CSV file
dat1<-read.csv(file.choose()) #Usethe data DAT1
dat1
#Removing all the missing values from data
na.omit(dat1)
#We need only the shares and not not that claims so we extract it from data
f_dat1<-dat1[14:22,]
f_dat1
View(f_dat1)
fi_dat1<-na.omit(f_dat1)
View(fi_dat1)
#Calculating the totals and making data appropriate for Pie chart
total<-c()
for(i in 1:8)
{
  total[i]<-sum(fi_dat1[i,2:25])
}
total

dat3<-rbind(fi_dat1[,1],total)
View(dat3)
colnames(dat3)<-c("Shares of US dollars","Shares of euro ","Shares of Japenese Yen","Shares of pounds sterling","Shares of Australian Dollars","Shares of Canadian Dollars","Shares of Swiss francs","Shares of other curriencies")
final_data<-dat3[2,]
View(final_data)
fiinal_data<-as.numeric(final_data)
View(fiinal_data)
pie3D(fiinal_data,radius=1,explode=0.1,col = rainbow(length(fiinal_data)),main="Composition of Foreign exchnge reserve from 2016 to 2021",)
legend(x=0.5,y=1.1,c("US Dollar","Euro","Chinese Renminbi","Japense Yen","Pound sterling","Australian Dollar","Canadian Dollar","Swiss Francs"), cex = 0.5, fill = rainbow(length(fiinal_data)))

#Graph 2- Global market Foreign exchange turnover- stacked bar chart

#STACKED BAR PLOT
dat2<-read.csv(file.choose())#Use the data DAT2-a
View(dat2)
library(ggplot2)
ggplot(dat2,aes(x=Years,y=Values,fill=Currency))+geom_bar(stat="identity")+ labs(title = "Stacked Bar Plot")+theme_bw()

ggplot(dat2, aes(x = Years, y = Values, fill = Currency, color = Years)) +
  geom_col(size=0.8) +
  scale_color_manual(values=c("red", "blue")) +
  scale_fill_viridis_d() +
  coord_polar("y")

#Graph 3-International Bond Issuance in Emerging Market- Nested Pie chart

library(ggplot2)
sample_data <- read.csv(file.choose())#Use the data DAT2-a
View(sample_data)
# create nested pie chart using ggplot
ggplot(sample_data, aes(x = factor(Years), y = Values, fill = factor(Currency))) +
  geom_col() +
  scale_x_discrete(limits = c("2020", "2021")) +
  coord_polar("y")+ labs(fill="Currency")+xlab("Years")+ggtitle("International Bond Issuance in Emerging Market")


#Interpreting Results : Dataset 2
#Missing Value Detection
dat1<-read.csv(file.choose())
#For export data set
dat1
dat<-dat1[5:18]
cor(dat)
cor(dat, use="complete.obs")
colnames(dat)<-c('x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13','x14')
dat
symnum(cor(dat, use="complete.obs"))

#Creating the indicator variable
Ind<-function(t)
{
  x<-dim(length(t))
  x[which(!is.na(t))]=1
  x[which(is.na(t))]=0
  return(x)
}
dat$I<-Ind(dat$x10)
dat

#Fitting a regression model of x10 on x11
lm(x10~x6,data=dat)
summary(lm(x10~x6,dat))
#x10= -12.265+1.935x6

#Similarly for other variables
lm(x11~x10,data=dat)
#x11=7.1677+0.3067x10

lm(x12~x11,data=dat)
#x12=13.9433-0.8048x11

lm(x13~x12,data=dat)
#x13=3.0044+0.6017x12

lm(x14~x1,data=dat)
#x14=6.83554-0.04491x1

#Missing value imputation
for(i in 1:11)
{
  if(dat$I[i]== 0)
  {
    dat$x10[i]=-12.265+1.935*dat$x6[i]
    dat$x11[i]=7.1677+0.3067*dat$x10[i]
    dat$x12[i]=13.9433-0.8048*dat$x11[i]
    dat$x13[i]=3.0044+0.6017*dat$x11[i]
   
    
  }
 
}
dat

#Now we transfer this file to excel for further calculations. 
#To file Exports1

#Missing Value Detection
dat1<-read.csv(file.choose())
#For import data set
dat1
dat<-dat1[5:18]
cor(dat)
cor(dat, use="complete.obs")
colnames(dat)<-c('x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13','x14')
dat
symnum(cor(dat, use="complete.obs"))

#Creating the indicator variable
Ind<-function(t)
{
  x<-dim(length(t))
  x[which(!is.na(t))]=1
  x[which(is.na(t))]=0
  return(x)
}
dat$I<-Ind(dat$x10)
dat

#Fitting a regression model of x10 on x11
lm(x10~x3,data=dat)
summary(lm(x10~x3,dat))
#x10=7.845-0.504x3 
#Similarly for other variables
lm(x11~x2,data=dat)
#x11=7.701-0.4817x2

lm(x12~x5,data=dat)
#x12=2.4078+0.5188x5

lm(x13~x12,data=dat)
#x13=1.4155+0.6582x12



#Missing value imputation
for(i in 1:11)
{
  if(dat$I[i]== 0)
  {
    dat$x10[i]=7.845-0.504*dat$x3[i]
    dat$x11[i]=7.701-0.4817*dat$x2[i]
    dat$x12[i]=2.4078+0.5188*dat$x5[i]
    dat$x13[i]=1.4155+0.6582*dat$x12[i]
    
    
  }
  
}
dat

#Visualization

#STACKED BAR PLOT
dat2<-read.csv(file.choose())
View(dat2)
#install.packages("ggplot2")
library(ggplot2)
ggplot(dat2,aes(x=Countries,y=Values_in_billions,fill=Variables))+geom_bar(stat="identity")+ labs(title = "Comparison of Gross Debt Vs Gross Revenue Vs Total Expenditure")+theme_bw()

#MULTIPLE BAR DIAGRAM
dat2<-read.csv(file.choose())
View(dat2)
#install.packages("ggplot2")
library(ggplot2)
ggplot(dat2,aes(x=Countries,y=Values_in_billions,fill=Variables))+geom_bar(stat="identity",position="dodge")+ labs(title = "Comparison of National Savings Vs Expenditure")+theme_bw()

#Time series Analysis

#loading the data
data <- read.csv(file.choose()) #Choose data India-GDP
data

data = subset(data, select = GDP)
data
#Time-Series plot
time_series = ts(data, start = c(2012), end = c(2022), frequency = 24)
plot(time_series)

#Decomposition
d1 = decompose(time_series)
plot(d1)

#Mann Kendall Test for presence of Trend
#install.packages("trend")
library(trend)
mk.test(data$GDP, alternative = "two.sided")

#install.packages("seastests")
library(seastests)

#Test for Seasonality
isSeasonal(data$GDP, test = "combined", freq = 4)

#Autocorrelation function
acf(time_series, lag.max = 250)
pacf(time_series)

#Test for Stationarity
library(tseries)
adf.test(time_series)
kpss.test(time_series)

#Both are rejected, adf concludes its stationary and kpss concludes non stationary
#, hence we write that the series is difference stationary


#install.packages("aTSA")
library(aTSA)

#Holt's linear trend model
#install.packages("fpp2")
library(fpp2)
model = holt(time_series)
model
model$model
x1 = model$fitted
x1
x2 = model$residuals
r=forecast(model,h=10)
f = r$mean
plot(time_series)
lines(x1, col = 'red')
lines(f, col='blue')

d1=diff(time_series,diff=3)
ARIMA = auto.arima(d1)
ARIMA
a = ARIMA$residuals
a
a1=ARIMA$fitted
a1


t = forecast(ARIMA,h=30)
t
plot(t)
