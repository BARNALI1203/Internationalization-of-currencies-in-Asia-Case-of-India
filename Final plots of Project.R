#PLots for the project

#Graph 1- Composition of foreign exchange reserve-pie chart

#Library for loading data
library(readxl)
install.packages("plotrix") #Plotting 3D pie chart
library(plotrix)
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

