dat1<-read.csv(file.choose())
dat1
pairs(dat1, pch=15, lower.panel = NULL, main="Scatter Plot matrix")
attach(dat1)
cor(dat1)
model<-lm(Foreign.exchange.turnover~GDP_current.prices+Total.Investment+Gross.national.savings+Gross.Debt+Imports+Population)
model
summary(model)
