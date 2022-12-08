# Regression with dummy variables
df = data.frame(
  income=c(45000, 48000, 54000, 57000, 65000, 69000,
                          78000, 83000, 98000, 104000, 107000),
  age=c(23, 25, 24, 29, 38, 36, 40, 59, 56, 64, 53),
  status=c('Single', 'Single', 'Single', 'Single',
                          'Married', 'Single', 'Married', 'Divorced',
                          'Divorced', 'Married', 'Married'))

# create dummy variables
married = ifelse(df$status == 'Married', 1, 0)
divorced = ifelse(df$status == 'Divorced', 1, 0)
Data=cbind(df[,-3],married,divorced)

reg_mod=lm(income ~ age + married + divorced, data=Data)
summary(reg_mod)

# Outlier detection in regression
cooksD <- cooks.distance(reg_mod)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

#calculate DFFITS for each observation in the model
dffits <- as.data.frame(dffits(reg_mod))
dfbetas <- as.data.frame(dfbetas(reg_mod))
rowSums(abs(dfbetas))
plot(reg_mod)
###################################################
rm(list=ls()) #Removes all items in Environment!
library(lmtest) #for coeftest() and bptest().
library(broom) #for glance() and tidy()
#library(PoEdata) #for PoE4 datasets
library(car) #for hccm() robust standard errors
#library(sandwich)
#library(knitr)
#library(stargazer)

kable(tidy(bptest(reg_mod)), 
      caption="Breusch-Pagan heteroskedasticity test")
# more options Goldfeld-Quandt
summary(reg_mod)
res=reg_mod$residuals
pred=reg_mod$fitted.values
plot(res,pred)


#library(PoEdata)
# http://www.principlesofeconometrics.com/poe5/poe5csv.html
data("food", package="PoEdata")
food=read.csv(file.choose())
Reg_mod2 <- lm(food_exp~income, data=food)
kable(tidy(bptest(Reg_mod2)), 
      caption="Breusch-Pagan heteroskedasticity test")
res=Reg_mod2$residuals
pred=Reg_mod2$fitted.values
plot(pred,res)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

########################
# Dealing with heteroscadesticity
library(nlme) # GLS
m2 = gls(food_exp~income, data=food, weights=varPower())
plot(m2)
###################################
# Multicolinearity
library(monomvn) # hald cement data
cement
model1=lm(y~.,data=cement)
summary(model1)
plot(model1)
################### Identification
x=model.matrix(model1)[,-1]
cor(x)

vif_values=vif(model1) # car library

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)

##################### Remedies 
model2=lm(y~x1+x2,data=cement)
summary(model2)
##### Ridge regression
library(ridge)
Ridge_Reg= linearRidge(y~., data=cement)
summary(Ridge_Reg)

################################################
# Logistic regression
mtcars
head(mtcars)
logistic_reg = glm(vs ~ wt + disp, 
                      data = mtcars, 
                      family = "binomial")
summary(logistic_reg)
exp(logistic_reg$coefficients)
# The Hosmer-Lemeshow test (HL test) is a goodness of fit test for binary classification models
library("ResourceSelection")
hl<-hoslem.test(mtcars$vs,fitted(logistic_reg),g=10)

####################### ROC 
logistic_reg$fitted.values
summary(logistic_reg)
predict_reg = predict(logistic_reg, 
                      mtcars, type = "response")
predict_reg 

predict_reg1 = ifelse(predict_reg >0.5, 1, 0)

table(mtcars$vs, predict_reg)

library("ROCR")  
ROCPred <- prediction(predict_reg, mtcars$vs) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)
legend(.6, .4, auc, title = "AUC", cex = 1)


