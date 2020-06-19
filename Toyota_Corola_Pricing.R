install.packages("car")
library(car)
Toyota1<-Toyota[, -c(1,2,5,6,8,10, 11,12,15,19:38)]
Toyota1
pairs(Toyota1)
cor(Toyota1)
price_model<-lm(Toyota1$Price~., data=Toyota1)
summary (price_model)
car::vif(price_model)
library(MASS)
stepAIC(price_model)
plot(price_model)
residualPlots(price_model)
avPlots(price_model)
qqPlot(price_model)
influenceIndexPlot(price_model)

#Iteration 1 for Optimizing the model
Toyota1["cc2"]<-Toyota1$cc*Toyota1$cc
Toyota1["Age2"]<-Toyota1$Age_08_04*Toyota1$Age_08_04
Toyota2<-Toyota1[-c(81,222),]
pairs(Toyota2)
cor(Toyota2)
price_model1<-lm(Toyota2$Price~., data=Toyota2)
summary (price_model1)
car::vif(price_model1)
library(MASS)
stepAIC(price_model1)
plot(price_model1)
residualPlots(price_model1)
avPlots(price_model1)
qqPlot(price_model1)
influenceIndexPlot(price_model1)

##Iteration 2 for Optimizing the model
Toyota1["cc2"]<-Toyota1$cc*Toyota1$cc
Toyota1["Age2"]<-Toyota1$Age_08_04*Toyota1$Age_08_04
Toyota3<-Toyota1[-c(81,222),-c(2,5)]
pairs(Toyota3)
cor(Toyota3)
price_model2<-lm(Toyota3$Price~., data=Toyota3)
summary (price_model2)
car::vif(price_model2)
library(MASS)
stepAIC(price_model2)
plot(price_model2)
residualPlots(price_model2)
avPlots(price_model2)
qqPlot(price_model2)
influenceIndexPlot(price_model2)

##Iteration 3 for Optimizing the model
Toyota1["cc2"]<-Toyota1$cc*Toyota1$cc
Toyota1["Age2"]<-Toyota1$Age_08_04*Toyota1$Age_08_04
Toyota4<-Toyota1[-c(600,959,222,524),-c(5,6,10)]
pairs(Toyota4)
cor(Toyota4)
price_model3<-lm(Toyota4$Price~., data=Toyota4)
summary (price_model3)
car::vif(price_model3)
library(MASS)
stepAIC(price_model3)
plot(price_model3)
residualPlots(price_model3)
avPlots(price_model3)
qqPlot(price_model3)
influenceIndexPlot(price_model3, cxe=.8)
