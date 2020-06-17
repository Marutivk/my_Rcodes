boxplot(NewspaperData$sunday, col="dodgerblue4")
boxplot(NewspaperData$sunday, col="dodgerblue4")
colnames(NewspaperData)
model<- lm(sunday~daily,data =NewspaperData[,-1])
summary(model)
new_daily=data.frame(daily=c(391.952))
sun1=predict(model,new_daily)
sun1
pred<-predict(model)
pred
finaldata<-data.frame(NewspaperData,pred,"Error"= NewspaperData$sunday-pred)
finaldata
