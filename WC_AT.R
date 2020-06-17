boxplot(WC_AT$Waist, col="dodgerblue4")
colnames(WC_AT)
model<- lm(Waist~AT,data =WC_AT)
summary(model)
new_AT=data.frame(AT=c(74.75))
AT=predict(model,new_AT)
AT
pred<-predict(model)
pred
finaldata<-data.frame(WC_AT,pred,"Error"= WC_AT$AT-pred)
finaldata
