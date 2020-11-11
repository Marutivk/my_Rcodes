install.packages("Metrics")
install.packages("readxl")
library(Metrics)
library(readxl)

setwd("F:\\Data Science Course\\My Assignments\\10 Forecasting")

AirData<-read_xlsx(choose.files(),1)

View(AirData)

dim.data.frame(AirData)

plot(AirData$Passengers, type = "l")

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )

colnames(X)<-month.abb # Assigning month names 
View(X)
Airdata_df<-cbind(AirData,X)
View(Airdata_df)

Airdata_df["t"]<- 1:96
View(Airdata_df)

Airdata_df["log_passenger"]<-log(Airdata_df["Passengers"])
Airdata_df["t_square"]<-Airdata_df["t"]*Airdata_df["t"]
View(Airdata_df)

# Data Partitioning

Air_train<-Airdata_df[1:84,]
Air_test<-Airdata_df[85:96,]

############# Liner Model ######################

linear_model<-lm(Passengers~t,data=Air_train)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =Air_test))
rmse_linear<-rmse(Air_test$Passengers,linear_pred$fit)
rmse_linear



######################### Exponential #################################


expo_model<-lm(log_passenger~t,data=Air_train)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=Air_test))
rmse_expo<-rmse(Air_test$Passengers,exp(expo_pred$fit))

rmse_expo

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=Air_train)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=Air_test))
rmse_Quad<-rmse(Air_test$Passengers,Quad_pred$fit)
rmse_Quad


######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=Air_train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=Air_test,interval='predict'))
rmse_sea_add<-rmse(Air_test$Passengers,sea_add_pred$fit)

rmse_sea_add
######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=Air_train)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=Air_test))
rmse_Add_sea_Linear<-rmse(Air_test$Passengers,Add_sea_Linear_pred$fit)

rmse_Add_sea_Linear
######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=Air_train)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=Air_test))
rmse_Add_sea_Quad<-rmse(Air_test$Passengers,Add_sea_Quad_pred$fit)

rmse_Add_sea_Quad 

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data =Air_train)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=Air_test,interval='predict'))
rmse_multi_sea<-rmse(Air_test$Passengers,exp(multi_sea_pred$fit))

rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################
multi_add_sea_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = Air_train)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=Air_test,interval='predict'))
rmse_multi_add_sea<-rmse(Air_test$Passengers,exp(multi_add_sea_pred$fit))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Use entire data : Multi Additive seasonality has least RMSE value
new_model <- lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = Airdata_df )

# Getting residuals 
resid <- residuals(new_model)
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 
k <- arima(resid, order=c(1,0,0))
pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)

