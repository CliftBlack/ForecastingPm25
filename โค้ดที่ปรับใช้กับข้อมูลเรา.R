#library#
library(nnfor)
library(tseries)
library(forecast)
library("lmtest")

#Getdata#
da <- read.table(file.choose(), header = TRUE, sep = "\t")
pm25 <- da[1:62,2]
data_ts <- ts(pm25, frequency = 12, start = c(2018, 1))
plot(data_ts, type = "l")
title(main="P.M 2.5 in Tambon Wiang Amphoe Mueang Chiang mai")

#Decompose#
de<-decompose(data_ts)
plot(de)
adf_test<-adf.test(data_ts)
adf_test
acf(data_ts)
pacf(data_ts)
data_ts_diff<-diff(data_ts)
plot(data_ts_diff)
adf_test_2<-adf.test(data_ts_diff)
adf_test_2
acf(data_ts_diff)
pacf(data_ts_diff)

#SARIMA#
auto.arima(data_ts,trace=TRUE)
model<-arima(data_ts,orde=c(1,0,0),seasonal=list(order=c(1,1,0),period=12))
coeftest(model)
Box.test(model$resid,lag=4,type="Ljung-Box")
acf_model<-acf(model$residuals,main="Correlogram")
print(summary(model))
fo<-forecast(model,h=12)
plot(fo)


#ANN#
set.seed(999999)
modelANN1=mlp(data_ts,hd=1)
modelANN2=mlp(data_ts,hd=2)

#SARIMA-ANN#
set.seed(999999)
A<-fitted(model)
B<-residuals(model)
modelres_ANN1=mlp(model$residuals,hd=1)
modelres_ANN2=mlp(model$residuals,hd=2)



#checkMSE#
modelres_ANN1
modelres_ANN2



model_ARIMA_ANN=fitted(model)+modelres_ANN2$fitted
fo_ARIMA_ANN=forecast(model_ARIMA_ANN,h=12)
fo_ARIMA_ANN
plot(fo_ARIMA_ANN,type = "l", main = "SARIMA-ANN Forecast")
ACC_ARIMA_ANN=accuracy(fo_ARIMA_ANN)
ACC_ARIMA_ANN


#SARIMA-ANN-REG#
fit.ARIMA=fitted(model)
fit.ANN=modelANN2$fitted
datamix<-cbind(data_ts,fit.ARIMA,fit.ANN)
reg<-lm(data_ts~0+fit.ARIMA+fit.ANN,data=datamix)
reg

A1=forecast(model)
A2=forecast(modelANN2)
newtest=cbind(data_ts,fit.ARIMA=A1$mean,fit.ANN=A2$mean)

forannarimareg<-predict(reg,newtest,h=12)
forannarimareg
plot(forannarimareg)
ANN_ARIMA_ANN_REG=accuracy(reg)
ANN_ARIMA_ANN_REG
