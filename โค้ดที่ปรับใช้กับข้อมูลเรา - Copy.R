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
modelANN3=mlp(data_ts,hd=3)
modelANN4=mlp(data_ts,hd=4)
modelANN5=mlp(data_ts,hd=5)
modelANN6=mlp(data_ts,hd=6)
modelANN7=mlp(data_ts,hd=7)
modelANN8=mlp(data_ts,hd=8)
modelANN9=mlp(data_ts,hd=9)
modelANN10=mlp(data_ts,hd=10)
modelANN11=mlp(data_ts,hd=11)
modelANN12=mlp(data_ts,hd=12)
modelANN13=mlp(data_ts,hd=13)
modelANN14=mlp(data_ts,hd=14)
modelANN15=mlp(data_ts,hd=15)
modelANN16=mlp(data_ts,hd=16)
modelANN17=mlp(data_ts,hd=17)
modelANN18=mlp(data_ts,hd=18)
modelANN19=mlp(data_ts,hd=19)
modelANN20=mlp(data_ts,hd=20)
modelANN21=mlp(data_ts,hd=21)
modelANN22=mlp(data_ts,hd=22)
modelANN23=mlp(data_ts,hd=23)
modelANN24=mlp(data_ts,hd=24)
modelANN25=mlp(data_ts,hd=25)
modelANN26=mlp(data_ts,hd=26)
modelANN27=mlp(data_ts,hd=27)
modelANN28=mlp(data_ts,hd=28)
modelANN29=mlp(data_ts,hd=29)
modelANN30=mlp(data_ts,hd=30)
modelANN31=mlp(data_ts,hd=31)
modelANN32=mlp(data_ts,hd=32)
modelANN33=mlp(data_ts,hd=33)
modelANN34=mlp(data_ts,hd=34)
modelANN35=mlp(data_ts,hd=35)
modelANN36=mlp(data_ts,hd=36)
modelANN37=mlp(data_ts,hd=37)
modelANN38=mlp(data_ts,hd=38)
modelANN39=mlp(data_ts,hd=39)
modelANN40=mlp(data_ts,hd=40)


#SARIMA-ANN#
set.seed(999999)
A<-fitted(model)
B<-residuals(model)
modelres_ANN1=mlp(model$residuals,hd=1)
modelres_ANN2=mlp(model$residuals,hd=2)
modelres_ANN3=mlp(model$residuals,hd=3)
modelres_ANN4=mlp(model$residuals,hd=4)
modelres_ANN5=mlp(model$residuals,hd=5)
modelres_ANN6=mlp(model$residuals,hd=6)
modelres_ANN7=mlp(model$residuals,hd=7)
modelres_ANN8=mlp(model$residuals,hd=8)
modelres_ANN9=mlp(model$residuals,hd=9)
modelres_ANN10=mlp(model$residuals,hd=10)
modelres_ANN11=mlp(model$residuals,hd=11)
modelres_ANN12=mlp(model$residuals,hd=12)
modelres_ANN13=mlp(model$residuals,hd=13)
modelres_ANN14=mlp(model$residuals,hd=14)
modelres_ANN15=mlp(model$residuals,hd=15)
modelres_ANN16=mlp(model$residuals,hd=16)
modelres_ANN17=mlp(model$residuals,hd=17)
modelres_ANN18=mlp(model$residuals,hd=18)
modelres_ANN19=mlp(model$residuals,hd=19)
modelres_ANN20=mlp(model$residuals,hd=20)
modelres_ANN21=mlp(model$residuals,hd=21)
modelres_ANN22=mlp(model$residuals,hd=22)
modelres_ANN23=mlp(model$residuals,hd=23)
modelres_ANN24=mlp(model$residuals,hd=24)
modelres_ANN25=mlp(model$residuals,hd=25)
modelres_ANN26=mlp(model$residuals,hd=26)
modelres_ANN27=mlp(model$residuals,hd=27)
modelres_ANN28=mlp(model$residuals,hd=28)
modelres_ANN29=mlp(model$residuals,hd=29)
modelres_ANN30=mlp(model$residuals,hd=30)
modelres_ANN31=mlp(model$residuals,hd=31)
modelres_ANN32=mlp(model$residuals,hd=32)
modelres_ANN33=mlp(model$residuals,hd=33)
modelres_ANN34=mlp(model$residuals,hd=34)
modelres_ANN35=mlp(model$residuals,hd=35)
modelres_ANN36=mlp(model$residuals,hd=36)
modelres_ANN37=mlp(model$residuals,hd=37)
modelres_ANN38=mlp(model$residuals,hd=38)
modelres_ANN39=mlp(model$residuals,hd=39)
modelres_ANN40=mlp(model$residuals,hd=40)



#checkMSE#
modelres_ANN1
modelres_ANN2
modelres_ANN3
modelres_ANN4
modelres_ANN5
modelres_ANN6
modelres_ANN7
modelres_ANN8
modelres_ANN9
modelres_ANN10
modelres_ANN11
modelres_ANN12
modelres_ANN13
modelres_ANN14
modelres_ANN15
modelres_ANN16
modelres_ANN17
modelres_ANN18
modelres_ANN19
modelres_ANN20
modelres_ANN21
modelres_ANN22
modelres_ANN23
modelres_ANN24
modelres_ANN25
modelres_ANN26
modelres_ANN27
modelres_ANN28
modelres_ANN29
modelres_ANN30
modelres_ANN31
modelres_ANN32
modelres_ANN33
modelres_ANN34
modelres_ANN35
modelres_ANN36
modelres_ANN37
modelres_ANN38
modelres_ANN39
modelres_ANN40




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
