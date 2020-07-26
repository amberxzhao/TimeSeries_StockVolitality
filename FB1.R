library(quantmod)
library(rugarch)
library(fBasics)
library(lmtest)
library(urca)
library(ggplot2)
library(PerformanceAnalytics)
library(FinTS)
library(forecast)
library(strucchange)
library(zoo)
library(TSA)
library(cold)
library(cubature)
library(lmtest)
library(reshape)
library(magrittr)
library(ggpubr)
library(xts)
library(aTSA)

fb = getSymbols("FB", auto.assign=F)
n = dim(fb)[1]
fb1 = fb[805:(n-96),]
dim(fb1)


chartSeries(fb1)

FBts = ts(fb1$FB.Close)
r.FB=diff(log(FBts))*100
r.fb = r.FB[!is.na(r.FB)]



plot(r.FB,main="Log Return of Facebook Daily Closing Price")
plot(r.FB^2,main="Squared Log Return of Facebook Daily Closing Price")

acf(r.FB, main = "ACF of Log Return")
pacf(r.FB, main = "PACF of Log Return")

acf(r.fb^2, main = "ACF of Squared Log Return")
pacf(r.fb^2, main = "PACF of Squared Log Return")

adf = adf.test(r.FB)

auto_model = auto.arima(r.fb)
summary(auto_model)
#ARMA(2,2)
coeftest(auto_model)

#selected arma(0,0)
auto_model2 = auto.arima(r.fb,max.q=1)
summary(auto_model2)
coeftest(auto_model2)
#arma(0,0)


eacf(r.fb)
arma00 = arima(r.fb,order=c(0,0,0),method='ML')
arma00
coeftest(arma00)

 
arma11 = arima(r.fb,order=c(1,0,1),method='ML')
arma11
coeftest(arma11)

#selected ARMR(2,2)

arma202 = arima(r.fb,order=c(2,0,2),method='ML')
arma202
coeftest(arma202)

arma33 = arima(r.fb,order=c(3,0,3),method='ML')
arma33

coeftest(arma33)


#selected ARMA(4,4)
arma44 = arima(r.fb,order=c(4,0,4),method='ML')
arma44
coeftest(arma44)

arma54 = arima(r.fb,order=c(5,0,4),method='ML')
arma54

arma65 = arima(r.fb,order=c(6,0,5),method='ML')
arma65

#ARCH effect test 

model_residuals = residuals(arma11)
ArchTest(model_residuals, lags=23)

summary(pacf(r.fb))
Box.test(r.fb,lag=23,type='Ljung')

var=(r.fb-mean(r.fb))^2
Box.test(var,lag=30,type='Ljung')


plot(density(r.fb))

# ruGARCH
fb_1 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)),
                 mean.model = list(armaOrder=c(2,2)),distribution.model = "std")
fbGarch1 = ugarchfit(spec=fb_1,data = r.fb)
fbGarch1

fb_2 = ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)),
                 mean.model = list(armaOrder=c(2,2)),distribution.model = "std")
fbGarch2 = ugarchfit(spec=fb_2,data = r.fb)
fbGarch2

par(mfrow = c(2,2))
plot(fbGarch1, which=8)
plot(fbGarch1, which=9)
plot(fbGarch1, which=10)
plot(fbGarch1, which=11)

par(mfrow=c(1,1))
cond_volatility <- sigma(fbGarch1)
mean_model_fit <- fitted(fbGarch1)

summary(r.fb)
dim(cond_volatility)

df1 = data.frame(x=seq(1,1006), y =cond_volatility)
df2 = data.frame(x=seq(1,1006), y =mean_model_fit )
df3 = data.frame(x=seq(1,1006), y =r.fb)
ggplot(df3, aes(x, y)) + 
  geom_line(color="grey") +
  geom_line(data = df1, color = "red")+
  geom_line(data = df2, color = "blue")


