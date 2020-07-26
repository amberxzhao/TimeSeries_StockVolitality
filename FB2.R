FB = read.csv('/Users/AmberZhao/Desktop/FBclose.csv')
plot(FB,pch='.', ylab = 'Daily Closing Price', main = 'Facebook Daily Closing Price')


dim(FB)
head(FB)
FBts = ts(FB$Close,frequence = 365)
ts.plot(FBts)

r.FB=diff(log(FBts))*100
head(r.FB)

install.packages("LSTS")
library(LSTS)
library(zoo)
library(forecast)
Box.test(r.FB^2,type="Ljung-Box",lag=365)
Box.test(coredata(r.FB),type="Ljung-Box",lag=365)

install.packages("FinTS")
library(FinTS)

ArchTest(r.FB)
Box.test(FBts,type="Ljung-Box",lag=365)

par(mfrow=c(2,1))
plot(r.FB,ylab="log return",main= "Log Return of Daily Close Price"); abline(h=0)
plot(r.FB^2,ylab="squared log return",main='Squared Log Return of Daily Close Price'); abline(h=0)
par(mfrow=c(2,1))
acf(r.FB, main = 'ACF of Log Return')
pacf(r.FB, main = 'PACF of Log Return')

par(mfrow=c(2,1))
acf(r.FB^2, main = 'ACF of Squared Log Return');pacf(r.FB^2, main = 'PACF of Squared Log Return')

par(mfrow=c(1,1))

qqnorm(r.FB);qqline(r.FB)
plot(density(r.FB))


summary(garchFit(~garch(1,1),r.FB))

fit = garchFit(~arma(0,0)+garch(1,1),r.FB)
predict(fit,n.ahead = 60,plot=TRUE)


fit1 = garchFit(~arma(0,0)+garch(1,1),FBts)
predict(fit1,n.ahead = 60,plot=TRUE)

fit2 = garchFit(~garch(1,2),r.FB)
summary(garchFit(~garch(1,2),r.FB))

predict(fit2,n.ahead = 60,plot=TRUE)
