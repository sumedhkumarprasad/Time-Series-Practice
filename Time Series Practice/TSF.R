rm(list=ls())
gc() 

library(readxl)
demand <-read_excel("DemandAB-1.xlsx",skip=1)
str(demand)

names(demand)[3] <- c("ItemA") 
names(demand)[4] <- c("ItemB") 
summary(demand)

library(imputeTS)

dem_ItA <- ts(demand[,3], start=c(2002,1), end=c(2017,7), frequency=12)
plot(dem_ItA)
statsNA(dem_ItA)
plotNA.distribution(dem_ItA)
a.withoutNA <- na.seadec(dem_ItA)
plotNA.imputations(dem_ItA,a.withoutNA)

dem_ItB <- ts(demand[,4], start=c(2002,1), end=c(2017,7), frequency=12)
plot(dem_ItB)
statsNA(dem_ItB)
plotNA.distribution(dem_ItB)
b.withoutNA <- na.seadec(dem_ItB)
plotNA.imputations(dem_ItB,b.withoutNA)

#storing values back to time series
dem_ItA = a.withoutNA
dem_ItB = b.withoutNA

#plot the two series together
ts.plot(dem_ItA, dem_ItB, gpars = list(col = c("black", "red")),xlab="year", ylab="demand")
legend("topleft", colnames(demand[3:4]), col=1:ncol(demand), lty=1.9, cex=.45)

monthplot(dem_ItA)
boxplot(dem_ItA ~cycle(dem_ItA))

monthplot(dem_ItB)
boxplot(dem_ItB ~cycle(dem_ItB))

ItA_Sea <- decompose(dem_ItA[,1]) 
plot(ItA_Sea)
plot(ItA_Sea$figure, type = "l")

ItB_Sea<-decompose(dem_ItB[,1]) 
plot(ItB_Sea)
plot(ItB_Sea$figure, type = "l")

#plot of trend +remainder vs series
series_names <- c('Deseasoned', 'Actual')
Deseason_ItA <- (ItA_Sea$trend+ItA_Sea$random) 
ts.plot(dem_ItA, Deseason_ItA, col=c("red", "blue"), main="ItemA Demand vs Deseasoned Demand")

Deseason_ItB <- (ItB_Sea$trend+ItB_Sea$random)
ts.plot(dem_ItB, Deseason_ItB, col=c("red", "blue"), main="ItemB Demand vs Deseasoned Demand")

# split the data into train and test sample
DataATrain <- window(dem_ItA, start=c(2002,1), end=c(2015,12), frequency=12)
DataATest <- window(dem_ItA, start=c(2016,1), frequency=12)

DataBTrain <- window(dem_ItB, start=c(2002,1), end=c(2015,12), frequency=12)
DataBTest <- window(dem_ItB, start=c(2016,1), frequency=12)

ItmATrn <- stl(DataATrain[,1], s.window="p") 
ItmBTrn <- stl(DataBTrain[,1], s.window="p")

library(forecast)

fcst.ItA.stl <- forecast(ItmATrn, method="rwdrift", h=19) 
fcst.ItB.stl <- forecast(ItmBTrn, method="rwdrift", h=19)

VecA<- cbind(DataATest,fcst.ItA.stl$mean) 
VecB<- cbind(DataBTest,fcst.ItB.stl$mean)

par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)

ts.plot(VecA, col=c("blue", "red"),xlab="year", ylab="demand", main="Quarterly Demand A: Actual vs Forecast")
MAPEA <- mean(abs(VecA[,1]-VecA[,2])/VecA[,1])
MAPEA
Box.test(fcst.ItA.stl$residuals, type="Ljung-Box")

ts.plot(VecB, col=c("blue", "red"),xlab="year", ylab="demand", main="Quarterly Demand B: Actual vs Forecast")
MAPEB <- mean(abs(VecB[,1]-VecB[,2])/VecB[,1])
MAPEB
Box.test(fcst.ItB.stl$residuals, type="Ljung-Box")

hwA <- HoltWinters(as.ts(DataATrain),seasonal="additive")
hwA
plot(hwA)

hwAForecast <- forecast(hwA, h=19)
VecA1 <- cbind(DataATest,hwAForecast) 
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecA1[,1],VecA1[,2], col=c("blue","red"),xlab="year", ylab="demand", main="Demand A: Actual vs Forecast")
Box.test(hwAForecast$residuals, type="Ljung-Box")

library(MLmetrics)
MAPE(VecA1[,1],VecA1[,2])

#hwB <- HoltWinters(as.ts(DataBTrain),seasonal="additive")
hwB <- ets(DataBTrain)
hwB


hwBForecast <- forecast(hwB, h=19) 
VecB1 <- cbind(DataBTest,hwBForecast) 
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecB1[,1],VecB1[,2], col=c("blue","red"),xlab="year", ylab="demand", main="Demand B: Actual vs Forecast")
Box.test(hwBForecast$residuals, type="Ljung-Box")

MAPE(VecB1[,1],VecB1[,2])

library(tseries)
adf.test(dem_ItA)

diff_dem_ItA <- diff(dem_ItA)
plot(diff_dem_ItA)
adf.test(diff(dem_ItA))

adf.test(dem_ItB)

diff_dem_ItB <- diff(dem_ItB)
plot(diff_dem_ItB)
adf.test(diff(dem_ItB))

#acf test for stationarity
acf(dem_ItA)
acf(diff_dem_ItA)

acf(dem_ItA, lag=15)
acf(diff_dem_ItA, lag=15)

acf(dem_ItB)
acf(diff_dem_ItB)

pacf(dem_ItA)
pacf(diff_dem_ItA)

pacf(dem_ItA, lag=15)
pacf(diff_dem_ItA, lag=15)

acf(dem_ItB, lag=15)
acf(diff_dem_ItB, lag=15)

pacf(dem_ItB)
pacf(diff_dem_ItB)

pacf(dem_ItB, lag=15)
pacf(diff_dem_ItB, lag=15)

ItA.arima.fit.train <- auto.arima(DataATrain, seasonal=TRUE)
ItA.arima.fit.train

plot(ItA.arima.fit.train$residuals)
plot(ItA.arima.fit.train$x,col="blue")
lines(ItA.arima.fit.train$fitted,col="red",main="Demand A: Actual vs Forecast")

MAPE(ItA.arima.fit.train$fitted,ItA.arima.fit.train$x)
acf(ItA.arima.fit.train$residuals)
pacf(ItA.arima.fit.train$residuals)
Box.test(ItA.arima.fit.train$residuals, type = c("Ljung-Box"), fitdf = 0)

ArimafcastA <- forecast(ItA.arima.fit.train, h=19)
VecA2 <- cbind(DataATest,ArimafcastA)
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecA2[,1],VecA2[,2], col=c("blue","red"),xlab="year", ylab="demand",
        main="Demand A: Actual vs Forecast")

ItB.arima.fit.train <- auto.arima(dem_ItB, seasonal=TRUE)
ItB.arima.fit.train

plot(ItB.arima.fit.train$residuals)
plot(ItB.arima.fit.train$x,col="blue")
lines(ItB.arima.fit.train$fitted,col="red", main="Demand B: Actual vs Forecast")
MAPE(ItB.arima.fit.train$fitted,ItB.arima.fit.train$x)
acf(ItB.arima.fit.train$residuals)
pacf(ItB.arima.fit.train$residuals)

Box.test(ItB.arima.fit.train$residuals, lag = 30, type = c("Ljung-Box"), fitdf = 0         )

ArimafcastB <- forecast(ItB.arima.fit.train, h=19)
VecB2 <- cbind(DataBTest,ArimafcastB)
par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(VecB2[,1],VecB2[,2], col=c("blue","red"),xlab="year", ylab="demand",
        main="Demand B: Actual vs Forecast")

ItA.arima.fit <- auto.arima(dem_ItA, seasonal=TRUE)
fcastA <- forecast(ItA.arima.fit, h=19)
plot(fcastA)

ItB.arima.fit <- auto.arima(dem_ItB, seasonal=TRUE)
fcastB <- forecast(ItB.arima.fit, h=19)
plot(fcastB)
