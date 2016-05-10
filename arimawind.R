# Time Series ARIMA Models in R
# install.packages("tseries")
library(tseries)
library("forecast")


# read data file
mydata<- read.csv("winddata-2yrs.csv")
mydata = ts(mydata[,2],start = c(2007,1))

Y <- mydata
d.Y <- diff(Y)

# describe the ts object and plot data and first differential
summary(Y)
summary(d.Y)
plot(Y)
plot(d.Y)

# ACF and PACF
acf(Y)
pacf(Y)
acf(d.Y)
pacf(d.Y)

# using auto.arima find out the AR,MA parameters
mydata.arima_auto = auto.arima(Y, approximation=TRUE,trace=TRUE,D=1)

mydata.arima311 <- arima(mydata, order = c(3,1,1))

# do forecast
fit.t <- Arima(Y, order = c(3,1,1),include.drift=TRUE)
forecast.t <- plot(forecast(fit.t, h=3000, level=c(80,95)))
