rm(list=ls(all=TRUE))
library(astsa)
library(forecast)
# read data to R variable and set it up to start working
data<-read.csv("COV19_Defunciones_Pru.csv")
#Pull out national incidence column data
defunciones<-data$defuncionesconf
data$fecha <- as.Date(data$fecha, "%d/%m/%Y")
ts.defunciones<-ts(defunciones)
acf(ts.defunciones)
pacf(ts.defunciones)

plot(log(ts.defunciones))
plot(diff(log(ts.defunciones)))
plot(diff(diff(log(ts.defunciones))))

acf(diff(diff(log(ts.defunciones))))
pacf(diff(diff(log(ts.defunciones))))

Box.test(ts.defunciones, lag=log(length(ts.defunciones)))
#First Approach (auto arima)
library(forecast)
def<-auto.arima(ts.defunciones)
plot(forecast(def))
forecast(def)
accuracy(def)

