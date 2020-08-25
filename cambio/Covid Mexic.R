rm(list=ls(all=TRUE))
library(astsa)
# read data to R variable and set it up to start working
data<-read.csv("COV19_Defunciones.csv")
#Pull out national incidence column data
defunciones<-data$defuncionesconf
data$fecha <- as.Date(data$fecha, "%d/%m/%Y")
ts.defunciones<-ts(defunciones)



#Plots
par( mfrow=c(1,2) )
hist(defunciones, main="Covid-19, México: Defunciones positivas",
     xlab="incidencias")
qqnorm(defunciones,main="Covid-19, México: Defunciones positivas")
qqline(defunciones)

#ACF
par( mfrow=c(2,1) )
plot.ts(ts.defunciones, main="Covid-19, México: Defunciones positivas",
        xlab="year", ylab="incidencias")
acf(ts.defunciones, main="ACF: Covid-19, México: Defunciones positivas")

#First Approach (auto arima)
library(forecast)
auto.arima(defunciones)

#How to choose "alpha" (Simple Exponential Smoothing)
SSE=NULL
n = length(defunciones)
alpha.values = seq( .001, .999, by=0.001)
number.alphas = length(alpha.values)
for( k in 1:number.alphas ) {
  forecast.values=NULL
  alpha = alpha.values[k]
  forecast.values[1] = defunciones[1]
  for( i in 1:n ) {
    forecast.values[i+1] = alpha*defunciones[i] + (1-alpha)*forecast.values[i]
  }
  SSE[k] = sum( (defunciones - forecast.values[1:n])^2 )
}
plot(SSE~alpha.values, main="Optimal alpha value Minimizes SSE")
index.of.smallest.SSE = which.min(SSE) #returns position 24
alpha.values[which.min(SSE)] #returns 0.973

# DIY Forecasting
alpha=.171 #increase alpha for more rapid decay
forecast.values = NULL #establish array to store forecast values
n = length(defunciones)
#naive first forecast
forecast.values [1] = defunciones[1]
#loop to create all forecast values
for( i in 1:n ) {
  forecast.values [i+1] = alpha*defunciones[i] + (1-alpha)* forecast.values [i]
}
paste("forecast for time",n+1," = ", forecast.values [n+1])

HoltWinters(ts.defunciones, beta=FALSE, gamma=FALSE)
plot(HoltWinters(ts.defunciones, beta=FALSE, gamma=FALSE))
