set.seed(43)
data=arima.sim(list(order=c(2,0,0),ar=c(0.7,-0.2)),n=2000)
par(mfrow=c(2,1))
acf(data,main="ACF of AR Data of Second Order")
acf(data,type = "partial",main="PACF of Time Series")
arima(data,order=c(2,0,0),include.mean = FALSE)
m=arima(data,order=c(2,0,0),include.mean = FALSE)
SSE=sum(resid(m)^2)
SSE





rm(list=ls(all=TRUE))
set.seed(500) #500	Seven kingdoms Kent, Essex, Sussex, Wessex, East Anglia, Mercia, and Northumbria.)
data = arima.sim( list(order = c(3,0,0), ar =c( 0.6, -0.1, .4)), n = 5000)

arima(data, order=c(2,0,0), include.mean=FALSE )
arima(data, order=c(3,0,0), include.mean=FALSE )
arima(data, order=c(4,0,0), include.mean=FALSE )



rm(list=ls(all=TRUE))
set.seed(597) # Saint Augustine arrives in England
data = arima.sim( list(order = c(1,0,0), ar = .3), n = 5000)

par(mfrow=c(2,1))
acf(data, main="ACF of Time Series Data")
acf(data, type="partial", main="PACF of Time Series Data")



rm(list=ls(all=TRUE))
set.seed(597) # Saint Augustine arrives in England
data = arima.sim( list(order = c(1,0,0), ar = .3), n = 5000);
arima(data, order=c(1,0,0) );






#ARMA simulation
set.seed(500)
data=arima.sim(list(order=c(1,0,1),ar=0.7,ma=0.2),n=1000000)
par(mfcol=c(3,1))
plot(data,main="ARMA(1,1) Time Series:phi1=0.7,theta1=0.2",xlim=c(0,400))
acf(data,main="Autocorrelation of ARMA(1,1),phi1=0.7,theta1=0.2")
acf(data,type="partial",main="Partial Autocorrelation of ARMA(1,1),phi1=0.7,theta1=0.2")



par(mfcol=c(2,1))
plot(discoveries,main="Time Series of Major Scientific discoveries in a Year")
#for discrete data
stripchart(discoveries,method = "stack",offset = 0.5,at=0.15,pch=19,main="Number of Discoveries Dotplot",xlab = "Number of Major Scientific Discoveries in a Year",ylab="Frequency")
acf(discoveries,main="ACF of Number of Major Scientific Discoveries in a Year")
acf(discoveries,type = "partial",main="PACF of Number of Major Scientific Discoveries in a Year")
p=c(0:3)
q=c(0:3)
result_matrix <- matrix(0, nrow = length(p), ncol = length(q))
for (i in 0:length(p)) {
  for (j in 0:length(q)) {
    result_matrix[i, j] <- arima(discoveries, order=c(i,0,j))$aic
    
  }
}
result_matrix




library(forecast)
auto.arima(discoveries,d=0,approximation = FALSE)

auto.arima(discoveries,d=0,ic="bic",approximation = FALSE)

data = arima.sim( n=1E4, list(ar=.5, ma=.2) )
auto.arima(data)



#Arima(2,1,1) simulation
# parameters
phi=c(.7, .2)
beta=0.5
sigma=3
m=10000

set.seed(5)
Simulated.Arima=arima.sim(n=m,list(order = c(2,1,1), ar = phi, ma=beta))
plot(Simulated.Arima, ylab=' ',main='Simulated time series from ARIMA(2,1,1) process', col='blue', lwd=2)
acf(Simulated.Arima)
Diff.Simulated.Arima=diff(Simulated.Arima)
plot(Diff.Simulated.Arima)
acf(Diff.Simulated.Arima)
pacf(Diff.Simulated.Arima)
library(astsa)
sarima(Simulated.Arima,2,1,1,0,0,0)
library(forecast)
auto.arima(Simulated.Arima)
fit1<-arima(Diff.Simulated.Arima, order=c(4,0,0))
fit1
fit2<-arima(Diff.Simulated.Arima, order=c(2,0,1))
fit2
fit3<-arima(Simulated.Arima, order=c(2,1,1))
fit3
Box.test(Diff.Simulated.Arima)









#Daily female birth

library(astsa)

# read data to R variable
birth.data<-read.csv("daily-total-female-births-in-cal.csv")

# pull out number of births column
number_of_births<-birth.data$Daily.total.female.births.in.California..1959

# use date format for dates
birth.data$Date <- as.Date(birth.data$Date, "%m/%d/%Y")

plot.ts(number_of_births,main='Daily total female births in california, 1959', ylab = 'Number of births')

# Test for correlation
Box.test(number_of_births, lag = log(length(number_of_births)))

# Plot the differenced data
plot.ts(diff(number_of_births), main='Differenced series', ylab = '')

# Test for correlation in the differenced data
Box.test(diff(number_of_births), lag = log(length(diff(number_of_births))))

# acf and pacf of the differenced data

acf(diff(number_of_births), main='ACF of differenced data', 50)
pacf(diff(number_of_births), main='PACF of differenced data', 50)

# Fit various ARIMA models


model1<-arima(number_of_births, order=c(0,1,1))
SSE1<-sum(model1$residuals^2)
model1.test<-Box.test(model1$residuals, lag = log(length(model1$residuals)))

model2<-arima(number_of_births, order=c(0,1,2))

SSE2<-sum(model2$residuals^2)

model2.test<-Box.test(model2$residuals, lag = log(length(model2$residuals)))


model3<-arima(number_of_births, order=c(7,1,1))
SSE3<-sum(model3$residuals^2)
model3.test<-Box.test(model3$residuals, lag = log(length(model3$residuals)))

model4<-arima(number_of_births, order=c(7,1,2))
SSE4<-sum(model4$residuals^2)
model4.test<-Box.test(model4$residuals, lag = log(length(model4$residuals)))

df<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, SSE1, model1.test$p.value), 
               c(model2$aic, SSE2, model2.test$p.value), c(model3$aic, SSE3, model3.test$p.value),
               c(model4$aic, SSE4, model4.test$p.value))
colnames(df)<-c('Arima(0,1,1)','Arima(0,1,2)', 'Arima(7,1,1)', 'Arima(7,1,2)')



format(df, scientific=FALSE)

# Fit a SARIMA model

sarima(number_of_births, 0,1,2,0,0,0)
