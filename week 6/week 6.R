#Sarima simulation
x=NULL
z=NULL
n=10000

z=rnorm(n)
x[1:13]=1

for(i in 14:n){
  x[i]<-z[i]+0.7*z[i-1]+0.6*z[i-12]+0.42*z[i-13]
}

par(mfrow=c(2,1))
plot.ts(x[12:120], main='The first 10 months of simulation SARIMA(0,0,1,0,0)_12', ylab='') 

acf(x, main='SARIMA(0,0,1,0,0,1)_12 Simulation')



library(astsa)

d=1
DD=1

per=4

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}









milk<-read.csv('monthlyMilkProduction.csv')
Milk<-milk$Monthly.milk.production..pounds.per.cow..Jan.62...Dec.75
library(astsa)
sarima(Milk, 0,1,0,0,1,1,12)
library(forecast)

d=NULL
DD=NULL
d=1
DD=1

per=12
for(p in 1:1){
  for(q in 1:1){
    for(i in 1:3){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=Milk, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
  model<- arima(x=Milk, order = c(0,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))  








library(readxl)
milk<-read_excel('monthly-milk-production-pounds-p.xlsx')
Milk<-milk$Milk
library(astsa)
sarima(Milk, 0,1,0,0,1,1,12)
library(astsa)
library(forecast)

d=NULL
DD=NULL
d=1
DD=1

per=12
for(p in 1:1){
  for(q in 1:1){
    for(i in 1:3){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=Milk, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
model<- arima(x=Milk, order = c(0,1,0), seasonal = list(order=c(0,1,1), period=12))
plot(forecast(model))





rain.data <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rain.ts <- ts(rain.data,start = c(1813))
par(mfrow=c(2,1))
hist(rain.data)
qqnorm(rain.data)
qqline(rain.data)
library(forecast)
auto.arima(rain.ts)
alpha=0.2
forecast.values=NULL
n=length(rain.data)
forecast.values[1]=rain.data[1]
for(i in 1:n){
  forecast.values[i+1]=alpha*rain.data[i]+(1-alpha)*forecast.values[i]
}
paste("forecast for time",n+1,"=",forecast.values[n+1])
HoltWinters(rain.ts,beta = FALSE,gamma = F)
HoltWinters(rain.ts,gamma = F)


library(forecast)
AirPassengers.hw <- HoltWinters(log10(AirPassengers))
AirPassengers.forecast <- forecast(AirPassengers.hw)
plot(AirPassengers.forecast)
