#AR(2) simulation
rm(list=ls(all=TRUE))
par(mfrow=c(3,1))
phi1=0.6
phi2=0.2
data=arima.sim(n=500,list(ar=c(phi1,phi2)))
plot(data,main=
       paste("Autoregressive Process with phi1=",phi1,"phi2=",phi2))
acf(data,main="Autocorrelation function")
acf(data,type = "partial",main="Partial Autocorrelation Function")

#AR(3) simulation
rm(list=ls(all=TRUE))
phi1 = 0.9
phi2 = -0.6
phi3= 0.3
data=arima.sim(n=500,list(ar=c(phi1,phi2,phi3)))
plot(data,main=
       paste("Autoregressive Process with phi1=",phi1,"phi2=",phi2,"phi3=",phi3))
acf(data,main="Autocorrelation function")
acf(data,type = "partial",main="Partial Autocorrelation Function")
# Beveridge_wheat price data set
par(mfrow=c(1,1))
data = read.table("beveridge_wheat.txt",header = TRUE)
beveridge = ts(data[,2],start=1500)
plot(beveridge,ylab="price",main="Beveridge Wheat Price Data")
beveridgeMA=filter(beveridge,rep(1/31,31),side=2)
lines(beveridgeMA,col="red")
par(mfrow=c(3,1))
y = beveridge/beveridgeMA
plot(y,ylab="scaled price",
     main="Transformed Beveridge Wheat Price Data")
acf(na.omit(y),
    main="Autocorrelation function of transformed beveridge")
acf(na.omit(y),type="partial",
    main="partial Autocorrelation function of transformed beveridge")
ar(na.omit(y),order.max = 5)
library(isdals)
data("bodyfat")
attach(bodyfat)
pairs(cbind(Fat,Triceps,Thigh,Midarm))
cor(cbind(Fat,Triceps,Thigh,Midarm))
# partial correlation manually
fatHat=predict(lm(Fat~Thigh))
tricepsHat = predict(lm(Triceps~Thigh))
cor((Fat-fatHat),(Triceps-tricepsHat))
# partial function
library(ppcor)
pcor(cbind(Fat,Triceps,Thigh))$estimate

pcor(cbind(Fat,Triceps,Thigh,Midarm))$estimate

fatHat=predict(lm(Fat~Thigh+Midarm))
tricepsHat = predict(lm(Triceps~Thigh+Midarm))
cor((Fat-fatHat),(Triceps-tricepsHat))

par( mfrow=c(3,1) )
plot(LakeHuron)
acf(LakeHuron)
acf(LakeHuron, type="partial")

attach(attitude);
rcl = cbind(rating, complaints, learning);
cor(rcl)

attach(attitude);
rating.hat = predict( lm( rating ~ learning) )
complaints.hat = predict( lm( complaints~learning) )
cor((rating-rating.hat),(complaints-complaints.hat))
(pacf(complaints.hat))
