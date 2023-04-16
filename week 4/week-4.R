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

#simulate ar(2) lab
set.seed(2017)
#model parameters that we will estimate
sigma=4
phi=NULL
phi[1:2]=c(1/3,1/2)
phi
n=10000
#simulate process
ar.process=arima.sim(n,model=list(ar=c(1/3,1/2)), sd=4)
ar.process[1:5]
#find 2nd & 3rd sample autocorrelation
r=NULL
r[1:2]=acf(ar.process, plot=F)$acf[2:3]
r
#matrix R
R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
R
# edit R
R[1,2]=r[1] # only diagonal entries are edited
R[2,1]=r[1] # only diagonal entries are edited
R
#b column vector on right
b=matrix(r,nrow=2,ncol=1)# b- column vector with no entries
b
#solve Rx=b
phi.hat=matrix(c(solve(R,b)[1,1], solve(R,b)[2,1]),2,1)
phi.hat
# variance estimation
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat
par(mfrow=c(3,1))
# plot time series,acf,pacf
plot(ar.process, main='Simulated AR(2)')
acf(ar.process, main='ACF')
pacf(ar.process, main='PACF')

#AR(3) simulation
sigma=4
phi=NULL
phi[1:3]=c(1/3,1/2,7/100)
n=100000
ar3.process=arima.sim(n,model=list(ar=c(1/3,1/2, 7/100)), sd=4)
r=NULL
r[1:3]=acf(ar3.process, plot=F)$acf[2:4]
r
R=matrix(1,3,3) 
R[1,2]=r[1] 
R[1,3]=r[2]
R[2,1]=r[1]
R[2,3]=r[1]
R[3,1]=r[2]
R[3,2]=r[1]
R
# b-column vector on the right
b=matrix(0,3,1)# b- column vector with no entries
b[1,1]=r[1]
b[2,1]=r[2]
b[3,1]=r[3]
b
# solve Rx=b and find phi's
phi.hat=solve(R,b)
phi.hat
# sigma estimation
c0=acf(ar3.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat
par(mfrow=c(3,1))
plot(ar3.process, main='Simulated AR(3)')
acf(ar3.process, main='ACF')
pacf(ar3.process, main='PACF')

#Modeling recruitment time
library(astsa)
my.data=rec

# Plot rec 
plot(rec, main='Recruitment time series', col='blue', lwd=3)
# subtract mean to get a time series with mean zero
ar.process=my.data-mean(my.data)

# ACF and PACF of the process
par(mfrow=c(2,1))
acf(ar.process, main='Recruitment', col='red', lwd=3)
pacf(ar.process, main='Recruitment', col='green', lwd=3)
# order
p=2

# sample autocorreleation function r
r=NULL
r[1:p]=acf(ar.process, plot=F)$acf[2:(p+1)]
cat('r=',r,'\n')
# matrix R
R=matrix(1,p,p) # matrix of dimension 2 by 2, with entries all 1's.

# define non-diagonal entires of R
for(i in 1:p){
    for(j in 1:p){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
    }
}
R
# b-column vector on the right
b=NULL
b=matrix(r,p,1)# b- column vector with no entries
b
# solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat
#variance estimation using Yule-Walker Estimator
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
# constant term in the model
phi0.hat=mean(my.data)*(1-sum(phi.hat))
phi0.hat
cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')






# Johnson Johnson model fitting

# Time plot for Johnson&Johnson
plot(JohnsonJohnson, main='Johnson&Johnosn earnings per share', col='blue', lwd=3)
# log-return of Johnson&Johnson
jj.log.return=diff(log(JohnsonJohnson))
jj.log.return.mean.zero=jj.log.return-mean(jj.log.return)
# Plots for log-returns
par(mfrow=c(3,1))
plot(jj.log.return.mean.zero, main='Log-return (mean zero) of Johnson&Johnosn earnings per share')
acf(jj.log.return.mean.zero, main='ACF')
pacf(jj.log.return.mean.zero, main='# Order
p=4PACF')
# sample autocorreleation function r
r=NULL
r[1:p]=acf(jj.log.return.mean.zero, plot=F)$acf[2:(p+1)]
r
# matrix R
R=matrix(1,p,p) # matrix of dimension 4 by 4, with entries all 1's.

# define non-diagonal entires of R
for(i in 1:p){
    for(j in 1:p){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
    }
}
R
# b-column vector on the right
b=matrix(r,p,1)# b- column vector with no entries
b
phi.hat=solve(R,b)[,1]
phi.hat
# Variance estimation using Yule-Walker Estimator
c0=acf(jj.log.return.mean.zero, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
# Constant term in the model
phi0.hat=mean(jj.log.return)*(1-sum(phi.hat))
phi0.hat
cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')
