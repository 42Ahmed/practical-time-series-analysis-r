library(astsa)
help(astsa)
help(jj)
plot(jj,type="o")
help("flu")
plot(flu)
plot(globtemp)
plot(globtempl)
plot(star)
purely_random_process <- ts(rnorm(100))
purely_random_process
acf(purely_random_process)
(acf(purely_random_process,type = "covariance"))
(acf(purely_random_process,main="Correlogram of purely random process"))
# Random walk
x <- NULL
x[1] <- 0
for (i in 2:1000){
  x[i] <- x[i-1]+rnorm(1) 
  }
plot(x)
random_walk <- ts(x)
plot(random_walk,main="A random walk",ylab=" ",xlab="Days",col="blue",lwd=2)
acf(random_walk)
plot(diff(random_walk))
acf(diff(random_walk))
# Generate noise
noise = rnorm(10000)
movingAvg <- NULL
for(i in 3:10000){
  movingAvg[i] <- noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}
movingAvgProcess = movingAvg[3:10000]
movingAvgProcess <- ts(movingAvgProcess)
#partition output graphics
par(mfrow=c(2,1))
plot(movingAvgProcess,main="A moving average process of order 2",ylab=" ",col="blue")
acf(movingAvgProcess,main="Corellogram of a moving average process of order 2")

# Quiz
# Simulating a non-stationary time series

# Set seed so thet we generate the same dataset
set.seed(2017)
# time variable 
t=seq(0,1,1/100)
# generate a time series
some.time.series=2+3*t+ rnorm(length(t))

# obtain acv for this time series below
(acf(some.time.series,type = "covariance"))
# Simulating a non-stationary time series

# Set seed so thet we generate the same dataset
set.seed(2017)
# time variable 
t=seq(0,1,1/100)
# generate a time series
some.time.series=2+3*t+ rnorm(length(t))
# obtain acf of the time series below
(acf(some.time.series,type ="correlation"))
# Simulating MA(4) process.
# X_t= Z_t+0.2 Z_(t-1)+0.3 Z_(t-2)+ 0.4 Z_(t-3)

set.seed(2^10)
z=NULL
z=rnorm(1000)
data=NULL
for(i in 4:1000){
  data[i-3]=z[i]+0.2*z[i-1]+0.3*z[i-2]+0.4*z[i-3]
}
data=ts(data)

# find acf below
(acf(data,type = "correlation"))
