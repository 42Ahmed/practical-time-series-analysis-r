par( mfrow=c(3,1) );
plot( arima.sim(n=150, list(order=c(0,0,0) )  ), main="WN" );
plot( arima.sim(n=150, list(ma=c(0.33, 0.33, 0.33)      )  ) , main="MA3");
plot( arima.sim(n=150, list(ma=c(0.2, 0.2, 0.2, 0.2, 0.2) )  ), main="MA5" );
set.seed=1
(acf(arima.sim(n=1000, model=list(ma=c(-0.5, 0.5)))))
#AR Simulation
phi1 = .1;
X.ts <- arima.sim(list(ar = c(phi1)), n=1000)
par(mfrow=c(2,1))
plot(X.ts,main=paste("AR(1) Time Series, phi1=",phi1))
X.acf = acf(X.ts, main="Autocorrelation of AR(1) Time Series")
par(mfrow=c(2,1))

phi1 = .4; phi2 = .3;
X.ts <- arima.sim(list(ar = c(phi1, phi2)), n=1000)
plot(X.ts,main=paste("AR(2) Time Series, phi1=",phi1,"phi2=",phi2))
acf(X.ts,main="ACF")
