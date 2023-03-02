par( mfrow=c(3,1) );
plot( arima.sim(n=150, list(order=c(0,0,0) )  ), main="WN" );
plot( arima.sim(n=150, list(ma=c(0.33, 0.33, 0.33)      )  ) , main="MA3");
plot( arima.sim(n=150, list(ma=c(0.2, 0.2, 0.2, 0.2, 0.2) )  ), main="MA5" );
set.seed=1
(acf(arima.sim(n=1000, model=list(ma=c(-0.2279, 0.2488)))))

