### Exercise 4 from ARMA HW1 ###
#Part2:
ARMA100=arima.sim(model=list(ar=0.5, ma= 0.5),n=100) 
ARMA10000=arima.sim(model=list(ar=0.5, ma= 0.5),n=10000) 
par(mfrow=c(2,1))
ts.plot(ARMA100)
ts.plot(ARMA10000)

OLS_AR_1=lm(ARMA100[-100]~ARMA100[-1])
Summary1=summary(OLS_AR_1)
Summary1$coefficients


OLS_AR_2=lm(ARMA10000[-10000]~ARMA10000[-1])
Summary2=summary(OLS_AR_2)
Summary2$coefficients

Mean_Bias=sum(Summary1$coefficients[2]-0.5, Summary2$coefficients[2]-0.5)/2
Mean_Bias
