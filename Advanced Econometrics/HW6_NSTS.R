options(digits = 3, scipen = 3)
library(ggplot2)
library(tseries)
#######################################################

#Exercise 1:
# Simulation of OLS Estimate
Nsim =1000
mu=0
T=100
t=seq(1,T-1,1)
rhohat = matrix(0,Nsim,1)
residuals= matrix(0, 99, Nsim)

for (i in 1:1000){
  Y = mu + arima.sim(model = list(order = c(0, 1, 0) ),
                     n = T, sd = 0.1)
  Dy = Y[2:T] - Y[1:(T-1)]
  Ly = Y[1:(T-1)]
  fitOLS = lm(Dy~Ly+t) 
  rhohat[i,1] = fitOLS$coef[2]
  residuals[,i]= fitOLS$residuals
}

quint = quantile(rhohat,c(0.975,0.025))
quint
quint[3]=0

par(mfrow=c(1,1), mar=c(4,5,2,1), family="serif")
plot(density(rhohat[,1]), main="Y_Hat:Density")
abline(v=quint, col =c("blue","blue","red"))

residuals=as.data.frame(residuals)
ehat=colMeans(residuals)
ehat=as.data.frame(ehat)
ggplot(ehat, aes(x = ehat)) +  
  geom_density(fill = "mediumseagreen", alpha = 0.1) +
  stat_function(fun = function(x) dnorm(x, mean = mean(ehat$ehat), 
                                        sd = sd(ehat$ehat)),
                color = "red", linetype = "dotted", size = 1)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

########################################################
adf.test(rhohat,alternative = c("stationary"))

##############################
#Exercise 5
pe = read_excel("pe.xlsx")
pe=pe[1:124,]
pe=pe$PE
tsdisplay(pe)

stationary.test(pe)  # same as adf.test(x)
stationary.test(pe, method = "pp") # same as pp.test(x)
stationary.test(pe, method = "kpss") # same as kpss.test(x)
res51=ur.ers(pe, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
             lag.max = 4)
summary(res51)

fitpe1=arima(pe, order=c(1,0,0),method="ML")
AIC(fitpe1)
fitpe2=arima(pe, order=c(2,0,0),method="ML")
AIC(fitpe2)
fitpe3=arima(pe, order=c(3,0,0),method="ML")
AIC(fitpe3)
fitpe4=arima(pe, order=c(4,0,0),method="ML")
AIC(fitpe4)
fitpe5=arima(pe, order=c(5,0,0),method="ML")
AIC(fitpe5)

checkresiduals(fitpe1)
summary(fitpe1)

forecast1=forecast(fitpe1)
forecast1
plot(forecast1)


v=diff(pe)
tsdisplay(v)
stationary.test(v)  # same as adf.test(x)
stationary.test(v, method = "pp") # same as pp.test(x)
stationary.test(v, method = "kpss") # same as kpss.test(x)
res52=ur.ers(v, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
             lag.max = 4)
summary(res52)

fitv1=arima(v, order=c(1,0,0),method="ML")
AIC(fitv1)
fitv2=arima(v, order=c(2,0,0),method="ML")
AIC(fitv2)
fitv3=arima(v, order=c(3,0,0),method="ML")
AIC(fitv3)
fitv4=arima(v, order=c(4,0,0),method="ML")
AIC(fitv4)
fitv5=arima(v, order=c(5,0,0),method="ML")
AIC(fitv5)
fitv6=arima(v, order=c(6,0,0),method="ML")
AIC(fitv6)

summary(fitv5)
checkresiduals(fitv5)

forecast2=forecast(fitv5)
forecast2
plot(forecast2)


##################6
KASE=read.csv("KASE.csv", header=TRUE, as.is=TRUE,na.strings = c("NA",".",""))
colnames(KASE)=c("date","closed")
y=log(KASE$closed)

plot(y)
tsdisplay(y)

stationary.test(y)  # same as adf.test(x)
stationary.test(y, method = "pp") # same as pp.test(x)
stationary.test(y, method = "kpss") # same as kpss.test(x)
library(urca)
library(vars)
res1=ur.ers(y, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
            lag.max = 4)
summary(res1)

w=diff(y)
plot(w)
tsdisplay(w)
d=density(w)
plot(d)
points(seq(min(w), max(w), length.out=500),
       dnorm(seq(min(w), max(w), length.out=500),
             mean(w), sd(w)), type="l", col="red")

stationary.test(w, nlag = 1)  # same as adf.test(x)
stationary.test(w, method = "pp") # same as pp.test(x)
stationary.test(w, method = "kpss") # same as kpss.test(x)
res2=ur.ers(w, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
            lag.max = 4)
summary(res2)