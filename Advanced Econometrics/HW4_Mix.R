#Libs:
library(stats)
library(xtable)
library(readxl)
library(stargazer)
library(xts)
library(lubridate)
library(quantmod)
library(car)
library(readr)
library(dplyr)
library(utils)
library(AER)
library(boot)
####################Some event studies######################

EVENT <- read_excel("event.xls")
EVENT = EVENT[,1:6] # getting rid of unnecessary cols
EVENT$date=ymd(EVENT$date)
EVENT=xts(EVENT[, 3:6], order.by = EVENT$date)
EVENT$rdx_rt=diff(log(EVENT$rdx))
EVENT$vei_rt=diff(log(EVENT$vei))
EVENT$atx_rt=diff(log(EVENT$atx))
EVENT = na.omit(EVENT)
head(EVENT[, 5:7])

#Windows
Estimation1=EVENT["/1991-01-24"]
Estimation2=EVENT["/1991-03-06"]
Window1=EVENT["1991-01-25/1991-03-07"]
Window2=EVENT["1991-03-07/1991-03-15"]
# Plot
par(mfrow=c(1,2))
plot(rdx_rt ~ atx_rt, data=EVENT, xlab="Market Ret - Risk Free Rate"
     , ylab="RDX", col = 3)
plot(vei_rt ~ atx_rt, data=EVENT, xlab="Market Ret - Risk Free Rate"
     , ylab="VEI", col =1)


reg_rdx = lm(rdx_rt ~ atx_rt, data = Estimation1)
reg_vei = lm(vei_rt ~ atx_rt, data = Estimation1)
reg_rdx_2 = lm(rdx_rt ~ atx_rt, data = Estimation2)
reg_vei_2 = lm(vei_rt ~ atx_rt, data = Estimation2)

#stargazer(reg_rdx, reg_vei, title="Results", align=TRUE, digits=2, type  = "html")

#Prediction boys:
#RDX
Window1$pred_rdx<-summary(reg_rdx)$coefficients[1]+
  + summary(reg_rdx)$coefficients[2]*Window1$atx_rt

Window1$rdx_abn=Window1$rdx_rt-Window1$pred_rdx
head(Window1[, 5:9])

c_rdx = sum(Window1$rdx_abn)
T1 = nrow(Window1)
X0 = matrix(c(rep(1, T1), Window1$atx_rt), ncol = 2)
XXinv = vcov(reg_rdx)
V_rdx = sigma(reg_rdx)*T1+sum(X0%*%XXinv%*%t(X0))
t_rdx = c_rdx/sqrt(V_rdx)

#VEI
Window1$pred_vei<-summary(reg_vei)$coefficients[1]+
  + summary(reg_vei)$coefficients[2]*Window1$atx_rt

Window1$vei_abn=Window1$vei_rt-Window1$pred_vei
Window1[1:3,c(6,7,10,11)]

c_vei = sum(Window1$vei_abn)
X0 = matrix(c(rep(1, T1), Window1$atx_rt), ncol = 2)
XXinv = vcov(reg_vei)
V_vei = sigma(reg_vei)*T1+sum(X0%*%XXinv%*%t(X0))
t_vei = c_vei/sqrt(V_vei)
t_vei

#Second window !

Window2$pred_rdx_2<-summary(reg_rdx_2)$coefficients[1]+
  + summary(reg_rdx_2)$coefficients[2]*Window2$atx_rt

Window2$rdx_2_abn=Window2$rdx_rt-Window2$pred_rdx_2
head(Window2[, 5:9])

c_rdx_2 = sum(Window2$rdx_2_abn)
T1 = nrow(Window2)
X0 = matrix(c(rep(1, T1), Window2$atx_rt), ncol = 2)
XXinv = vcov(reg_rdx_2)
V_rdx_2 = sigma(reg_rdx_2)*T1+sum(X0%*%XXinv%*%t(X0))
t_rdx_2 = c_rdx_2/sqrt(V_rdx_2)

#vei_2
Window2$pred_vei_2<-summary(reg_vei_2)$coefficients[1]+
  + summary(reg_vei_2)$coefficients[2]*Window2$atx_rt

Window2$vei_2_abn=Window2$vei_rt-Window2$pred_vei_2
Window2[1:3,c(6,7,10,11)]

c_vei_2 = sum(Window2$vei_2_abn)
X0 = matrix(c(rep(1, T1), Window2$atx_rt), ncol = 2)
XXinv = vcov(reg_vei_2)
V_vei_2 = sigma(reg_vei_2)*T1+sum(X0%*%XXinv%*%t(X0))
t_vei_2 = c_vei_2/sqrt(V_vei_2)

Results=data.frame(t_rdx, t_rdx_2, t_vei, t_vei_2)
Results

#Going for the plots of excess returns:
par(mfrow=c(1,2))

plot(rdx_abn ~t,data = Window1, col = "red", type = "b", pch = 19,
     xlab = "time", ylab = "Cum XS Ret", lty = 1, main="First Window")
lines(vei_abn~t,data = Window1, col = "blue", lty = 2 , pch = 18)
abline(v=0, col="black")
legend("topleft", legend=c("rdx", "vei"),col=c("red", "blue"), lty = 1:2, cex=0.8)

plot(rdx_2_abn ~t,data = Window2, col = "red", type = "b", pch = 19,
     xlab = "time", ylab = "Cum XS Ret", lty = 1, main="Second Window")
lines(vei_2_abn~t,data = Window2, col = "blue", lty = 2 , pch = 18)
abline(v=0, col="black")
legend("topright", legend=c("rdx", "vei"),col=c("red", "blue"), lty = 1:2, cex=0.8)


##################### UIRP ###################################
#First get the exchange rates:

getSymbols("JPYNZD=X", src = "yahoo")
JPYNZD=`JPYNZD=X`[, ncol(`JPYNZD=X`)]
colnames(JPYNZD)=c("S_t")
JPYNZD$S_t_minus_1=lag(JPYNZD$S_t)
JPYNZD$Y=log(JPYNZD$S_t)-log(JPYNZD$S_t_minus_1)
JPYNZD=na.omit(JPYNZD)

#Now let's get the interest rates:
NZD <- read_csv("NZD.csv")
NZD$Date=mdy(NZD$Date)
NZD=xts(NZD$Price, order.by = NZD$Date)
colnames(NZD)=c("R_NZD")
NZD$R_NZD=(1+NZD$R_NZD/30/100)

JPN <-read_csv("JPN.csv")
JPN$Date=mdy(JPN$Date)
JPN=xts(JPN$Price, order.by = JPN$Date)
colnames(JPN)=c("R_JPN")
JPN$R_JPN=(1+JPN$R_JPN/30/100)

Data=na.omit(merge(JPYNZD, JPN, NZD))
Data$F_t=Data$S_t_minus_1*Data$R_JPN/Data$R_NZD
Data$F_t_minus_1=lag(Data$F_t)
Data=na.omit(Data)
Data$X=log(Data$F_t_minus_1)-log(Data$S_t_minus_1)

head(Data)

Results=lm(Y~X, data = Data)
summary(Results)
stargazer(Results ,title="Results", align=TRUE, digits=2, type = "html")

t_stat_b_1=abs(Results$coefficients[2]-1)/0.024988
t_stat_b_1

linearHypothesis(Results, c("(Intercept)=0", "X=1"),test = "F")
######################################################################
#Moving on with the USD/UK:

fw <- read_excel("uirp.xls", sheet = "USBP")
fw=fw%>%mutate(s.USBP=log(EXUSBP),
               f.USBP=log(F1USBP),
               ds.USBP=s.USBP-dplyr::lag(s.USBP,n=1),
               fp.USBP=dplyr::lag(f.USBP,n=1)-dplyr::lag(s.USBP,n=1))
fw=na.omit(fw)

reg.useur=lm(ds.USBP~fp.USBP,data=fw)
summary(reg.useur)

# Residual standard error "by hand"
e=reg.useur$residual
df=reg.useur$df.residual;df
se.reg.useur=sqrt(sum(e^2)/df);se.reg.useur


# standard errors of coefficients, t-stats and p-values by hand
coef.reg.useur=reg.useur$coefficients
coef.reg.useur
se.coef.reg.useur=sqrt(diag(vcov(reg.useur)))
se.coef.reg.useur
t.coef.reg.useur=coef.reg.useur/se.coef.reg.useur
t.coef.reg.useur
pvalue.coef.reg.useur=2*(1-pt(abs(t.coef.reg.useur),df=df))
pvalue.coef.reg.useur

# test H0: beta1=1
t.beta1=(coef.reg.useur[2]-1)/se.coef.reg.useur[2]
pvalue.beta1=2*(1-pt(abs(t.beta1),df=df))
t.beta1
pvalue.beta1

# joint test by hand
R=as.matrix(diag(2))
delta=as.vector(c(0,1))
d=R%*%coef.reg.useur
X=model.matrix(reg.useur)

W=t(d-delta)%*%solve((se.reg.useur^2)*(R%*%solve(t(X)%*%X)%*%t(R)))%*%(d-delta)
pvalue.W=1-pchisq(W,2)
pvalue.W

# F-test by hand
Rsq.u=summary(reg.useur)$r.squared
e.restricted=fw[,"ds.USBP"]-fw[,"fp.USBP"]
sse.r=sum(e.restricted^2)
ssy=sum((fw[,"ds.USBP"]-mean(fw$ds.USBP))^2)

Rsq.r=1-sse.r/ssy
F=df*(Rsq.u-Rsq.r)/(2*(1-Rsq.u))
F

linearHypothesis(reg.useur, c("(Intercept)=0", "fp.USBP=1"),test = "F")


###########CCAPM#####################
#Ex1:
CCAPM1=read_excel("ccapm.xls")
CCAPM1$I2=lag(CCAPM1$`I(-1)`,1)
CCAPM1$DP2=lag(CCAPM1$`DP(-1)`,1)
CCAPM1=na.omit(CCAPM1)

#First Stage:

olsreg1=lm(CCAPM1$CG~CCAPM1$`I(-1)`+CCAPM1$`DP(-1)`+CCAPM1$I2+CCAPM1$DP2)
summary(olsreg1)

#Second Stage:
ivreg1=ivreg(CCAPM1$Y~CCAPM1$CG|CCAPM1$`I(-1)`+CCAPM1$`DP(-1)`+CCAPM1$I2+CCAPM1$DP2)
summary(ivreg1)
summary(ivreg1, vcov=sandwich, diagnostics = TRUE)


#Ex2:
ie_data=read.csv("ie_data1.csv", stringsAsFactors=FALSE) #Dodelat potom!!!!!!
ie_data$Real.I=as.numeric(ie_data$Real.I)
ie_data$I1=lag(ie_data$Real.I,1)
ie_data$DP1=lag(ie_data$ln.dp.,1)
ie_data$I2=lag(ie_data$Real.I,2)
ie_data$DP2=lag(ie_data$ln.dp.,2)
ie_data=na.omit(ie_data)

#First Stage:

olsreg2=lm(ie_data$CG~ie_data$I1+ie_data$DP1+ie_data$I2+ie_data$DP2)
summary(olsreg2)

#Second Stage:
ivreg2=ivreg(ie_data$ln.1.Rt.~ie_data$CG|ie_data$I1+ie_data$DP1+ie_data$I2+ie_data$DP2)
summary(ivreg2)
summary(ivreg2, vcov=sandwich, diagnostics = TRUE)


#############################BootStrap#############
#1) 
#First KZT/USD:
KZTUSD <- read_csv("USD_KZT Historical Data.csv")
KZTUSD$Date=mdy(KZTUSD$Date)
KZTUSD=xts(KZTUSD[, 2], order.by = KZTUSD$Date)
KZTUSD=to.monthly(KZTUSD, indexAt = "lastof", OHLC=FALSE)

Log_Ret=na.omit(diff(log(KZTUSD$Price)))
Simple_ret=exp(Log_Ret)-1

Simple_Means=data.frame(mean(Simple_ret), mean(Log_Ret))
Simple_Means

Trim_Means=data.frame(mean(Simple_ret, trim=0.05), mean(Log_Ret, trim=0.05))
Trim_Means

All_Means=merge(Simple_Means, Trim_Means)
colnames(All_Means)=c("Simple", "Log", "Trimmed Simple", "Trimmed Log")
All_Means

# Boot KZT/USD
T = length(Log_Ret)
Ret_mean_boot =0 #Intialise
for(i in 1:1000){
rsample = sample(1:T,T,replace = TRUE)
Ret_mean_boot[i] = mean(Log_Ret[rsample], trim =0.05, na.rm = TRUE)
}
summary(Ret_mean_boot)
qqnorm(Ret_mean_boot, ylab="Means of resamples")


se.boot = sd(Ret_mean_boot,na.rm=TRUE)
t.star = qt(0.975, df=49)
t.star
moe = t.star * se.boot
mean(Log_Ret, trim=0.05, na.rm=TRUE) + c(-moe, moe)

#Now, the GBPUSD:
getSymbols("GBPUSD=X", src = "yahoo")
GBPUSD=`GBPUSD=X`[, ncol(`GBPUSD=X`)]
colnames(GBPUSD)=c("Ret")
GBPUSD=to.monthly(GBPUSD, indexAt = "lastof", OHLC=FALSE)

Log_Ret=na.omit(diff(log(GBPUSD$Ret)))
Simple_ret=exp(Log_Ret)-1

Simple_Means=data.frame(mean(Simple_ret), mean(Log_Ret))
Simple_Means

Trim_Means=data.frame(mean(Simple_ret, trim=0.05), mean(Log_Ret, trim=0.05))
Trim_Means

All_Means=merge(Simple_Means, Trim_Means)
colnames(All_Means)=c("Simple", "Log", "Trimmed Simple", "Trimmed Log")
All_Means

# Boot GBP/USD
T = length(Log_Ret)
Ret_mean_boot =0 #Intialise

for(i in 1:1000){
  rsample = sample(1:T,T,replace = TRUE)
  Ret_mean_boot[i] = mean(Log_Ret[rsample], trim =0.05, na.rm = TRUE)
}
summary(Ret_mean_boot)
qqnorm(Ret_mean_boot, ylab="Means of resamples ")


se.boot = sd(Ret_mean_boot,na.rm=TRUE)
t.star = qt(0.975, df=49)
t.star
moe = t.star * se.boot
mean(Log_Ret, trim=0.05, na.rm=TRUE) + c(-moe, moe)

#2)
Data <- read_excel("AE_6_FF.xlsx")
Fit_Agric=lm(Agric~Mkt.RF+SMB+HML, data = Data)
summary(Fit_Agric)

#Starting with residuals

r = residuals(Fit_Agric)
dim = dim(Data)
T = dim[1]
fit.boot = data.frame(matrix(0,1000,T))

for(i in 1:1000){
  rsample = sample(1:T,T,replace = TRUE)
  
  Data$Agric_fit = Data$Agric+r[rsample]
  
  results = lm(Agric_fit ~ Mkt.RF + HML + SMB, data = Data)
  
  fit.boot[i,] = results$fitted.values #Extracts the fits
}

upper = matrix(0,T,1)
lower = matrix(0,T,1)
Residual = matrix(0,T,1)

for (i in 1:T){
  upper[i] = mosaic::qdata(fit.boot[,i], 0.975)
  lower[i] = mosaic::qdata(fit.boot[,i], 0.025)
  Residual[i]= upper[i]-lower[i]
}

head(fit.boot[, c(1:3, ncol(fit.boot))])
head(Residual)
#Going for cases:

fit.boot.cases = data.frame(matrix(0,1000,T))

for(i in 1:1000){
  rsample = sample(1:T,T,replace = TRUE)
  
  results = lm(Agric ~ Mkt.RF + HML + SMB, data = Data[rsample,])
  
  fit.boot.cases[i,] = results$fitted.values #Extracts the fits
}

upper = matrix(0,T,1)
lower = matrix(0,T,1)
Cases = matrix(0,T,1)

for (i in 1:T){
  upper[i] = mosaic::qdata(fit.boot.cases[,i], 0.975)
  lower[i] = mosaic::qdata(fit.boot.cases[,i], 0.025)
  Cases[i]= upper[i]-lower[i]
}

plot(Residual, ylab="Width" ,xlab = "Observations", col = "red", type = 'l', lty = 1, 
 lwd = 1.5, ylim=range( c(Residual, Cases) ))
lines(Cases, col = "blue", lty = 1, lwd = 1.5)
legend("center", bg = 'transparent', legend=c("Cases", "Residual"), fill=c("blue", "red"))

#3)
Data2 <- read_excel("AE_25_FF.xlsx")
#inputs:

dim = dim(Data2)
T = dim[1]
alph.boot = data.frame(matrix(0,1000,25))
initial_alphas = vector()
CAPM_alphas_exp.boot = data.frame(matrix(0,1000,25))

#mega-looping stuff:

for(j in 5:29){    
  initial_regression = lm(unlist(Data2[, j]) ~ MktxRF + HML + SMB, data = Data2)
  initial_alphas[j-4] = initial_regression$coefficients[1]

  
  for(i in 1:1000){
    rsample = sample(1:T,T,replace = TRUE)
    results = lm(unlist(Data2[, j]) ~ MktxRF + HML + SMB, data = Data2[rsample,])
    
    Sigma = cov(as.matrix(results$residuals))
    SigmaInv<-solve(Sigma)
    alpha = summary(results)$coefficients[1]
    CAPM_alphas_exp=(t(alpha)%*%SigmaInv%*%alpha)
    
    CAPM_alphas_exp.boot[i,j-4] = CAPM_alphas_exp
    alph.boot[i,j-4] = results$coefficients[1]
  }
}

head(alph.boot[, c(1:3, ncol(alph.boot))])


#CI's for alphas:
upper = matrix(0,25,1)
lower = matrix(0,25,1)
CI = matrix(0,25,1)

for (i in 1:25){
  upper[i] = mosaic::qdata(alph.boot[,i], 0.975)
  lower[i] = mosaic::qdata(alph.boot[,i], 0.025)
  CI= data.frame(cbind(lower, upper))
}

colnames(CI)=c("2.5%", "97.5%")  
CI  

#RMSE and Absolute:
Absolute_Mean_Alphas=sapply(alph.boot, function(x) mean(abs(x)))
RMSE_Alphas=sapply(alph.boot, function(x) sqrt((t(x)%*%x)/1000))
Alpha_Means=data.frame(Absolute_Mean_Alphas, RMSE_Alphas)
Alpha_Means

#4)
f = density(CAPM_alphas_exp.boot[,1],kernel = "epanechnikov", na.rm = TRUE)
plot(f, main = "Kernel Density Alphas vs Normal Density")
X = seq(from=-4, to=4, by=0.0001)
SND_pdf = dnorm(x=X, mean=mean(CAPM_alphas_exp.boot[,1], na.rm = TRUE), sd=sd(CAPM_alphas_exp.boot[,1], na.rm = TRUE))
lines(X, SND_pdf, col="blue")
