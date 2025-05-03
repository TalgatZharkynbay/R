options(scipen = 10, digits = 3)
library(xts)
library(lubridate)
library(forecast)
library(quantmod)
library(rugarch)
library(readr)
library(highcharter)
library(moments)
########################################################
Data <- read_csv("USD_KZT Historical Data.csv")
Data$Date=mdy(Data$Date)
Data=xts(Data[,2], order.by = Data$Date)
###########################################
#Data as a whole:
highchart(type = "stock") %>%
  hc_title(text = "Whole Data") %>%
  hc_add_series(Data$Price,
                name = "Exchange Rate")

Data1=diff(log(Data$Price))
Data1=na.omit(Data1)
Data2=Data1["/2015-08-12"]
Data3=Data1["2015-08-12/"]


acf(Data1)
pacf(Data1)
Data1_Mean_Model=ar(as.vector(Data1$Price),method="mle")
aic_1=Data1_Mean_Model$aic
plot(c(0:12),aic_1,type="h",xlab="Order",ylab="AIC")
lines(0:12,aic_1,lty=2)

ADF = ur.df(Data1$Price, type = "trend", lags = 4)
summary(ADF)

Model1 <- ugarchspec(variance.model=list(model="sGARCH",
                                        garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(4,0),
                                    include.mean=F),
                    distribution.model="norm") #or "std"

Fit1 <- ugarchfit(spec=Model1,
                     data=Data1)

Results1=cbind(Fit1@fit$coef,Fit1@fit$se.coef)
colnames(Results1)=c("Coef", "SE")
Results1


kurtosis(Fit1@fit$residuals)

################
Model1_2 <- ugarchspec(variance.model=list(model="eGARCH",
                                         garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(4,0),
                                     include.mean=F),
                     distribution.model="norm") #or "std"

Fit1_2 <- ugarchfit(spec=Model1_2,
                  data=Data1)

Results1_2=cbind(Fit1_2@fit$coef,Fit1_2@fit$se.coef)
colnames(Results1_2)=c("Coef", "SE")
Results1_2


kurtosis(Fit1_2@fit$residuals)


############# Data 2####################################
acf(Data2)
pacf(Data2)
Data2_Mean_Model=ar(as.vector(Data2$Price),method="mle")
aic_1=Data2_Mean_Model$aic
plot(c(0:12),aic_1,type="h",xlab="Order",ylab="AIC")
lines(0:12,aic_1,lty=2)

ADF = ur.df(Data2$Price, type = "trend", lags = 1)
summary(ADF)

Model2 <- ugarchspec(variance.model=list(model="sGARCH",
                                         garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(1,0),
                                     include.mean=F),
                     distribution.model="norm") #or "std"

Fit2 <- ugarchfit(spec=Model2,
                  data=Data2)

Results2=cbind(Fit2@fit$coef,Fit2@fit$se.coef)
colnames(Results2)=c("Coef", "SE")
Results2


kurtosis(Fit2@fit$residuals)

################
Model2_2 <- ugarchspec(variance.model=list(model="eGARCH",
                                           garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(1,0),
                                       include.mean=F),
                       distribution.model="std") #or "std"

Fit2_2 <- ugarchfit(spec=Model2_2,
                    data=Data2)

Results2_2=cbind(Fit2_2@fit$coef,Fit2_2@fit$se.coef)
colnames(Results2_2)=c("Coef", "SE")
Results2_2


kurtosis(Fit2_2@fit$residuals)

############## Data 3, only SGARCH ##########################
acf(Data3)
pacf(Data3)
Data3_Mean_Model=ar(as.vector(Data3$Price),method="mle")
aic_1=Data3_Mean_Model$aic
plot(c(0:12),aic_1,type="h",xlab="Order",ylab="AIC")
lines(0:12,aic_1,lty=2)

ADF = ur.df(Data3$Price, type = "trend", lags = 4)
summary(ADF)

Model3 <- ugarchspec(variance.model=list(model="sGARCH",
                                         garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(4,0),
                                     include.mean=F),
                     distribution.model="norm") #or "std"

Fit3 <- ugarchfit(spec=Model3,
                  data=Data3)

Results3=cbind(Fit3@fit$coef,Fit3@fit$se.coef)
colnames(Results3)=c("Coef", "SE")
Results3


kurtosis(Fit3@fit$residuals)
