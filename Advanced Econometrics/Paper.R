options(digits = 3, scipen = 3)
library(forecast)
library(readxl)
library(rugarch)
library(moments)
library(urca)
library(ggplot2)
library(lmtest)
library(tseries)
library(lubridate)
library(xts)
library(tidyquant)
library(aTSA)
library(writexl)
###############################################
KASE=read_xlsx("KASE.xlsx")
KASE$Date=dmy(KASE$Date)
KASE=xts(KASE$Close, order.by = KASE$Date)
KASE=KASE["2010/"]
plot(KASE)
KASE <-
  Return.calculate(KASE,
                   method = "log") %>%
  na.omit()

getSymbols("DCOILBRENTEU", src = "FRED")
BRENT <-
  Return.calculate(DCOILBRENTEU,
                   method = "log") %>%
  na.omit()

KASE=na.omit(merge(KASE, BRENT))
colnames(KASE)=c("LogRet", "Brent")

plot(KASE$LogRet)
acf(KASE$LogRet)
pacf(KASE$LogRet) 

Model_1_AIC <- data.frame()
for(d in 0:1){
  for(p in 0:9){
    for(q in 0:9){
      
      fit=Arima(KASE$LogRet,order=c(p,d,q))
      Model_1_AIC <- rbind(Model_1_AIC, c(d,p,q,AIC(fit))) #
    }
  }
}
names(Model_1_AIC) <- c("Diff", "AR", "MA",  "AIC")
rowNum <- which(Model_1_AIC$AIC==min(Model_1_AIC$AIC))
Model_1_AIC[rowNum,]

adf_test=ur.df(KASE$LogRet,type="drift", lags = 5) 
summary(adf_test)

kpss_test = ur.kpss(KASE$LogRet, type = "tau", lags = "short")
summary(kpss_test)

pp_test = ur.pp(KASE$LogRet, type = "Z-tau", model = "trend", lags = "short")
summary(pp_test)

Test_of_Adequacy=arima(KASE$LogRet, order = c(5, 0, 5))
Box.test(Test_of_Adequacy$residuals, type = "Ljung")
plot(Test_of_Adequacy$residuals)
kurtosis(Test_of_Adequacy$residuals)

ehat=as.data.frame(Test_of_Adequacy$residuals)

ggplot(ehat, aes(x = x)) +  
  geom_density(fill = "mediumseagreen", alpha = 0.1) +
  stat_function(fun = function(x) dnorm(x, mean = mean(ehat$x), 
                                        sd = sd(ehat$x)),
                color = "red", linetype = "dotted", size = 1)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())


arch.test(Test_of_Adequacy, output = TRUE)
FinTS::ArchTest(Test_of_Adequacy$residuals)

############## DAYS OF WEEK ############################
KASE$Monday=ifelse(wday(KASE) == 2, 1, 0)
KASE$Wednesday=ifelse(wday(KASE) == 4, 1, 0)
KASE$Thursday=ifelse(wday(KASE) == 5, 1, 0)
KASE$Friday=ifelse(wday(KASE) == 6, 1, 0)
#KASE$Lag_5=lag(KASE$LogRet,5)
KASE=na.omit(KASE)


########## Fitting #############################


Model1 <- ugarchspec(variance.model=list(model="eGARCH",
                                         garchOrder=c(1,1)),
                     
                     mean.model=list(armaOrder=c(5,5),
                                     include.mean=F,
                                     external.regressors=as.matrix(cbind(KASE[,2], 
                                                                         KASE[,3],
                                                                         KASE[,4],
                                                                         KASE[,5],
                                                                         KASE[,6]))),
                     distribution.model="std") #or "std"
#,solver ='hybrid'

Fit1 <- ugarchfit(spec=Model1,
                  data=as.matrix(KASE$LogRet))

Fit1

Results=as.data.frame(Fit1@fit$robust.matcoef)
write_xlsx(Results, "C:/Users/talga/Desktop/Results.xlsx")
Results
kurtosis(Fit1@fit$residuals)
plot(Fit1@fit$residuals)

############## HOLIDAYS ###############################
KASE2=read_xlsx("KASE.xlsx")
KASE2$Date=dmy(KASE2$Date)
KASE2=xts(KASE2$Close, order.by = KASE2$Date)
KASE2=KASE2["2010/"]
KASE2 <-
  Return.calculate(KASE2,
                   method = "log") %>%
  na.omit()

KASE2=na.omit(merge(KASE2, BRENT))
colnames(KASE2)=c("LogRet", "Brent")


KASE2$Nauryz <- ifelse(.indexmday(KASE2) == 20 & .indexmon(KASE2)==2, 1,
               ifelse(.indexmday(KASE2) == 21 & .indexmon(KASE2)==2, 1,
                      ifelse(.indexmday(KASE2) == 22 & .indexmon(KASE2)==2, 1,
                             ifelse(.indexmday(KASE2) == 23 & .indexmon(KASE2)==2, 1,
                                    ifelse(.indexmday(KASE2) == 24 & .indexmon(KASE2)==2, 1,
                                           ifelse(.indexmday(KASE2) == 25 & .indexmon(KASE2)==2, 1,
                                    0))))))


KASE2$Vosmoe <- ifelse(.indexmday(KASE2) == 6 & .indexmon(KASE2)==2, 1,
                       ifelse(.indexmday(KASE2) == 7 & .indexmon(KASE2)==2, 1,
                              ifelse(.indexmday(KASE2) == 8 & .indexmon(KASE2)==2, 1,
                                     ifelse(.indexmday(KASE2) == 9 & .indexmon(KASE2)==2, 1,
                                            ifelse(.indexmday(KASE2) == 10 & .indexmon(KASE2)==2, 1,
                                                   ifelse(.indexmday(KASE2) == 11 & .indexmon(KASE2)==2, 1,
                                                          0))))))


KASE2$Pobeda <- ifelse(.indexmday(KASE2) == 7 & .indexmon(KASE2)==4, 1,
                       ifelse(.indexmday(KASE2) == 8 & .indexmon(KASE2)==4, 1,
                              ifelse(.indexmday(KASE2) == 9 & .indexmon(KASE2)==4, 1,
                                     ifelse(.indexmday(KASE2) == 10 & .indexmon(KASE2)==4, 1,
                                            ifelse(.indexmday(KASE2) == 11 & .indexmon(KASE2)==4, 1,
                                                   ifelse(.indexmday(KASE2) == 12 & .indexmon(KASE2)==4, 1,
                                                          0))))))


KASE2$INDP <- ifelse(.indexmday(KASE2) == 14 & .indexmon(KASE2)==11, 1,
                       ifelse(.indexmday(KASE2) == 15 & .indexmon(KASE2)==11, 1,
                              ifelse(.indexmday(KASE2) == 16 & .indexmon(KASE2)==11, 1,
                                     ifelse(.indexmday(KASE2) == 17 & .indexmon(KASE2)==11, 1,
                                            ifelse(.indexmday(KASE2) == 18 & .indexmon(KASE2)==11, 1,
                                                   ifelse(.indexmday(KASE2) == 19 & .indexmon(KASE2)==11, 1,
                                                          0))))))


KASE2$Astana <- ifelse(.indexmday(KASE2) == 4 & .indexmon(KASE2)==6, 1,
                       ifelse(.indexmday(KASE2) == 5 & .indexmon(KASE2)==6, 1,
                              ifelse(.indexmday(KASE2) == 6 & .indexmon(KASE2)==6, 1,
                                     ifelse(.indexmday(KASE2) == 7 & .indexmon(KASE2)==6, 1,
                                            ifelse(.indexmday(KASE2) == 8 & .indexmon(KASE2)==6, 1,
                                                   ifelse(.indexmday(KASE2) == 9 & .indexmon(KASE2)==6, 1,
                                                          0))))))


KASE2=na.omit(KASE2)

Model2 <- ugarchspec(variance.model=list(model="eGARCH",
                                         garchOrder=c(1,1)),
                     
                     mean.model=list(armaOrder=c(3,0),
                                     include.mean=F,
                                     external.regressors=as.matrix(cbind(KASE2[,2], 
                                                                         KASE2[,3],
                                                                         KASE2[,4],
                                                                         KASE2[,5],
                                                                         KASE2[,6],
                                                                         KASE2[,7]))),
                     distribution.model="std") #or "std"

Fit2 <- ugarchfit(spec=Model2,
                  data=as.matrix(KASE2$LogRet))

Fit2

Results2=as.data.frame(Fit2@fit$robust.matcoef)
write_xlsx(Results2, "C:/Users/talga/Desktop/Results.xlsx")
Results2
kurtosis(Fit2@fit$residuals)
