#Q1
#i) 
library(dplyr)
library(ggplot2)
library(nlme)
library(car)

Signals=read.csv(file="Signals_A_62016.csv", header=TRUE, as.is = TRUE, na.strings = c("NA",".",""))

#Creating new dummy variables
Signals$ETP_IndNeg[is.na(Signals$ETP)] = 1
Signals$ETP_IndNeg[!is.na(Signals$ETP)] = 0
Signals$ETP_ValPos[is.na(Signals$ETP)] = 0
Signals$ETP_ValPos[!is.na(Signals$ETP)] = Signals$ETP[!is.na(Signals$ETP)]

#Creating Log Variables of Size and BTM
Signals = Signals %>%
  mutate(LogSize = log(Size))
Signals = Signals %>%
  mutate(LogBTM = log(BTM))
SignalsDF_1990 = Signals[Signals$PFormDate == "30/06/1990", ]

#Estimating Equation 1
EQ1=lm(AvgMonRet~LogSize+LogBTM+ETP_IndNeg+ETP_ValPos, data = SignalsDF_1990)
summary(EQ1)
# We can see that a 1% increase in Size leads to a 0.0014 decrease in the
#Average Monthly Returns (AMR), while a 1% increase in Book to Market Ratio
#leads to approximately 0,0015 increase in the AMR
#In addition, if a firm has a negative Earnings to Price Ratio
#then its AMR increases by 0.42 on average, while positive ETP
# decreases the AMR 2.23 on average. R^2=0.01327, while adjusted
#R^2 equals 0.01154. Although, only LogSize coefficient seems to be significant.

#ii)
#Winsorizing Size
L = 0.005
H = 0.995
Size_L = quantile(Signals$Size, prob=L, na.rm=T)
Size_H = quantile(Signals$Size, prob=H, na.rm=T)
Signals$Size[Signals$Size<= Size_L] = Size_L
Signals$Size[Signals$Size>= Size_H] = Size_H

Signals = Signals %>%
  mutate(LogSize = log(Size))

#Winsorizing BTM
BTM_L = quantile(Signals$BTM, prob=L, na.rm=T)
BTM_H = quantile(Signals$BTM, prob=H, na.rm=T)
Signals$BTM[Signals$BTM<= BTM_L] = BTM_L
Signals$BTM[Signals$BTM>= BTM_H] = BTM_H

Signals = Signals %>%
  mutate(LogBTM = log(BTM))

#Winsorizing ETP_ValPos
ETP_ValPos_L = quantile(Signals$ETP_ValPos, prob=L, na.rm=T)
ETP_ValPos_H = quantile(Signals$ETP_ValPos, prob=H, na.rm=T)
Signals$ETP_ValPos[Signals$ETP_ValPos<= ETP_ValPos_L] = ETP_ValPos_L
Signals$ETP_ValPos[Signals$ETP_ValPos>= ETP_ValPos_H] = ETP_ValPos_H

SignalsDF_1990 = Signals[Signals$PFormDate == "30/06/1990", ]

EQ2=lm(AvgMonRet~LogSize+LogBTM+ETP_IndNeg+ETP_ValPos, data = SignalsDF_1990)
summary(EQ2)
# It looks like the data did not have much of outliers, since the coefficients
# have not changed significantly, and both the R^2 and adjusted R^2 have
#decreased.

#iii
#a)BTM 90th, ETP_ValPos 90th, Size 10th

SignalsDF_1990 = Signals[Signals$PFormDate == "30/06/1990", ]

LogSize3A=quantile(SignalsDF_1990$LogSize, prob=0.1, na.rm=T)
LogBTM3A=quantile(SignalsDF_1990$LogBTM, prob=0.9, na.rm=T)
ETP_ValPos3A=quantile(SignalsDF_1990$ETP_ValPos, prob=0.9, na.rm=T)

DF3A=data.frame(LogSize=LogSize3A, LogBTM=LogBTM3A, ETP_ValPos=ETP_ValPos3A,
                ETP_IndNeg=0)

predict(EQ2, DF3A)

# Check how many such observations are in the sample:
SignalsDF_1990_3A=SignalsDF_1990 %>%
  filter(LogSize==quantile(Signals$LogSize, prob=0.1, na.rm=T), 
         LogBTM==quantile(Signals$LogBTM, prob=0.9, na.rm=T),
         ETP_ValPos==quantile(Signals$ETP_ValPos, prob=0.9, na.rm=T))
summary(SignalsDF_1990_3A) # There are NO such observations!

#b) BTM 10th, ETP 10th, Size 90th.

LogSize3B=quantile(SignalsDF_1990$LogSize, prob=0.9, na.rm=T)
LogBTM3B=quantile(SignalsDF_1990$LogBTM, prob=0.1, na.rm=T)
ETP_ValPos3B=quantile(SignalsDF_1990$ETP_ValPos, prob=0.1, na.rm=T)

DF3B=data.frame(LogSize=LogSize3B, LogBTM=LogBTM3B, ETP_ValPos=ETP_ValPos3B,
                ETP_IndNeg=0)
predict(EQ2, DF3B)

# Check how many such observations are in the sample:
SignalsDF_1990_3B=SignalsDF_1990 %>%
  filter(LogSize==quantile(Signals$LogSize, prob=0.9, na.rm=T), 
         LogBTM==quantile(Signals$LogBTM, prob=0.1, na.rm=T),
         ETP_ValPos==quantile(Signals$ETP_ValPos, prob=0.1, na.rm=T))

summary(SignalsDF_1990_3B)# There are NO such observations!

#C) BTM 10th, ETP_Ind=0, Size 90th.

LogSize3C=quantile(SignalsDF_1990$LogSize, prob=0.9, na.rm=T)
LogBTM3C=quantile(SignalsDF_1990$LogBTM, prob=0.9, na.rm=T)

DF3C=data.frame(LogSize=LogSize3C, LogBTM=LogBTM3C, ETP_ValPos=0,
                ETP_IndNeg=1)
predict(EQ2, DF3C)

# Check how many such observations are in the sample:
SignalsDF_1990_3C=SignalsDF_1990 %>%
  filter(LogSize==quantile(Signals$LogSize, prob=0.9, na.rm=T), 
         LogBTM==quantile(Signals$LogBTM, prob=0.9, na.rm=T),
         ETP_ValPos==0, ETP_IndNeg==1)
# There are NO such observations!

#To sum up, first strategy predicts the highest return of 1.34051,
#hence our trading strategy should be holding a long position in firms
#with Small Size, and highest BTM and ETP ratios!

#v
#Fama-French 1992

#Winsorizing Size
L = 0.005
H = 0.995
Size_L = quantile(Signals$Size, prob=L, na.rm=T)
Size_H = quantile(Signals$Size, prob=H, na.rm=T)
Signals$Size[Signals$Size<= Size_L] = Size_L
Signals$Size[Signals$Size>= Size_H] = Size_H

Signals = Signals %>%
  mutate(LogSize = log(Size))

#Winsorizing BTM
BTM_L = quantile(Signals$BTM, prob=L, na.rm=T)
BTM_H = quantile(Signals$BTM, prob=H, na.rm=T)
Signals$BTM[Signals$BTM<= BTM_L] = BTM_L
Signals$BTM[Signals$BTM>= BTM_H] = BTM_H

Signals = Signals %>%
  mutate(LogBTM = log(BTM))

SignalsDF_1990 = Signals[Signals$PFormDate == "30/06/1990", ]

EQ3=lm(AvgMonRet~LogSize+LogBTM, data = SignalsDF_1990)

summary(EQ3)
summary(EQ2)
#The adjusted R^2 has decreased from 0.01132 to 0.008151. Hence, we
#disagree with Fama and French.

#Sloan 1996

#Winsorizing OACC
OACC_L = quantile(Signals$OACC, prob=L, na.rm=T)
OACC_H = quantile(Signals$OACC, prob=H, na.rm=T)
Signals$OACC[Signals$OACC<= OACC_L] = OACC_L
Signals$OACC[Signals$OACC>= OACC_H] = OACC_H

SignalsDF_1990 = Signals[Signals$PFormDate == "30/06/1990", ]

EQ4=lm(AvgMonRet~LogSize+LogBTM+OACC, data = SignalsDF_1990)

summary(EQ2)
summary(EQ4)
# Again adjusted R^2 has decreased, and so did the explanatory power,
# hencewe disagreee with Sloan too.

#VIF
vif(EQ4)

#VIF of LogSize on LogBTM is 1.050066, and VIF of OACC is 1.003660. Since 
# all values are very close to 1, it means they are almost perfectly
# uncorrelated.

#vi

#We have already winsorized the data in the previous task.
#We start by creating new dataset.

SignalsDF_New=Signals %>%
  filter(LogSize!="NA",LogBTM!="NA",AvgMonRet!="NA",
         PFormDate>="30/06/1990",PFormDate<="30/06/2015")

SignalsDF_New=SignalsDF_New %>%
  select("PFormDate", "LogSize", "LogBTM", "AvgMonRet")

#Then we do regressions

fit_FM = lmList(AvgMonRet ~ LogSize+LogBTM| factor(PFormDate), data=SignalsDF_New)

summary_fit_FM = summary(fit_FM)

summary(coefficients(summary_fit_FM)[,,2])#This is estimates of LogSize or B1

summary(coefficients(summary_fit_FM)[,,3])#This is estimates of LogBTM or B2

summary(coefficients(fit_FM)) # Table of estimates

summary((summary_fit_FM)$r.squared) #This is R-squared

# According to results, on average LogBTM has a positive effect on returns,
#while LogSize has negative effect. R-squared equals 0.031872 on average.


#vii

#Let's reupload the Signals data to avoid possible mistakes:
Signals=read.csv(file="Signals_A_62016.csv", header=TRUE, as.is = TRUE, na.strings = c("NA",".",""))


Signals = Signals %>%
  mutate(LogSize = log(Size))
Signals = Signals %>%
  mutate(LogBTM = log(BTM))
SignalsDF_1990 = Signals[Signals$PFormDate == "30/06/1990", ]

SignalsDF_1990Q7 <- SignalsDF_1990 %>%  #NEW Column by classification
  mutate(EXCHNG_11 = ifelse(EXCHG ==11, 1, 0),
         EXCHNG_12 = ifelse(EXCHG ==12, 1, 0),
         EXCHNG_14 = ifelse(EXCHG ==14, 1, 0))

#Omit LogBTM*EXCNG_11 (c) Doron

EQ5=lm(AvgMonRet ~ 0+LogSize+LogBTM+EXCHNG_11+EXCHNG_12+EXCHNG_14
       +LogBTM:EXCHNG_12+LogBTM:EXCHNG_14,
       data = SignalsDF_1990Q7)

summary(EQ5)

#viii
#Regression without intercept is called the regression through origin,
#This model omits b0, because it would not make sense for a firm to have
#Returns if its Size was zero. If we estimated a model with the intercept
#we would have unbiased OLS estimators and ensure Zero conditional mean,
#(MLR3-4). This model certainly needs to include the intercept so follow
#Gauss-Markov assumptions.

#Q2)

#i
#b0 is the intercept, b1 is the AEBE Effect of college on salary (log-level relationship),
#b2 estimates how percentage increase in sales leads to a percentage increase in
#salary. b3 measures the percentage effect of profits, b4 measures the effect of
#tenure, and b5 measures the effect of squared tenure. So that the effect of
#tenure becomes b4+b5*2*ceotenure!


#ii
CEOSAL2=read.table(file="CEOSAL2.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))
CEOSAL2=CEOSAL2[,c('V1','V3','V6','V7','V8','V14')]
colnames(CEOSAL2)=c("salary","college", "ceoten", "sales", "profits","ceotensq")

CEOSAL2$profits[CEOSAL2$profits<0]=NA
CEOSAL2_NEW=na.omit(CEOSAL2)
fit1_p2=lm(log(salary)~college+log(sales)+log(profits)+ceoten+ceotensq,data=CEOSAL2_NEW)
summaryfit1_p2=summary(fit1_p2)
summaryfit1_p2

# If a CEO attended a college, the salary is expected to increase by 0.83331%,
# increase in sales by 1% increases salary by 0.227%, while profits increase
#salary only by 0.023%. The Tenure seems to have a parabolic shape, since
#it increases salary in the begining, but it tends to decrease the salary if a
# tenure becomes too large.

#iii

newDF_p2=data.frame(college=0,sales=quantile(CEOSAL2_NEW$sales, prob=0.5, na.rm=T),profits=quantile(CEOSAL2_NEW$profits, prob=0.7, na.rm=T),ceoten=10,ceotensq=100)
predict(fit1_p2,newDF_p2)

#iv

fit2_p2=lm(salary~college+sales+profits+ceoten+ceotensq,data=CEOSAL2_NEW)
summaryfit2_p2=summary(fit2_p2)
summaryfit2_p2

MoM_Est=mean(exp(fit1_p2$residuals));MoM_Est
MoM_FitVal=MoM_Est*exp(fit1_p2$fitted.values)
(cor(CEOSAL2_NEW$salary,MoM_FitVal))^2
summaryfit2_p2$r.squared

#We would disagree, becayse the Pseudo R^2 of a new model is equal to 0.23,
#it is less than 0.3323 Adjusted R^2 for the first model.