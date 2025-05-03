library(dplyr)
library(ggplot2)
library(nlme)
library(car)
library(stargazer)
library(aod)
library(car)
library(sandwich)
library(lattice)
library(latticeExtra)
#Q1
JTRAIN2 = read.table('JTRAIN2.txt', sep = '', header=FALSE,as.is=TRUE,na.strings = c("NA",".",""))

colnames(JTRAIN2) = c('train','age','educ','black','hisp','married',
                      'nodegree','mosinex','re74','re75','re78',
                      'unem74','unem75','unem78','lre74',
                      'lre75','lre78','agesq','mostrn')

JTRAIN_NEW=JTRAIN2[,c('train','age','educ','black','hisp','married',
                      'unem74','unem75','unem78')]

#Q2
sum(JTRAIN_NEW$train)
#If a person is participating, then train=1. Hence, 185 people participated.

#Q3
max(JTRAIN2$mostrn)
#24 months is the maximum number a person has participated.

#Q4
#First, let's estimate our models:
#Model 1:
Fit_LPM=lm(unem78~train+age+educ+black+hisp+married+unem74+unem75, 
           data = JTRAIN_NEW)
Summary_Fit_LPM=summary(Fit_LPM)

#Model 2:
Fit_Logit=glm(unem78~train+age+educ+black+hisp+married+unem74+unem75, 
           data = JTRAIN_NEW, family = binomial(link = "logit"))
Summary_Fit_Logit=summary(Fit_Logit)

#Model 3:
Fit_Probit=glm(unem78~train+age+educ+black+hisp+married+unem74+unem75, 
              data = JTRAIN_NEW, family = binomial(link = "probit"))
Summary_Fit_Probit=summary(Fit_Probit)

#Analysing Train and and EDUC
stargazer(Fit_LPM, Fit_Logit, Fit_Probit, title="Regression Results", align=TRUE,type="html")

#Q5
### Train, discrete variable:
#Approach 1, PEA for Logit:
Means_train_eq1 = data.frame(train=1, age=mean(JTRAIN_NEW$age), 
                   educ=mean(JTRAIN_NEW$educ),black=mean(JTRAIN_NEW$black), 
                   hisp=mean(JTRAIN_NEW$hisp),
                   married=mean(JTRAIN_NEW$married),
                   unem74=mean(JTRAIN_NEW$unem74), 
                   unem75=mean(JTRAIN_NEW$unem75))

Means_train_eq0 = data.frame(train=0, age=mean(JTRAIN_NEW$age), 
                             educ=mean(JTRAIN_NEW$educ),black=mean(JTRAIN_NEW$black), 
                             hisp=mean(JTRAIN_NEW$hisp),
                             married=mean(JTRAIN_NEW$married),
                             unem74=mean(JTRAIN_NEW$unem74), 
                             unem75=mean(JTRAIN_NEW$unem75))

CDF_Logit_train_eq_1=plogis(predict(Fit_Logit, Means_train_eq1), location = 0,
                            scale = 1)
CDF_Logit_train_eq_1

CDF_Logit_train_eq_0=plogis(predict(Fit_Logit, Means_train_eq0), location = 0,
                            scale = 1)
CDF_Logit_train_eq_0

PEA_Logit=CDF_Logit_train_eq_1-CDF_Logit_train_eq_0
PEA_Logit
# AEBE, a change in Train from 0 to 1, decreases the probability that an
# average person is unemployed for 1978 by 0.112445.

#Approach 1, PEA for Probit:
CDF_Probit_train_eq_1=pnorm(predict(Fit_Probit, Means_train_eq1), mean = 0,
                            sd = 1)
CDF_Probit_train_eq_1

CDF_Probit_train_eq_0=pnorm(predict(Fit_Probit, Means_train_eq0), mean = 0,
                            sd = 1)
CDF_Probit_train_eq_0

PEA_Probit=CDF_Probit_train_eq_1-CDF_Probit_train_eq_0
PEA_Probit
# AEBE, a change in Train from 0 to 1, decreases the probability that an
# average person is unemployed for 1978 by 0.1143574.

#Approach 2, APE for Logit:
Data_train_eq1 = data.frame(train=1, age=JTRAIN_NEW$age, 
                            educ=JTRAIN_NEW$educ,black=JTRAIN_NEW$black, 
                            hisp=JTRAIN_NEW$hisp,
                            married=JTRAIN_NEW$married,
                            unem74=JTRAIN_NEW$unem74, 
                            unem75=JTRAIN_NEW$unem75)

Data_train_eq0 = data.frame(train=0, age=JTRAIN_NEW$age, 
                            educ=JTRAIN_NEW$educ,black=JTRAIN_NEW$black, 
                            hisp=JTRAIN_NEW$hisp,
                            married=JTRAIN_NEW$married,
                            unem74=JTRAIN_NEW$unem74, 
                            unem75=JTRAIN_NEW$unem75)


MeanCDF_Logit_train_eq_1=mean(plogis(predict(Fit_Logit, Data_train_eq1), 
                                     location = 0,scale = 1))
MeanCDF_Logit_train_eq_1

MeanCDF_Logit_train_eq_0=mean(plogis(predict(Fit_Logit, Data_train_eq0), 
                                     location = 0,scale = 1))
MeanCDF_Logit_train_eq_0

APE_Logit=MeanCDF_Logit_train_eq_1-MeanCDF_Logit_train_eq_0
APE_Logit
#AEBE, on average, a change in Train from 0 to 1, decreases the probability 
#that a person will be unemployed in 1978 by 0.1112376.

#Approach 2, APE for Probit:

MeanCDF_Probit_train_eq_1=mean(pnorm(predict(Fit_Probit, Data_train_eq1), 
                                     mean = 0,sd = 1))
MeanCDF_Probit_train_eq_1

MeanCDF_Probit_train_eq_0=mean(pnorm(predict(Fit_Probit, Data_train_eq0), 
                                     mean = 0,sd = 1))
MeanCDF_Probit_train_eq_0

APE_Probit=MeanCDF_Probit_train_eq_1-MeanCDF_Probit_train_eq_0
APE_Probit

#AEBE, on average, a change in Train from 0 to 1, decreases the probability 
#that a person will be unemployed in 1978 by 0.1123301.

### EDUC, continuous variable:
#Approach 1, PEA for Logit:
Means_cv = data.frame(train=mean(JTRAIN_NEW$train), 
                      age=mean(JTRAIN_NEW$age), 
                      educ=mean(JTRAIN_NEW$educ),
                      black=mean(JTRAIN_NEW$black), 
                      hisp=mean(JTRAIN_NEW$hisp),
                      married=mean(JTRAIN_NEW$married),
                      unem74=mean(JTRAIN_NEW$unem74), 
                      unem75=mean(JTRAIN_NEW$unem75))

ScaleFactor_Logit_PEA=dlogis(predict(Fit_Logit, Means_cv), location=0, scale=1)

ScaleFactor_Logit_PEA

PEA_Logit_CV=ScaleFactor_Logit_PEA*Fit_Logit$coefficients["educ"] 

PEA_Logit_CV

#AEBE, an extra year of education, decreases the probability that average
#person will be unemployed in 1978 by approx. 0.00033.

#Approach 1, PEA for Probit:
ScaleFactor_Probit_PEA=dnorm(predict(Fit_Probit, Means_cv), mean=0, sd=1)

ScaleFactor_Probit_PEA

PEA_Probit_CV=ScaleFactor_Probit_PEA*Fit_Probit$coefficients["educ"] 

PEA_Probit_CV

#AEBE, an extra year of education, decreases the probability that average
#person will be unemployed in 1978 by approx. 0.000654.

#Approach 2, APE for Logit:
ScaleFactor_Logit_APE=mean(dlogis(predict(Fit_Logit), location=0, 
                                  scale=1))

ScaleFactor_Logit_APE

APE_Logit_CV=ScaleFactor_Logit_APE*Fit_Logit$coefficients["educ"] 

APE_Logit_CV

#AEBE, on average, an extra year of education, decreases the probability of
#being unemployed for 1978 by 0.00032.

#Approach 2, APE for Probit:
ScaleFactor_Probit_APE=mean(dnorm(predict(Fit_Probit), mean=0, 
                                   sd=1))

ScaleFactor_Probit_APE

APE_Probit_CV=ScaleFactor_Probit_APE*Fit_Probit$coefficients["educ"] 

APE_Probit_CV

#AEBE, on average, an extra year of education, decreases the probability of
#being unemployed for 1978 by 0.000635.

#Q6
predict_Logit = predict(Fit_Logit, type="response")
predict_Probit = predict(Fit_Probit, type="response")
predict_LPM=predict(Fit_LPM)


#Logit
predict_Logit_min=quantile(predict_Logit,prob=0,na.rm=T)
predict_Logit_max=quantile(predict_Logit,prob=1,na.rm=T)
predict_Logit5=quantile(predict_Logit,prob=0.05,na.rm=T)
predict_Logit25=quantile(predict_Logit,prob=0.25,na.rm=T)
predict_Logit50=quantile(predict_Logit,prob=0.50,na.rm=T)
predict_Logit75=quantile(predict_Logit,prob=0.75,na.rm=T)
predict_Logit95=quantile(predict_Logit,prob=0.95,na.rm=T)
predicted_Logit=c(predict_Logit5, predict_Logit25, predict_Logit50, predict_Logit75, predict_Logit95)
predicted_Logit

#Probit
predict_Probit_min=quantile(predict_Probit,prob=0,na.rm=T)
predict_Probit_max=quantile(predict_Probit,prob=1,na.rm=T)
predict_Probit5=quantile(predict_Probit,prob=0.05,na.rm=T)
predict_Probit25=quantile(predict_Probit,prob=0.25,na.rm=T)
predict_Probit50=quantile(predict_Probit,prob=0.50,na.rm=T)
predict_Probit75=quantile(predict_Probit,prob=0.75,na.rm=T)
predict_Probit95=quantile(predict_Probit,prob=0.95,na.rm=T)
predicted_Probit=c(predict_Probit5, predict_Probit25, predict_Probit50, predict_Probit75, predict_Probit95)
predicted_Probit


#LPM
predict_LPM_min=quantile(predict_LPM,prob=0,na.rm=T)
predict_LPM_max=quantile(predict_LPM,prob=1,na.rm=T)
predict_LPM5=quantile(predict_LPM,prob=0.05,na.rm=T)
predict_LPM25=quantile(predict_LPM,prob=0.25,na.rm=T)
predict_LPM50=quantile(predict_LPM,prob=0.50,na.rm=T)
predict_LPM75=quantile(predict_LPM,prob=0.75,na.rm=T)
predict_LPM95=quantile(predict_LPM,prob=0.95,na.rm=T)
predicted_LPM=c(predict_LPM5, predict_LPM25, predict_LPM50, predict_LPM75, predict_LPM95)
predicted_LPM


summary(predicted_Logit)
summary(predicted_Probit)
summary(predicted_LPM)

#Q7
NewDataQ7=data.frame(train=1, age=52, educ=16, black=0, hisp=0, married = 1, 
                     unem74=1, unem75=1)
predict_LogitQ7 = predict(Fit_Logit, NewDataQ7, type="response")
predict_ProbitQ7 = predict(Fit_Probit, NewDataQ7, type="response")
predict_LMPQ7=predict(Fit_LPM, NewDataQ7)
predict_LogitQ7 # The probability is 0.106759 
predict_ProbitQ7# The probability is 0.105627 
predict_LMPQ7# The probability is 0.08525644 

#Q8
table(Fit_Probit$fitted.values>0.5, JTRAIN_NEW$unem78)
table(Fit_Logit$fitted.values>0.5, JTRAIN_NEW$unem78)
table(Fit_LPM$fitted.values>0.5, JTRAIN_NEW$unem78)
#This measure shows the same results, because all values for all 3 models are
#less than 0.5. Hence we can not compare the effectiveness using this approach.
#Let's use Option 3 from slide 33:
rsquared_logit=(cor(JTRAIN_NEW$unem78, Fit_Logit$fitted.values, 
                    use="complete.obs", method="pearson"))^2

rsquared_probit=(cor(JTRAIN_NEW$unem78, Fit_Probit$fitted.values, 
                     use="complete.obs", method="pearson"))^2

SummaryFIT_LPM=summary(Fit_LPM)

rsquared_logit
rsquared_probit
SummaryFIT_LPM$r.squared
#The LPM Model has slightly higher "usual" R^2 than pseudo R^2 for Logit and
#Probit models. Hence, LPM is more accurate.

#Q9


#Q10
linearHypothesis(Fit_LPM, c("married=0", "black=0", "hisp=0"), test="F")
wald.test(b=Fit_Logit$coefficients, Sigma=vcov(Fit_Logit), Terms=c(5,6,7))
wald.test(b=Fit_Probit$coefficients, Sigma=vcov(Fit_Probit), Terms=c(5,6,7))

#Q11
table(Fit_LPM$fitted.values>mean(JTRAIN_NEW$unem78),JTRAIN_NEW$unem78)
#Correctly predicted for UNEM78=1: 84/(84+53)=61.31%
#Correctly predicted for UNEM78=0: 177/(177+131)=57.46%
#Overall correctly predicted=58.65%
table(Fit_Probit$fitted.values>mean(JTRAIN_NEW$unem78),JTRAIN_NEW$unem78)
#Correctly predicted for UNEM78=1: 84/(84+53)=61.31%
#Correctly predicted for UNEM78=0: 177/(177+131)=57.46%
#Overall correctly predicted=58.65%
table(Fit_Logit$fitted.values>mean(JTRAIN_NEW$unem78),JTRAIN_NEW$unem78)
#Correctly predicted for UNEM78=1: 84/(84+53)=61.31%
#Correctly predicted for UNEM78=0: 177/(177+131)=57.46%
#Overall correctly predicted=58.65%

#This approach performs much better, because at the threshold of 0.5, we had
#0 true values predicted correctly. Now, we have correct results with the mean
#approach, because we our prediction models do not have fitted values greater
#than 0.5.


