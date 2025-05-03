#############
############# Sample Exam
#############
require("nlme")
require(doBy)
require(plyr)
require(Hmisc)
require(DataCombine)
require(moments)
require(tseries)
require(stargazer)
require(dplyr)
require(plot)
require(nlme)
require(tseries)
require(foriegn)
require(sandwich)
require(lmtest)
require(car)
require(lfe)
### Part A

setwd("/Users/shrwydbrg/Desktop")
Signals=read.csv(file="Signals_A_62016.csv", header=TRUE, as.is = TRUE, na.strings = c("NA",".",""))

### Q1 
Signals$PFormDate=as.Date(Signals$PFormDate,format = "%d/%m/%Y")
Signals$CompInd=ifelse(Signals$BBHL=="Computers",1,0)
describe(Signals$PermNo)
#119463
#8859--> Unique
describe(Signals$PFormDate)
#119463
#53--> Unique
describe(Signals$BBHL)
#119463
#18--> Unique
describe(Signals$CompInd)
#15770
describe((Signals$PermNo)[Signals$CompInd=="1"])
#1582--> Unique

### Q2
CrossSecDF_New = Signals[Signals$PFormDate=="1992-06-30" &
                         !is.na(Signals$Size) & !is.na(Signals$BTM) & !is.na(Signals$ETP) & !is.na(Signals$AvgMonRet),  ]
describe(CrossSecDF_New)
describe((CrossSecDF_New$PermNo)[CrossSecDF_New$CompInd=="1"])
describe((CrossSecDF_New$PermNo)[CrossSecDF_New$CompInd=="0"])

### Part B

### Q3
### introduce log(Size), log(BTM)variables
CrossSecDF_New$logSize=log(CrossSecDF_New$Size)
CrossSecDF_New$logBTM=log(CrossSecDF_New$BTM)

### estimate the three models
FIT_combined=lm(AvgMonRet~logSize+logBTM+ETP, data =CrossSecDF_New)
SummaryFIT_combined=summary(FIT_combined);SummaryFIT_combined

CrossSecDF_Computers=CrossSecDF_New[CrossSecDF_New$CompInd=="1",]

FIT_Computers=lm(AvgMonRet~logSize+logBTM+ETP, data =CrossSecDF_Computers)
SummaryFIT_Computers=summary(FIT_Computers);SummaryFIT_Computers

CrossSecDF_Others=CrossSecDF_New[CrossSecDF_New$CompInd=="0",]

FIT_Others=lm(AvgMonRet~logSize+logBTM+ETP, data =CrossSecDF_Others)
SummaryFIT_Others=summary(FIT_Others);SummaryFIT_Others

stargazer(FIT_combined,FIT_Computers,FIT_Others,type = "text",out = "1.htm")

#Test for homoskedasticity
bptest(FIT_combined,studentize = FALSE)
bptest(FIT_Computers,studentize = FALSE)
bptest(FIT_Others,studentize = FALSE)


### find confidence intervals under heteroskedasticity
vcovHC0=vcovHC(FIT_combined,type="HC0")
coefci(FIT_combined,vcov.=vcovHC0, "logBTM",level=0.95,alternative="two.sided")
vcov=vcov(FIT_Computers)
coefci(FIT_Computers,vcov.=vcov, "logBTM",level=0.95,alternative="two.sided")
vcovHC0=vcovHC(FIT_Others,type="HC0")
coefci(FIT_Others,vcov.=vcovHC0, "logBTM",level=0.95,alternative="two.sided")


#find two-tailed 95% interval estimate for the difference between skewness of errors 
#using 
residuals = cbind(FIT_Others$residuals, FIT_Computers$residuals)
orig_sample = residuals
n = dim(residuals)[1]
orig_index = 1:n

### Generate 10,000 Bootstrap indexes and corresponding samples (resamples) of sizes n from the original paired sample
### Compute 10,000 Bootstrap estimates of the estimator (one for each resample) to generate the Bootstrap sampling distribution of the estimator 
boot_estimates = rep(NA, times=10000)

for(i in 1:10000){
  boot_index = sample(orig_index, n, replace=TRUE)
  boot_sample = orig_sample[boot_index, ]
  boot_estimates[i] = skewness((boot_sample)[ ,1])- skewness((boot_sample)[ ,2])
}

### Bootstrap 95% confidence intervals for R-squared
### Using the standard error method
se_boot = sd(boot_estimates) 
orig_sample_estimate = skewness((boot_sample)[ ,1])- skewness((boot_sample)[ ,2])

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
                    orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

# Using the percentile method
qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method 

### Q4
FIT2_combined=lm(AvgMonRet~logSize+logBTM+ETP+CompInd+CompInd*logBTM, data =CrossSecDF_New)
SummaryFIT2_combined=summary(FIT2_combined);SummaryFIT2_combined

#Test for homoskedasticity
bptest(FIT2_combined,studentize = FALSE)

#Hypothesis testing
vcovHC0=vcovHC(FIT2_combined,type="HC0")
linearHypothesis(FIT2_combined,"logBTM=0",vcov.=vcovHC0)

linearHypothesis(FIT2_combined,c("logBTM+logBTM:CompInd=0"),test="Chisq",vcov.=vcovHC0)

### Q5
FIT_PLM=lm(CompInd~logSize+logBTM+ETP+AvgMonRet, data =CrossSecDF_New)
SummaryFIT_PLM=summary(FIT_PLM);SummaryFIT_PLM

FIT_Logit=glm(CompInd~logSize+logBTM+ETP+AvgMonRet, data =CrossSecDF_New,family=binomial(link="logit"))
SummaryFIT_Logit=summary(FIT_Logit);SummaryFIT_Logit

stargazer(FIT_PLM,FIT_Logit,type = "text",out="Q5.htm")

#### The partial effects of AvgMonRet on CompInd at the average (PEA).

Means=data.frame(AvgMonRet=mean(CrossSecDF_New$AvgMonRet),logSize=mean(CrossSecDF_New$logSize),logBTM=mean(CrossSecDF_New$logBTM),ETP=mean(CrossSecDF_New$ETP))
describe(CrossSecDF_New$AvgMonRet)
ScaleFactor_Logit_1=dlogis(predict(FIT_Logit,Means),location=0,scale=1)
PEA_Logit_AvgMonRet=ScaleFactor_Logit_1*FIT_Logit$coefficients["AvgMonRet"]
PEA_Logit_AvgMonRet

