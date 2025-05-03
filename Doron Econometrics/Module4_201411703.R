## Talgat Zharkynbay
## "Module 4 Homework"

### Load packages

```{r load-packages, message = FALSE}
library(dplyr)
library(stargazer)
require(aod)
require(car)
library(Hmisc)
library(lmtest)
library(sandwich)
```
### Q1
```{r}
#i)
FERTIL=read.table(file="FERTIL.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))
colnames(FERTIL)=c("GFR", "PE", "YEAR")
FERTIL$WW2=ifelse(FERTIL$YEAR>=1941 & FERTIL$YEAR<=1945, 1, 0)
FERTIL$PILL=ifelse(FERTIL$YEAR>=1963,1,0)
FERTIL=FERTIL[order(FERTIL$YEAR),]
FERTIL$PE_LAG1=Lag(FERTIL$PE,1)
FERTIL$PE_LAG2=Lag(FERTIL$PE,2)
#ii)
Fit1=lm(GFR~PE+PE_LAG1+PE_LAG2, data = FERTIL)
SummaryFit1=summary(Fit1)
SummaryFit1
#The impact propensity multiplier of PE is estimate of the b1 coefficient and is equal to -0.0158. With a t-value of -0.11 and p-value of 0.91, it is insignificant at the 5% level of the test.
#The estimated effect of a one-dollar change in PE at time t on GFR at time t+1 is equal to the sum of the estimates of the b1 and b2 coefficients and is equal to -0.0158-0.0213=-0.0371. We use F-test to check if the effect is significantly different from zero:
linearHypothesis(Fit1, c("PE+PE_LAG1=0"),test = "F" )
#With a high p-value of 0.79, we can not reject the hypothesis that effect is not significantly different from zero.
#The LRP is equal to the sum of the estimates of the coefficients of b1+b2+b3, or -0.0158-0.0213+0.0539=0.0168. Hence, we can test the significance with the following F-test:
linearHypothesis(Fit1, c("PE+PE_LAG1+PE_LAG2=0"),test = "F" )
#With a high p-value of 0.66, we can not reject the hypothesis that LRP is not significantly different from zero.
#iii)
FERTIL = FERTIL %>%
  mutate(X2 = PE_LAG1-PE, X3=PE_LAG2-PE)
Fit1_LRP_CI=lm(GFR~PE+X2+X3, data = FERTIL)
confint(Fit1_LRP_CI, "PE", level = 0.95, alternative="two.sided")
#This is the 95% CI for LRP, we can see that it contains zero value, which is consistent with the hypothesis we tested above.
#iv)
#First, we add the time trend:
FERTIL$t=1:length(FERTIL$YEAR)
FERTIL = FERTIL %>%
  mutate(t_sq = t^2)
#Next, we estimate our 3 Models:
Fit2_1=lm(GFR~PE+WW2+PILL, data = FERTIL)
Fit2_2=lm(GFR~PE+WW2+PILL+t, data = FERTIL)
Fit2_3=lm(GFR~PE+WW2+PILL+t+t_sq, data = FERTIL)
```
```{r, results='asis'}
stargazer(Fit2_1, Fit2_2, Fit2_3, title="Regression Results", align=TRUE,type="html")
#The first model that ignores time trends predicts that a 1$ increase in the average real dollar value of the personal tax exemption immediately increases the number of children born to every 1,000 women by 0.083. 
#The second model take into account the linear time trend, because GFR in the US displays a clear downward trend over the years. This has increased the estimate of the PE to 0.279, hence  1$ increase in the average real dollar value of the personal tax exemption immediately increases the number of children born to every 1,000 women by 0.279. Hence, after the detrending of the data, we learn that PE actually has larger effect on GFR.
#The third model uses quadratic time trend idea. Since b4 is negative and b5 is positive, we can infer that changes in GFR due to the passage of time has a U-shape. The coefficient of PE is still highly significant with a value of 0.348, which means that PE actually has even higher effect on GFR.
# The third model seems to have the best explanation of the variation in GFR, since it includes the quadratic time trend of GFR. The coefficient estimate of t is equal to -2.531, and the coefficient estimate of t_sq is equal to 0.020. Hence, each year the GFR decreases by -2.531+2*0.020=-2.49. Model 1 and Model 2 underestimate the effect of PE on GFR, because data is not fully detrended. 
# The estimates of R^2 and adjusted R^2 for detrended models contain the upward bias and overestimate the power of the model. Hence, we need to properly find the measures of the R^2:
```
```{r, results='asis'}
Fit2_Detrend=lm(GFR~t, data=FERTIL)
GFR_resid=Fit2_Detrend$residuals
Fit2_Appropriate=lm(GFR_resid~PE+WW2+PILL+t, data=FERTIL)

Fit2_Detrend_t_sq=lm(GFR~t+t_sq, data=FERTIL)
GFR_resid_t_sq=Fit2_Detrend$residuals
Fit2_Appropriate_t_sq=lm(GFR_resid~PE+WW2+PILL+t+t_sq, data=FERTIL)


Regout=stargazer(Fit2_Appropriate, Fit2_Appropriate_t_sq,title="Appropriate Goodness-Of-Fit", align=TRUE,type="html")
#The results confirm our statement that Model 3 has the better explanation. It's appropriate goodness-of-fit measure is equal to 0.586, which is higher than linear trend Model 2 with adjusted R^2 of 0.496, and simple model of 0.450.
```

### Q2
```{r}
#i)
Traffic=read.csv(file="TRAFFIC2_R.csv", header=FALSE, as.is=TRUE,sep = ",", na.strings = c("NA", ".", ""))

View(Traffic)
Traffic=Traffic[, c("V1","V2", "V3","V4", "V5", "V6", "V7", "V8", "V9", "V10" )]
colnames(Traffic)=c("YEAR", "MONTH", "TOTACC", "FATACC", "INJACC", "PDOACC", "UNEM","SPDLAW", "BELTLAW","WKENDS")
Traffic$BELTLAW_lag1=Lag(Traffic$BELTLAW, 1)
Traffic$SPDLAW_lag1=Lag(Traffic$SPDLAW, 1)
Traffic[ Traffic$BELTLAW-Traffic$BELTLAW_lag1==1, c(1,2)]
Traffic[ Traffic$SPDLAW-Traffic$SPDLAW_lag1==1, c(1,2)]
#The belt law took effect in 1986 and the speed law took effect in 1987.
#ii)
Traffic$t= 1:length(Traffic$YEAR)
Fit3=lm(log(TOTACC)~ t + factor(MONTH), data=Traffic)
summaryFit3=summary(Fit3)
summaryFit3
bptest(Fit3, studentize = FALSE)
#Firstly, we see that coefficient estimate on the time trend is highly significant and equal to 0.002747. Hence we have a Log-Level relationship, and each year the number of traffic accidents increases by 0.275% on average. The BP test shows a high p-value, hence we accept homoskedasticity. To test the effect of seasonality, we can do the F-test:
Traffic$FEB=ifelse(Traffic$MONTH=="2", 1,0)
Traffic$MAR=ifelse(Traffic$MONTH=="3", 1,0)
Traffic$APR=ifelse(Traffic$MONTH=="4", 1,0)
Traffic$MAY=ifelse(Traffic$MONTH=="5", 1,0)
Traffic$JUN=ifelse(Traffic$MONTH=="6", 1,0)
Traffic$JUL=ifelse(Traffic$MONTH=="7", 1,0)
Traffic$AUG=ifelse(Traffic$MONTH=="8", 1,0)
Traffic$SEP=ifelse(Traffic$MONTH=="9", 1,0)
Traffic$OCT=ifelse(Traffic$MONTH=="10", 1,0)
Traffic$NOV=ifelse(Traffic$MONTH=="11", 1,0)
Traffic$DEC=ifelse(Traffic$MONTH=="12", 1,0)

Fit3=lm(log(TOTACC)~ t + FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+
          NOV+DEC, data=Traffic)

linearHypothesis(Fit3, c("FEB=0","MAR=0","APR=0","MAY=0","JUN=0",
                         "JUL=0","AUG=0","SEP=0","OCT=0","NOV=0","DEC=0"
), test = "F")
#The p-value is very low, hence we can conclude that there is a seasonality in total accidents.

#iii)
Fit4=lm(log(TOTACC)~ t + factor(MONTH)+UNEM+SPDLAW+BELTLAW+WKENDS, data=Traffic)
summaryFit4=summary(Fit4)
summaryFit4
bptest(Fit4, studentize = FALSE)
#We again accept homoskedasticity. With regards to UNEM estimates, the regression predicts that on average the number of traffic accidents decreases by 2.12% with a 1 unit increase in unemployment rate. This might make sense, since unemployed people might have less money to buy cars and hence they will have less car accidents. With the introduction of Speedlaw the number of traffic accidents decreases by 5.38% which is sort of expected. However, with the introduction of Beltlaw the number of traffic accidents increases by 9.55%, which is very unexpected.
#iv)
Traffic$PRCFAT=(Traffic$FATACC/Traffic$TOTACC)*100
mean(Traffic$PRCFAT)
#The results suggest that 0.886% of total accidents are fatal. In other words, almost 1 out of 100 car accidents results in death of at least one passenger. The magnitude does not seem to be unusual. 
#v)
Fit5=lm(PRCFAT~ t + factor(MONTH)+UNEM+SPDLAW+BELTLAW+WKENDS, data=Traffic)
summaryFit5=summary(Fit5)
summaryFit5
#The belt law decreases the percentage of fatal accidents by 0.029%, but the estimate is statistically insignificant. The speed law, on the contrary increased the number of fatal accident 0.067% and is statistically significant, which is very counterintuitive. Hence, it seems that introduction of the laws did not reach the desired effects.
#vi)
Traffic$PRCFAT_lag1=Lag(Traffic$PRCFAT, 1)
Traffic$UNEM_lag1=Lag(Traffic$UNEM, 1)
Fit6=lm(PRCFAT~PRCFAT_lag1, data=Traffic)
summaryFit6=summary(Fit6)
summaryFit6
bptest(Fit6, studentize = FALSE) # accept homoskedasticity
confint(Fit6, "PRCFAT_lag1", level = 0.95, alternative="two.sided")
#We are not worried about the unit root, since both the CI and point estimate indicate that rho is less than 0.8, and the estimate is significant.

Fit7=lm(UNEM ~ UNEM_lag1, data=Traffic)
summaryFit7=summary(Fit7)
summaryFit7
bptest(Fit7, studentize = FALSE) # accept homoskedasticity
confint(Fit7, "UNEM_lag1", level = 0.95, alternative="two.sided")
#Here, we are worried about the unit root, because the estimate is very close to 1 and significant, meaning that the variable follows a random walk, hence the usual inference procedures will not be valid.
#vii)
Traffic$PRCFAT_DIF=Traffic$PRCFAT-Traffic$PRCFAT_lag1
Traffic$UNEM_DIF=Traffic$UNEM-Traffic$UNEM_lag1

Fit8=lm(PRCFAT_DIF~UNEM_DIF+SPDLAW+BELTLAW+WKENDS+t+FEB+MAR+APR+MAY+JUN+JUL+AUG+SEP+OCT+NOV+DEC,data=Traffic)

SummaryFit8=summary(Fit8)
SummaryFit8
```
```{r, results='asis'}
stargazer(Fit5, Fit8, title="Non-difference vs Difference", align=TRUE,type="html")
# There are very interesting results, since with the regression with differences we see that UNEM, SPDLAW and time trend variables became insignificant. In the question number #v) we have notived counterintuitive results, and now we see that these results are not true, because variables are statistically insignificant, because we have removed the linear time trend and most of serial correlation. However, there is still some evidence for seasonality of fatal accidents.
#viii)
#This is a very difficult question for even professional econometricians. We have rejected unit root for PRCFAT and accepted it for UNEM, but since we did the difference regression, we have noticed that other variables can not actually explain the changes in PRCFAT. Hence, the results from difference regression DO not always give results similar to levels, but we still need to do difference regression if we suspect a unit root.
#ix)
Fit9=lm(PRCFAT~t+factor(MONTH)+UNEM+SPDLAW+BELTLAW+WKENDS, data=Traffic)
U=Traffic$Fit_resid=Fit9$residuals
U_lag1=Traffic$Fit_resid_lag1=Lag(Traffic$Fit_resid, 1)

Fit_AR1=lm(U~U_lag1, data=Traffic)
summaryFit_AR1=summary(Fit_AR1)
summaryFit_AR1
#The results of the regression show that the coefficient estimate of U_lag1 is 0.282 and highly significant with t-value of 2.985 and p-value of 0.00353. Hence, we can conclude that the errors AR(1) are serially correlated. The idea os strict exogeneity does not seem to make sense, since strictly exogeneous explanatory variables can not react to what has happened to Yt in the past. Our sample contains only 108 variables, which I consider a small sample (small n), hence the strict exogeneity assumption does not make sense, in my opinion.
Fit_AR2=lm(U~U_lag1+t+factor(MONTH)+UNEM+SPDLAW+BELTLAW+WKENDS, data=Traffic)
summaryFit_AR2=summary(Fit_AR2)
summaryFit_AR2
#This regression does not require strict exogeneity. We get the estimate of 0.284, with t-value of 2.762 and p-value of 0.00697, which is statistically significant. Hence, there is serial correlation of error terms.
#x)
vcovHAC=vcovHAC(Fit9, weights=bwNeweyWest)
vcovHAC1=vcovHAC(Fit9, weghts=bwAndrews)
coeftest(Fit9, vcov.=vcovHAC)
coeftest(Fit9, vcov.=vcovHAC1)
#The test with Newey-West standard errors demonstrates that both estimates of SPDLAW and BELTLAW are not significantly different from zero. The Andrews test on the contrary shows significance of SPDLAW and BELTLAW.
summaryFit5
#HAC standard errors affect the significance of two policy variables. From part v) SPDLAW variable was significant, while BELTLAW variable was insignificant. NEwey-West standard errors demonstrated the lack of significance, while Andrews standard erros demonstrate that both policy variables are significant.
```

### Q3
```{r}
#i)
#The H0 for AR(1) would be that b1=0, and for AR(2) H0 would be b1=b2=0.
#ii)
#Yes, AssumptionTS#3 holds under each of null hypotheses, because null hypotheses state that stock returns are serially uncorrelated, so we can safely assume that they are weakly dependent.
#iii)
NYSE=read.csv(file="NYSE.csv", header=TRUE, as.is=TRUE,sep = ",", na.strings = c("NA", ".", ""))
NYSE$RET_lag1=Lag(NYSE$RET, 1)
NYSE$RET_lag2=Lag(NYSE$RET, 2)
#iv)
Fit_AR3=lm(RET~RET_lag1, data=NYSE)
summaryFit_AR3=summary(Fit_AR3)
summaryFit_AR3
# The estimates tell us that there is positive effect of 0.0589 on returns from one week to next.

Fit_AR4=lm(RET~RET_lag1+RET_lag2, data=NYSE)
summaryFit_AR4=summary(Fit_AR4)
summaryFit_AR4
# The estimates of b1 tell us that there is positive effect of 0.06032 on returns from one week to next, but estimate of b2 has a negative effect of 0.03807. Hence, from one week to next there is a positive effect, but the week before past week has a negative effect on returns. 
#v)
#AR(1) H0: b1=0. We would not reject the null, since t-value of b1 estimate is 1.549 and p-value is 0.1218.
#AR(2) H0:b1=b2=0. Assuming homoskedasticity, we can run the standard F-test to test the null hypothesis:
linearHypothesis(Fit_AR4, c("RET_lag1=0", "RET_lag2=0"), test = "F")
#With a p-value of 0.1912, we can not reject the null hypothesis.
#vi)
bptest(Fit_AR3, studentize = FALSE)
bptest(Fit_AR4, studentize = FALSE)
#We find that there is no homscedasticity in both AR(1) and AR(2) models. Hence, we need to use heteroskedasticity robust standard errors:
vcovHC=vcovHC(Fit_AR3, type = "HC0")
linearHypothesis(Fit_AR3, "RET_lag1=0", vcov. = vcovHC)
#Even with robust SE's we can not reject the null hypothesis. Hence, we ahd valid inference in v).
vcovHC1=vcovHC(Fit_AR4, type = "HC0")
linearHypothesis(Fit_AR4, c("RET_lag1=0", "RET_lag2=0"), vcov. = vcovHC1,  test="F")
#Same here. we do not reject H0 and hence we had valid inferences in part v).
```


