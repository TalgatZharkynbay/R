library(dplyr)
library(sandwich)
library(car)
library(lmtest)
library(lfe)
#Part A
Signals=read.csv(file="Signals_A_62016.csv", header=TRUE, as.is = TRUE, na.strings = c("NA",".",""))
SignalsDF_1992 = Signals[Signals$PFormDate == "30/06/1992", ]
##Q1)
EQ1=lm(AvgMonRet~log(Size)+log(BTM)+ROE+NSI+NXF, data = SignalsDF_1992)
SummaryEQ1=summary(EQ1)
#i)
SummaryEQ1$coefficients
#A 1% increase in Size decreases the Average Returns by -0.00290.
#A 1% increase in BTM increases the Average Returns by 0.00625.
#A 1 unit increase in ROE decreases the Average Returns by 0.068.
#A 1 unit increase in NSI decreases the Average Returns by 0.961.
#A 1 unit increase in NXF decreases the Average Returns by -0.0001.
#The formula for bj(hat) is bj+ΣVi,j(hat)*Ui/ΣV^2i,j(hat)
#The formula for b0 is b0+(b1-b1(hat))*x1(bar)+...+(bp-bp(hat))*xp(bar)+U(hat)

#ii)
SummaryEQ1$coefficients
#ROE and NXF have large standard errors, and hence we can not reject
#the null hypothesis that b3 and b4 are equal to zero.
#The formula for standard error is sqrt(Σ(Y-Y(hat))^2)/n)

#iii)
SummaryEQ1$coefficients
#Coefficients with large SE's have smaller t-values. Hence, ROE and NXF have
#smaller t-values. Formula for t-value is (bj(hat)-bj)/se(bj(hat))

#iv)
SummaryEQ1$sigma
# The residual standard error is calculated as sqrt(SSR/n-p-1), or it is
# the square root of the Mean Squared Error.

#v)
SummaryEQ1$r.squared
SummaryEQ1$adj.r.squared

#R^2 and adjusted R^2 are measurements of Goodness-Of-Fit for the MLR.
#R^2=SSE/SST, while Ra^2=1-(n-1)*(1-R^2)/(n-p-1)

#vi)
SummaryEQ1$fstatistic
#F-statistic checks if the MLR model's coefficients are significant overall.
#The formula is ((SSRr-SSRur)/q)/(SSRur/n-p-1). Our model has high F-stat and
#hence we reject the null hypothesis that all coefficients are zero.

##Q2) The White F-test
WhiteTestDF=data.frame(UhatSQ=SummaryEQ1$residuals^2,Yhat=fitted(EQ1),
                       YhatSQ=fitted(EQ1)^2)

Test=lm(UhatSQ~Yhat+YhatSQ, data = WhiteTestDF)

vcovHC0=vcovHC(Test, type = "HC0")

linearHypothesis(Test, c("Yhat=0", "YhatSQ=0"), vcov. =vcovHC0, test = "F")

#The p-value is above the chosen significance level of 2%, hence we accept
#homoskedasticity.

##Q3)

#i)
confint(EQ1, "log(Size)", level = 0.95, alternative="two.sided")
confint(EQ1, "log(BTM)", level = 0.95, alternative="two.sided")
confint(EQ1, "ROE", level = 0.95, alternative="two.sided")
confint(EQ1, "NSI", level = 0.95, alternative="two.sided")
confint(EQ1, "NXF", level = 0.95, alternative="two.sided")
#When there is homoskedasticity, we can use OLS SE's with confint() function.

#ii)
newDF=data.frame(Size=quantile(SignalsDF_1992$Size, prob=0.2, na.rm=T),
                 NSI=quantile(SignalsDF_1992$NSI, prob=0.2, na.rm=T),
                 NXF=quantile(SignalsDF_1992$NXF, prob=0.2, na.rm=T),
                 BTM=quantile(SignalsDF_1992$BTM, prob=0.8, na.rm=T), 
                 ROE=quantile(SignalsDF_1992$ROE, prob=0.5, na.rm=T))

predict(EQ1, newDF)
# This is a MEAN/EXPECTED value of Average Monthly Returns. Now we need a CI:

SignalsDF_1992$Size_New=SignalsDF_1992$Size-quantile(SignalsDF_1992$Size, prob=0.2, na.rm=T)
SignalsDF_1992$BTM_New=SignalsDF_1992$BTM-quantile(SignalsDF_1992$BTM, prob=0.8, na.rm=T)
SignalsDF_1992$ROE_New=SignalsDF_1992$ROE-quantile(SignalsDF_1992$ROE, prob=0.5, na.rm=T)
SignalsDF_1992$NSI_New=SignalsDF_1992$NSI-quantile(SignalsDF_1992$NSI, prob=0.2, na.rm=T)
SignalsDF_1992$NXF_New=SignalsDF_1992$NXF-quantile(SignalsDF_1992$NXF, prob=0.2, na.rm=T)

EQ1_New=lm(AvgMonRet~log(Size_New)+log(BTM_New)+ROE_New+NSI_New+NXF_New, 
           data = SignalsDF_1992)
SummaryEQ1_New=summary(EQ1_New)

confint(EQ1_New, "(Intercept)", level = 0.95, alternative="two.sided")

#This is Interval estimate for predicted mean of AvgMonRet. Now we can do 
#the Interval estimate for the specific value:
SE_Spc=sqrt(vcov(EQ1_New)[1,1]+SummaryEQ1$sigma^2)
SE_Spc
EQ1_New$coefficients["(Intercept)"]-qt(p=0.975, df=1875)*SE_Spc
EQ1_New$coefficients["(Intercept)"]-qt(p=0.025, df=1875)*SE_Spc
#The interval estimate for the specific value is [-5.34683, 11.91045]

#iii)
# We can use the F-Test with the linearhypothesis function
linearHypothesis(EQ1, c("ROE=0", "NSI=0", "NXF=0"),test = "F" )
#We have a 5% significance level and p-value of 0.2351, which is much higher.
#Hence, we accept H0 and conclude that b3,b4 and b5 are not significantly
#different from zero.

#iv)
linearHypothesis(EQ1, "NSI-3*log(Size)=0")
#Again we have very high p-value, hence we fail to reject the H0 and we
#conclude that  in the population of
#interest the partial effect of NSI on future returns is 3 times as big as the
#partial effect of log(Size)

#v)
linearHypothesis(EQ1, c("ROE=0","NXF=0",  "NSI-3*log(Size)=0"),
                 test = "F" )
#I used the F-test. Again at the p-value of 0.9733 we fail to reject H0,
#and we conclude that statement is true.

##Q4)
bptest(EQ1)
#The p-value is below the signifcance level of 7%. Hence, we reject the H0
#of homoskedasticity.

##Q5)
#We will now use the Robust interval estimates with coefci function

#i) 
vcovHC0=vcovHC(EQ1, type = "HC0")
coefci(EQ1, "log(Size)", level = 0.95, vcov. = vcovHC0)
coefci(EQ1, "log(BTM)", level = 0.95, vcov. = vcovHC0)
coefci(EQ1, "ROE", level = 0.95, vcov. = vcovHC0)
coefci(EQ1, "NSI", level = 0.95, vcov. = vcovHC0)
coefci(EQ1, "NXF", level = 0.95, vcov. = vcovHC0)
#Since we rejected homoskedasticity in Q4, we used Robust Standard Errors and
#received robust confidence intervals for our coefficients.

#ii)
newDF=data.frame(Size=quantile(SignalsDF_1992$Size, prob=0.2, na.rm=T),
                 NSI=quantile(SignalsDF_1992$NSI, prob=0.2, na.rm=T),
                 NXF=quantile(SignalsDF_1992$NXF, prob=0.2, na.rm=T),
                 BTM=quantile(SignalsDF_1992$BTM, prob=0.8, na.rm=T), 
                 ROE=quantile(SignalsDF_1992$ROE, prob=0.5, na.rm=T))

predict(EQ1, newDF)
# This is a MEAN/EXPECTED value of Average Monthly Returns. Now we need a CI:

SignalsDF_1992$Size_New=SignalsDF_1992$Size-quantile(SignalsDF_1992$Size, prob=0.2, na.rm=T)
SignalsDF_1992$BTM_New=SignalsDF_1992$BTM-quantile(SignalsDF_1992$BTM, prob=0.8, na.rm=T)
SignalsDF_1992$ROE_New=SignalsDF_1992$ROE-quantile(SignalsDF_1992$ROE, prob=0.5, na.rm=T)
SignalsDF_1992$NSI_New=SignalsDF_1992$NSI-quantile(SignalsDF_1992$NSI, prob=0.2, na.rm=T)
SignalsDF_1992$NXF_New=SignalsDF_1992$NXF-quantile(SignalsDF_1992$NXF, prob=0.2, na.rm=T)

EQ1_New=lm(AvgMonRet~log(Size_New)+log(BTM_New)+ROE_New+NSI_New+NXF_New, 
           data = SignalsDF_1992)
SummaryEQ1_New=summary(EQ1_New)

coefci(EQ1_New, "(Intercept)", level = 0.95, alternative="two.sided")

#This is Interval estimate for predicted mean of AvgMonRet. Now we can do 
#the Interval estimate for the specific value:
SE_Spc=sqrt(vcov(EQ1_New)[1,1]+SummaryEQ1$sigma^2)
SE_Spc
EQ1_New$coefficients["(Intercept)"]-qt(p=0.975, df=1875)*SE_Spc
EQ1_New$coefficients["(Intercept)"]-qt(p=0.025, df=1875)*SE_Spc
#The interval estimate for the specific value is [-5.34683, 11.91045]
#This results are the same in Q3 with homoskedasticity, because OLS method
#point estimators are still unbiased.

#iii)
# We can use the F-Test with the linearhypothesis function
linearHypothesis(EQ1, c("ROE=0", "NSI=0", "NXF=0"),
                 vcov. =vcovHC0, test = "F" )
#We have a 5% significance level and p-value of 0.2, which is much higher.
#Hence, we accept H0 and conclude that b3,b4 and b5 are not significantly
#different from zero.With OLS SE's we had p-value of 0.2351.

#iv)
linearHypothesis(EQ1, "NSI-3*log(Size)=0",vcov. =vcovHC0)

#Again we have very high p-value of 0.84, hence we fail to reject the H0 and we
#conclude that  in the population of
#interest the partial effect of NSI on future returns is 3 times as big as the
#partial effect of log(Size). With OLS SE's we had p-value of 0.8506.

#v)
linearHypothesis(EQ1, c("ROE=0","NXF=0",  "NSI-3*log(Size)=0"),
                 vcov. =vcovHC0, test = "F" )
#I used the F-test. Again at the p-value of 0.9223 we fail to reject H0,
#and we conclude that statement is true. With OLS SE's we had p-value of 0.9733 

##Q6)
#i)
EQ1_CL=felm(AvgMonRet~log(Size)+log(BTM)+ROE+NSI+NXF |0|0| BBHL,
            data = SignalsDF_1992)
SummaryEQ1_CL=summary(EQ1_CL)
SummaryEQ1_CL$coefficients
#We found Cluster Robust Standard Errors and corresponding t and p values
#Such techniqiue should be used in financial data sets, because very often
#there is not only heteroskedasticity in the data, but also cross-correlation.
SummaryEQ1$coefficients
#The results are different from the results of Question 3. Cluster SE's are
#typically larger and t-values are smaller for most of estimates.

#ii)
vcovCluster=EQ1_CL$clustervcv

linearHypothesis(EQ1_CL, c("ROE=0", "NSI=0", "NXF=0"),
                 vcov. =vcovCluster, test = "F" )
#We generated var-covar matrix with cluster-robust estimates and used that
#matrix in the F-test. As a result, we have a p-value of 0.2047. Hence, 
#we can not reject the H0.

#Part B
##Q1)
EQ2=lm(AvgMonRet~log(Size)+log(BTM)+ROE+NSI+NXF, data = Signals)
SummaryEQ2=summary(EQ2)
#i)
SummaryEQ2$coefficients
#A 1% increase in Size decreases the Average Returns by 0.0025473.
#A 1% increase in BTM increases the Average Returns by 0.00317.
#A 1 unit increase in ROE decreases the Average Returns by 0.00464.
#A 1 unit increase in NSI decreases the Average Returns by 0.267.
#A 1 unit increase in NXF decreases the Average Returns by 0.00076,

#ii)
SummaryEQ2$coefficients
#ROE has large standard error, and hence we can not reject
#the null hypothesis that b3 is equal to zero. Other estimates have lower SE's
#hence they are significantly different from zero.

#iii)
SummaryEQ2$coefficients
#Coefficients with large SE's have smaller t-values. Hence, ROE has
#smaller t-value of 1.233. Size and BTM have very high t-values.

#iv)
SummaryEQ2$sigma
# The residual standard error is calculated as sqrt(SSR/n-p-1), or it is
# the square root of the Mean Squared Error.

#v)
SummaryEQ2$r.squared
SummaryEQ2$adj.r.squared

#R^2 and adjusted R^2 are measurements of Goodness-Of-Fit for the MLR.
#R^2=SSE/SST, while Ra^2=1-(n-1)*(1-R^2)/(n-p-1)

#vi)
SummaryEQ2$fstatistic
#F-statistic checks if the MLR model's coefficients are significant overall.
#The formula is ((SSRr-SSRur)/q)/(SSRur/n-p-1). Our model has high F-stat and
#hence we reject the null hypothesis that all coefficients are zero.

##Q2)
SummaryEQ1
SummaryEQ2
#Previously, both NXF and ROE were insignificant, in the second model only
#ROE is statistically insignificant. In both models estimates have the same
#signs and hence the same effects. Also, estimates in the second model
#are smaller than in the first model. Second model has lower R^2 and adjusted 
#R^2 compared to the first model. 

##Q3)
#i)
EQ2_CL_Perm=felm(AvgMonRet~log(Size)+log(BTM)+ROE+NSI+NXF |0|0| PermNo,
            data = Signals)
SummaryEQ2_CL_Perm=summary(EQ2_CL_Perm)
SummaryEQ2_CL_Perm$coefficients
SummaryEQ2$coefficients
# We can see taht Cluster SE's are different from SE's of original model.
#Most of SE's have increased, except the SE of NXF, which has decreased, meaning
#that it became more statistically significant.
#ii)
EQ2_CL_PForm=felm(AvgMonRet~log(Size)+log(BTM)+ROE+NSI+NXF |0|0| PFormDate,
            data = Signals)
SummaryEQ2_CL_PForm=summary(EQ2_CL_PForm)
SummaryEQ2_CL_PForm$coefficients
SummaryEQ2$coefficients
#This time Cluster SE's are substantially higher. As a result, both NSI and ROE
#became statistically insignificant with t-values of 0.74 and 1.49
#iii)
EQ2_CL_TwoWay=felm(AvgMonRet~log(Size)+log(BTM)+ROE+NSI+NXF |0|0| PFormDate
                   +PermNo,
                  data = Signals)
SummaryEQ2_CL_TwoWay=summary(EQ2_CL_TwoWay)
SummaryEQ2_CL_TwoWay$coefficients
SummaryEQ2$coefficients
#Once again, SE's are larger and t-values are smaller, especially for ROE and NSI.

##Q4)
EQ2_CL_TwoWay_Fixed=felm(AvgMonRet~log(Size)+log(BTM)+ROE+NSI+NXF|PFormDate+PermNo|0|PFormDate+
                           PermNo, data = Signals)
SummaryEQ2_CL_TwoWay_Fixed=summary(EQ2_CL_TwoWay_Fixed)
SummaryEQ2_CL_TwoWay_Fixed$coefficients
SummaryEQ2$coefficients

#We have added fixed effects for portfolio formation date and each firm.
#Because of that we expxerienced very different SE's, which are much larger
#than SE's that we have computed in Question 1 of Part B.
#Now, we can see that both ROE, NSI and NXF have large SE's and all three
#estimates are not significantly different from zero. In Question 1 of Part B,
#only ROE was insignificant.