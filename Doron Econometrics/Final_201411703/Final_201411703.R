## Talgat Zharkynbay
## "Final Exam"

### Load packages

```{r load-packages, message = FALSE}
require(boot)
require(stargazer)
require(lmtest)
require(moments)
require(tseries)
require(sandwich)
require(car)
require(ISLR)
library(dplyr)
```
### Q1
Equation 1:
  mpg=b0+b1*cylinders+b2*displacement+b3*horsepower+b4*weight+U

Equation 2, taking into account the ideas of Prof. Israeli:
  
  mpg=b0+b1*cylinders + b2*displacement + b3*displacement*cylinders + b4*horsepower + b5*horsepower^2 + log(weight)

We added the interaction term between displacement and cylinders, since the relationship between mpg and displacement depends on the number of cylinders. Next, we added horsepower^2 to the model, since Prof. Israeli told that horsepower has quadratic relationship with mpg. Last, but not least, we changed the weight variable to log(weight), because we wanted to capture the diminishing marginal effect of weight, i.e. Level-Log relationship.

### Q2
```{r, results='asis'}

Fit_EQ1 = lm(mpg~cylinders+displacement+horsepower+weight, data = Auto)

Summary_Fit_EQ1=summary(Fit_EQ1)

Fit_EQ2 = lm(mpg~cylinders+displacement+displacement:cylinders+horsepower+ I(horsepower^2)+log(weight), data = Auto)

Summary_Fit_EQ2=summary(Fit_EQ2)

stargazer(Fit_EQ1, Fit_EQ2,title="Equation 1 vs Equation 2", align=TRUE,type="html")
```
Coefficient estimates on Equation 1:
  
  Beta0 coefficient or Constant is equal to 45.757. It shows that in case of all explanatory variables are equal to 0, the expected mean value of mpg will be 45.757.

With regards to the coefficient estimate for cylinders, regression output shows the value of -0.393. Hence, AEBE, on average,(a unit increase in the number of cylinders is associated with 0.393 units of decrease in mpg). Hence, cars with more cylinders waste more petrol.

The coefficient estimate for displacement: regression output shows the value of 0.0001. Hence, AEBE, on average,(a unit increase in displacement is associated with 0.0001 units of increase in mpg).

The coefficient estimate for horsepower: regression output shows the value of -0.043. Hence, AEBE, on average,(a unit increase in horsepower is associated with 0.043 units of decrease in mpg).

The coefficient estimate for weight: regression output shows the value of -0.005. Hence, AEBE, on average,(a unit increase in weight is associated with 0.005 units of decrease in mpg).

Coefficient estimates on Equation 2:
  
  Beta0 coefficient or Constant is equal to 125.030. It shows that in case of all explanatory variables are equal to 0, the expected mean value of mpg will be 125.030.

With regards to the coefficient estimate of b1 for cylinders, regression output shows the value of -1.188. Hence, AEBE, on average,(a unit increase in the number of cylinders is associated with 1.188 units of decrease in mpg, when displacement is equal to zero!). Otherwise, when displacement is not equal to zero: the estimated effect is -1.188+0.008*displacement.

The coefficient estimate of b2 for displacement: regression output shows the value of -0.063. Hence, AEBE, on average,(a unit increase in displacement is associated with 0.063 units of decrease in mpg, when cylinders are equal to zero!). Otherwise, when cylinders are not equal to zero: the estimated effect is -0.063+0.008*cylinders.

The coefficient estimate for horsepower: regression output shows the value of -0.226 for b4 and the value of 0.001 for b5. Hence, AEBE, on average,(a unit increase in horsepower is associated with -0.226+2*0.001*horsepower change in mpg. Hence, the effect on mpg is horsepower dependent. This is very different from Equation 1 model. 
                                                                                                                                                    
                                                                                                                                                    The coefficient estimate for log(weight): regression output shows the value of -9.529. Hence, AEBE, on average,(a unit increase in log(weight) is associated with 9.529 units of decrease in mpg) OR (a 1% increase in weight is associated with approx. 0.0953 units of decrease in mpg).
                                                                                                                                                    
                                                                                                                                                    ### Q3
                                                                                                                                                    Taking into account our interpretations for interactive variables in Question 2, we now can plug-in interesting values to estimate the effects of one explanatory variable on mpg:
                                                                                                                                                      For Equation 1:
                                                                                                                                                      ```{r}
                                                                                                                                                    #The partial effects are going to be the same that we have mentioned when we interpreted coefficient estimates in Question 2:
                                                                                                                                                    #The coefficient estimate for displacement: regression output shows the value of 0.0001. Hence, AEBE, on average,(a unit increase in displacement is associated with 0.0001 units of increase in mpg).
                                                                                                                                                    
                                                                                                                                                    #The coefficient estimate for horsepower: regression output shows the value of -0.043. Hence, AEBE, on average,(a unit increase in horsepower is associated with 0.043 units of decrease in mpg).
                                                                                                                                                    
                                                                                                                                                    #The coefficient estimate for weight: regression output shows the value of -0.005. Hence, AEBE, on average,(a unit increase in weight is associated with 0.005 units of decrease in mpg).
                                                                                                                                                    ```
                                                                                                                                                    
                                                                                                                                                    For Equation 2:
                                                                                                                                                      ```{r}
                                                                                                                                                    #The estimated partial effect on mpg of displacement is -0.063+0.008*cylinders:
                                                                                                                                                    -0.063+0.008*mean(Auto$cylinders)
                                                                                                                                                    
                                                                                                                                                    #The estimated partial effect on mpg of horsepower is -0.226+2*0.001*horsepower:
                                                                                                                                                    -0.226+2*0.001*mean(Auto$horsepower)
                                                                                                                                                    
                                                                                                                                                    #The estimated partial effect on mpg of weight is the same that we have mentioned in Question 2:
                                                                                                                                                    #AEBE, on average,(a unit increase in log(weight) is associated with 9.529 units of decrease in mpg) OR (a 1% increase in weight is associated with approx. 0.0953 units of decrease in mpg).
                                                                                                                                                    ```
                                                                                                                                                    
                                                                                                                                                    ### Q4
                                                                                                                                                    The Adjusted R-squared which is a measure that is used in order to make a comparison between regression models with different numbers of independent variables where the dependent variables are identical:
                                                                                                                                                      ```{r}
                                                                                                                                                    Summary_Fit_EQ1$adj.r.squared
                                                                                                                                                    Summary_Fit_EQ2$adj.r.squared
                                                                                                                                                    #Equation 2 has higher Adjusted R^2, and hence it better explains variarion in car's fuel economy.
                                                                                                                                                    ```
                                                                                                                                                    
                                                                                                                                                    ### Q5
                                                                                                                                                    Since we use classical approach, we do not need to use robust standard errors for hypothesis testing. 
                                                                                                                                                    ```{r}
                                                                                                                                                    # First, let's create separate dataframes based on origins:
                                                                                                                                                    American=Auto%>%
                                                                                                                                                      filter(origin==1)
                                                                                                                                                    European=Auto%>%
                                                                                                                                                      filter(origin==2)
                                                                                                                                                    Japanese=Auto%>%
                                                                                                                                                      filter(origin==3)
                                                                                                                                                    #Now, we can do the F-tests for 3 separate data frames, with 3 different lm models:
                                                                                                                                                    Fit_American = lm(mpg~cylinders+displacement+displacement:cylinders+horsepower+ I(horsepower^2)+log(weight), data = American)
                                                                                                                                                    
                                                                                                                                                    Fit_European = lm(mpg~cylinders+displacement+displacement:cylinders+horsepower+ I(horsepower^2)+log(weight), data = European)
                                                                                                                                                    
                                                                                                                                                    Fit_Japanese = lm(mpg~cylinders+displacement+displacement:cylinders+horsepower+ I(horsepower^2)+log(weight), data = Japanese)
                                                                                                                                                    
                                                                                                                                                    #The F-tests for weight:
                                                                                                                                                    linearHypothesis(Fit_American, "log(weight)=-10", test = "F")
                                                                                                                                                    #With the p-value of 0.65, we can not reject the null hypothesis that a 1% increase in weight reduces the average miles per gallon by 0.1.
                                                                                                                                                    linearHypothesis(Fit_European, "log(weight)=-10", test = "F")
                                                                                                                                                    #With the p-value of 0.55, we can not reject the null hypothesis that a 1% increase in weight reduces the average miles per gallon by 0.1.
                                                                                                                                                    linearHypothesis(Fit_Japanese, "log(weight)=-10", test = "F")
                                                                                                                                                    #With the p-value of 0.49, we can not reject the null hypothesis that a 1% increase in weight reduces the average miles per gallon by 0.1.
                                                                                                                                                    
                                                                                                                                                    #The F-Tests for displacement:
                                                                                                                                                    linearHypothesis(Fit_American, "displacement+4*cylinders:displacement=-0.0192", test="F")
                                                                                                                                                    
                                                                                                                                                    #With the p-value of 0.045, we can reject the null hypothesis at 5% significance that the effect of a one unit increase in ????displacement on the average miles per gallon for a car with 4 ??cylinders is equal to -0.0192 for American cars.
                                                                                                                                                    
                                                                                                                                                    linearHypothesis(Fit_European, "displacement+4*cylinders:displacement=-0.0192", test="F")
                                                                                                                                                    
                                                                                                                                                    #With the p-value of 0.74, we can accept the null hypothesis at 5% significance that the effect of a one unit increase in ????displacement on the average miles per gallon for a car with 4 ??cylinders is equal to -0.0192 for European cars.
                                                                                                                                                    
                                                                                                                                                    linearHypothesis(Fit_Japanese, "displacement+4*cylinders:displacement=-0.0192", test="F")
                                                                                                                                                    
                                                                                                                                                    #With the p-value of 0.21, we can accept the null hypothesis at 5% significance that the effect of a one unit increase in ????displacement on the average miles per gallon for a car with 4 ??cylinders is equal to -0.0192 for Japanese cars.
                                                                                                                                                    ```
                                                                                                                                                    
                                                                                                                                                    ### Q6
                                                                                                                                                    ```{r}
                                                                                                                                                    #Let's first do the non-formal tests for skewness and kurtosis to check normality:
                                                                                                                                                    round(skewness(Fit_EQ2$residuals), digits = 3)
                                                                                                                                                    #The skewness is positive and equal to 0.548, which means that the distribution is not-normal and actually skewed to the right.
                                                                                                                                                    round(kurtosis(Fit_EQ2$residuals), digits = 3)
                                                                                                                                                    #The kurtosis is more than 3 and equal to 4.72. Hence, the curve is more peaked than a normal distribution curve and has thinner tails. Hence, the distribution is not normal.
                                                                                                                                                    
                                                                                                                                                    #Now, let's do the formal Jarque-Bera test for normality:
                                                                                                                                                    jarque.bera.test(Fit_EQ2$residuals)
                                                                                                                                                    #We have very low p-value and hence we reject the null hypothesis of normality.
                                                                                                                                                    qqnorm(Fit_EQ2$residuals, col = "red", ylab = "Residual Quantiles")
                                                                                                                                                    #The normal qq-plot demonstrates the right skewness trend too.
                                                                                                                                                    
                                                                                                                                                    #Since both formal and non-formal tests presented evidence against normality of the data, our inferences about the coefficient estimates in part 5 are biased. To conduct proper tests about the coefficient estimates, we would need to use heteroskedasticity robust standard errors using vcov.= function from the sandwich package.
                                                                                                                                                    
                                                                                                                                                    #BootStrap bonus Question:
                                                                                                                                                    boot.fn= function (data ,index )
                                                                                                                                                      + coefficients(lm(mpg~cylinders+displacement+displacement:cylinders+horsepower+ I(horsepower^2)+log(weight),data=data,
                                                                                                                                                                        subset = index ))
                                                                                                                                                    set.seed(1)
                                                                                                                                                    
                                                                                                                                                    BootStrapErrors=boot(Auto,boot.fn ,10000)
                                                                                                                                                    BootStrapErrors
                                                                                                                                                    #The bootstrapped SE for log(weight) is 2.272226.
                                                                                                                                                    
                                                                                                                                                    CI_95_se_method=c(-9.529138+qnorm(p=0.025, mean=0, sd=1)*2.272226,
                                                                                                                                                                      -9.529138-qnorm(p=0.025, mean=0, sd=1)*2.272226)
                                                                                                                                                    
                                                                                                                                                    CI_95_se_method
                                                                                                                                                    
                                                                                                                                                    #Now, we divide these values by 100 to estimate the changes from 1% increase in weight, not log(weight):
                                                                                                                                                    
                                                                                                                                                    CI_95_se_method=CI_95_se_method/100
                                                                                                                                                    CI_95_se_method.
                                                                                                                                                    
                                                                                                                                                    #Hence, AEBE, on average,(a unit increase in log(weight) is associated with between 13.98 and 5.08 units of decrease in mpg) OR (a 1% increase in weight is associated with approx. between 0.1398 and 0.0508 units of decrease in mpg).
                                                                                                                                                    
                                                                                                                                                    
                                                                                                                                                    ```
                                                                                                                                                    
                                                                                                                                                    ### Q7
                                                                                                                                                    ```{r}
                                                                                                                                                    #Creating the dummy variable:
                                                                                                                                                    Auto$mpg_b = ifelse(Auto$mpg >= quantile(Auto$mpg, prob = 0.75, na.rm = TRUE), 1,0)
                                                                                                                                                    #Estimating EQ1 with new dependent variable:
                                                                                                                                                    Fit_LPM = lm(mpg_b ~ cylinders + displacement + horsepower + weight, data = Auto)
                                                                                                                                                    
                                                                                                                                                    summaryFit_LPM = summary(Fit_LPM)
                                                                                                                                                    summaryFit_LPM
                                                                                                                                                    #Interpreting coefficients. This is the LPM MLR Model, because the dependent variable is a binary variable that takes values of either 0 or 1. All of our explanatory variables are continuous:
                                                                                                                                                    
                                                                                                                                                    #The coefficient on cylinders is equal to 0.0516 and statistically insignificant. It indicates that when car’s number of cylinders increases by 1, the estimated change in the probability that mpg_b=1, or the car being in the top 25% of the sample distribution of mpg, increases by 0.0516.
                                                                                                                                                    
                                                                                                                                                    #The coefficient on displacement is equal to 0.00000584 and statistically insignificant. It indicates that when car’s displacement increases by 1, the estimated change in the probability that mpg_b=1, or the car being in the top 25% of the sample distribution of mpg, increases by 0.00000584.
                                                                                                                                                    
                                                                                                                                                    #The coefficient on horsepower is equal to -0.00161210 and statistically insignificant. It indicates that when car’s horsepower increases by 1, the estimated change in the probability that mpg_b=1, or the car being in the top 25% of the sample distribution of mpg, decreases by 0.00161210.
                                                                                                                                                    
                                                                                                                                                    #The coefficient on weight is equal to -0.00033208 and statistically significant. It indicates that when car’s horsepower increases by 1, the estimated change in the probability that mpg_b=1, or the car being in the top 25% of the sample distribution of mpg, decreases by 0.00033208.
                                                                                                                                                    ```
                                                                                                                                                    
                                                                                                                                                    ### Q8
                                                                                                                                                    ```{r}
                                                                                                                                                    #Professor Israeli said that it does not make sense to use OLS, because under LPM model the values can become greater than 1 or less than 0. Since, we are estimating the probability, it does not make sense to have probabilities more than 1 and less than zero. Instead of OLS, it would be right to use Maximum Likelihood Estimators that always control the probability to be between 0 and 1, because of the G() function (Logit or Probit models). 
                                                                                                                                                    
                                                                                                                                                    #Fitting the Logit model:
                                                                                                                                                    Fit_Logit = glm(mpg_b ~ cylinders + displacement + horsepower + weight, data = Auto, family = binomial(link = 'logit'))
                                                                                                                                                    summary(Fit_Logit)
                                                                                                                                                    
                                                                                                                                                    #The cylinders coefficient is equal to  0.61696 and statistically insignificant. It indicates that when car’s number of cylinders increases by 1, the odds of mpg_b=1 will increase by exactly 100*(exp(0.61696)-1)=85.3%.
                                                                                                                                                    
                                                                                                                                                    #The displacement coefficient is equal to  -0.01849 and statistically insignificant. It indicates that when car’s displacement increases by 1, the odds of mpg_b=1 will decrease by exactly 100*(exp(-0.01849)-1)=1.83%.
                                                                                                                                                    
                                                                                                                                                    #The horsepower coefficient is equal to  -0.07641 and statistically significant. It indicates that when car’s horsepower increases by 1, the odds of mpg_b=1 will decrease by exactly 100*(exp(-0.07641)-1)=7.36%.
                                                                                                                                                    
                                                                                                                                                    #The weight coefficient is equal to  -0.00145 and statistically insignificant. It indicates that when car’s weight increases by 1, the odds of mpg_b=1 will decrease by exactly 100*(exp(-0.00145)-1)=0.145%.
                                                                                                                                                    
                                                                                                                                                    #Estimating the Average Partial Effect (APE)
                                                                                                                                                    ScaleFactor_Logit = mean(dlogis(predict(Fit_Logit),location = 0,scale = 1))
                                                                                                                                                    ScaleFactor_Logit
                                                                                                                                                    APE_Logit_Weight = ScaleFactor_Logit * Fit_Logit$coefficients["weight"]
                                                                                                                                                    APE_Logit_Weight
                                                                                                                                                    #AEBE, on average, a unit increase in cars weight  decreases the probability that a car is in the top 25% of the sample distribution of mpg by -0.000124.
                                                                                                                                                    ```
                                                                                                                                                    
                                                                                                                                                    