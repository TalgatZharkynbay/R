###### BOOTSTRAP EXAMPLE
#We first create a simple function, boot.fn(), which takes in the Auto data
#set as well as a set of indices for the observations, and returns the 
#intercept and slope estimates for the linear regression model. 
#We then apply this function to the full set of 392 observations in order to 
#compute the estimates of b0 and b1 on the entire data set using the usual 
#linear regression coefficient estimates:
require(boot)

boot.fn= function (data ,index)
  +return (coef(lm(mpg~horsepower , data=data , subset = index )))

boot.fn(Auto ,1:392)

#The boot.fn() function can also be used in order to create bootstrap 
#estimates for the intercept and slope terms by randomly sampling from among
#the observations with replacement. Here we give two examples:
set.seed(1)
boot.fn(Auto ,sample (392 ,392 , replace =T))
boot.fn(Auto ,sample (392 ,392 , replace =T))

#Next, we use the boot() function to compute the standard errors of 1,000
#bootstrap estimates for the intercept and slope terms:
boot(Auto ,boot.fn ,10000)

#This indicates that the bootstrap estimate for SE(βˆ0) is 0.86, and that
#the bootstrap estimate for SE(βˆ1) is 0.0074. As discussed in Section 3.1.2,
#standard formulas can be used to compute the standard errors for the
#regression coefficients in a linear model. These can be obtained using the
#summary() function:

summary(lm(mpg~horsepower ,data =Auto))$coef
#The standard error estimates for βˆ0 and βˆ1 obtained using the formulas
#from Section 3.1.2 are 0.717 for the intercept and 0.0064 for the slope.
#Interestingly, these are somewhat different from the estimates obtained
#using the bootstrap. Does this indicate a problem with the bootstrap? In
#fact, it suggests the opposite.

#Below we compute the bootstrap standard error estimates and the standard 
#linear regression estimates that result from fitting the quadratic model
#to the data. Since this model provides a good fit to the data (Figure 3.8),
#there is now a better correspondence between the bootstrap estimates and
#the standard estimates of SE(βˆ0), SE(βˆ1) and SE(βˆ2):
boot.fn= function (data ,index )
  + coefficients(lm(mpg~horsepower +I( horsepower ^2) ,data=data ,
                    subset = index ))
set.seed(1)

boot(Auto,boot.fn ,10000)

summary(lm(mpg~horsepower +I( horsepower ^2) ,data= Auto))$coef 
