options(digits = 3)
library(GRS.test)
library(readxl)
library(stargazer)
library(moments)

#Exercise 2: FF and 6 Industry Portfolios:
Data <- read_excel("AE_6_FF.xlsx")

print(Data[c(1:3, nrow(Data)), ])

Fit_Agric=lm(Agric~`Mkt-RF`+SMB+HML, data = Data)
Fit_Food=lm(Food~`Mkt-RF`+SMB+HML, data = Data)
Fit_Soda=lm(Soda~`Mkt-RF`+SMB+HML, data = Data)
Fit_Beer=lm(Beer~`Mkt-RF`+SMB+HML, data = Data)
Fit_Smoke=lm(Smoke~`Mkt-RF`+SMB+HML, data = Data)
Fit_Toys=lm(Toys~`Mkt-RF`+SMB+HML, data = Data)

stargazer(Fit_Agric, Fit_Food, Fit_Soda, Fit_Beer, Fit_Smoke, Fit_Toys,
          title="Regression Results", align=TRUE,type="html", 
          out = "FF_6_Results.html")

FF_Kurtosis=data.frame(kurtosis(Fit_Agric$residuals),
                       kurtosis(Fit_Food$residuals),
                       kurtosis(Fit_Soda$residuals),
                       kurtosis(Fit_Beer$residuals),
                       kurtosis(Fit_Smoke$residuals),
                       kurtosis(Fit_Toys$residuals))


colnames(FF_Kurtosis)=colnames(Data[, 5:10])
FF_Kurtosis


#### A little bit of capm ####
Fit_Agric_CAPM=lm(Agric~`Mkt-RF`, data = Data)
Fit_Food_CAPM=lm(Food~`Mkt-RF`, data = Data)
Fit_Soda_CAPM=lm(Soda~`Mkt-RF`, data = Data)
Fit_Beer_CAPM=lm(Beer~`Mkt-RF`, data = Data)
Fit_Smoke_CAPM=lm(Smoke~`Mkt-RF`, data = Data)
Fit_Toys_CAPM=lm(Toys~`Mkt-RF`, data = Data)

stargazer(Fit_Agric_CAPM, Fit_Food_CAPM, Fit_Soda_CAPM, 
          Fit_Beer_CAPM, Fit_Smoke_CAPM, Fit_Toys_CAPM,
          title="Regression Results", align=TRUE,type="html", 
          out = "FF_6_Results_CAPM.html")

kurtosis(Fit_Agric_CAPM$residuals)
kurtosis(Fit_Food_CAPM$residuals)
kurtosis(Fit_Soda_CAPM$residuals)
kurtosis(Fit_Beer_CAPM$residuals)
kurtosis(Fit_Smoke_CAPM$residuals)
kurtosis(Fit_Toys_CAPM$residuals)

######### Extended Problem ################
Data2 <- read_excel("AE_25_FF.xlsx")

print(Data2[c(1:3, nrow(Data2)), ])

#1) 
Means=data.frame(colMeans(Data2[, 5:29]))
Means

#2) 
TwentyFive = colnames(Data2[,5:29])

results <- list()

for (i in seq_along(TwentyFive)) {
  eq <- paste(TwentyFive[i],"~ MktxRF")
  fit <- lm(as.formula(eq), data= Data2)
  results[[i]]=fit
}      

stargazer(results,
          title="Regression Results", align=TRUE,type="html", 
          out = "FF_25_Results_CAPM.html")

####### Some GRS Tests #########
Returns_Matrix=Data2[, 5:29]

Mkt_Rf=Data2[, 2]

CAPM_GRS=GRS.test(Returns_Matrix,Mkt_Rf)

c(CAPM_GRS$GRS.stat, CAPM_GRS$GRS.pval)

N=25
T=nrow(Data2)
Critical_Values=data.frame(qf(0.99, (T-N-1), N), qf(0.95, (T-N-1), N),
qf(0.90, (T-N-1), N))
colnames(Critical_Values)=c("99%", "95%", "90%")

Critical_Values

Capm_Alphas=CAPM_GRS$coef[,1]
Capm_Alphas_Means=mean(Capm_Alphas)
Capm_Alphas_Means

Capm_Alphas_RMSE=sqrt((t(Capm_Alphas)%*%Capm_Alphas)/25)
Capm_Alphas_RMSE

#####FF###########################

FF_3_Factors=Data2[, 2:4]

FF_3_GRS=GRS.test(Returns_Matrix, FF_3_Factors)

c(FF_3_GRS$GRS.stat, FF_3_GRS$GRS.pval)

FF_Alphas=FF_3_GRS$coef[,1]
FF_Alphas_Means=mean(FF_Alphas)
FF_Alphas_Means

FF_Alphas_RMSE=sqrt((t(FF_Alphas)%*%FF_Alphas)/25)
FF_Alphas_RMSE

#3.2
Results=data.frame(Capm_Alphas_Means, Capm_Alphas_RMSE,
              FF_Alphas_Means, FF_Alphas_RMSE)
Results

#3.3

Expected=list()

for (i in seq_along(TwentyFive)) {
  Plot_Means=mean(results[[i]]$fitted.values)
  Expected[[i]]=Plot_Means
}      

Expected=as.numeric(Expected)
Actual=as.numeric(Means$colMeans.Data2...5.29..)

plot(Actual, Expected,
     xlab = "Actual" , ylab = "Predicted", 
     main = "Predicted vs Actual Plot")
abline(a=0, b=1)


