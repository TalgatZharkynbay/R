library(boot)
library(mosaic)

Data <- read_excel("AE_6_FF.xlsx")
Fit_Agric=lm(Agric~`Mkt-RF`+SMB+HML, data = Data)
summary(Fit_Agric)
confint(Fit_Agric)
#################################Residual bootsrapping###########################
r = residuals(Fit_Agric)
dim = dim(Data)
T = dim[1]
beta.boot = data.frame(matrix(0,1000,4))
colnames(beta.boot) = names(Fit_Agric$coefficients)

for(i in 1:1000){
  rsample = sample(1:T,T,replace = TRUE)
  
  Data$Agric_fit = Data$Agric+r[rsample]
  
  results = lm(Agric_fit ~ `Mkt-RF` + HML + SMB, data = Data)
  
  beta.boot[i,] = summary(results)$coefficients[1:4] #Extracts the coefficients
}

#Confidence intervals for each coefficient:
coef_names = colnames(beta.boot)

for (i in 1:4){
  print(coef_names[i])
  print(mosaic::qdata(beta.boot[,i],c(0.025, 0.975)))
}

########################Cases BootStrapping ###########################

dim = dim(Data)
T = dim[1]
beta.boot = data.frame(matrix(0,1000,4))
colnames(beta.boot) = names(Fit_Agric$coefficients)

for(i in 1:1000){
  rsample = sample(1:T,T,replace = TRUE)
  
  results = lm(Agric ~ `Mkt-RF` + HML + SMB, data = Data[rsample,])
  
  beta.boot[i,] = summary(results)$coefficients[1:4] #Extracts the coefficients
}

#Confidence intervals for each coefficient:
coef_names = colnames(beta.boot)

for (i in 1:4){
  print(coef_names[i])
  print(mosaic::qdata(beta.boot[,i],c(0.025, 0.975)))
}