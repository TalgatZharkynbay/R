options(scipen = 10, digits = 3)
library(writexl)
library(lubridate)
library(tidyverse)
library(quantmod)
library(highcharter)
library(tidyquant)
library(readxl)
library(readr)
library(data.table)
library(ggplot2)
###########################################################################
LogReturns <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns.xlsx")
Fama_French=read.csv("C:/Users/talga/Desktop/Thesis/Fama_French.csv")
Fama_French[2:7]=Fama_French[2:7]/100
LogReturns=xts(LogReturns[,2:ncol(LogReturns)], order.by = LogReturns$index)
Fama_French$X=ymd(Fama_French$X)
Fama_French=xts(Fama_French[,2:ncol(Fama_French)], order.by = Fama_French$X)
LogReturns=merge(Fama_French, LogReturns)
LogReturns=LogReturns["2012-03-22/"]
LogReturns[,7:ncol(LogReturns)]=LogReturns[,7:ncol(LogReturns)]-LogReturns$RF

Test=na.locf(LogReturns)
Test=Test[ , colSums(is.na(Test)) == 0]
A=as.matrix(Test[,7:ncol(Test)])
B=as.matrix(Test[,6])
Riskadjusted <- apply(A, 2, `-`, B)
Riskadjusted=as.data.frame(Riskadjusted)
Riskadjusted$Date=index(LogReturns)
write_xlsx(Riskadjusted, "C:/Users/talga/Desktop/LogReturns_Cleaner.xlsx")
Riskadjusted$Date=ymd(Riskadjusted$Date)
Riskadjusted=xts(Riskadjusted[, -ncol(Riskadjusted)], order.by = Riskadjusted$Date)
Riskadjusted=merge(Riskadjusted, Fama_French)
Riskadjusted=na.omit(Riskadjusted)
#######################################################################################

Stonks = colnames(Riskadjusted[, -c(1908:1913)])
MarketBeta <- list()
SMBLoading <- list()
HMLLoading <- list()

for (i in seq_along(Stonks)) {
  eq <- paste(Stonks[i],"~ Mkt.RF+SMB+HML")
  fit <- lm(as.formula(eq), data= Riskadjusted)
  MarketBeta[i]=fit$coefficients[2]
  SMBLoading[i]=fit$coefficients[3]
  HMLLoading[i]=fit$coefficients[4]
}    

MarketBeta=data.table(unlist(MarketBeta))
SMBLoading=data.table(unlist(SMBLoading))
HMLLoading=data.table(unlist(HMLLoading))
Means=data.table(colMeans(Riskadjusted[, -c(1908:1913)]))
SDs=data.table(apply(Riskadjusted[, -c(1908:1913)], 2, sd.annualized))
########################################################################################
ggplot(MarketBeta, aes(x = MarketBeta$V1)) +  
  geom_density(fill = "mediumseagreen", alpha = 0.25) +
  stat_function(fun = function(x) dnorm(x, mean = mean(MarketBeta$V1), 
                                        sd = sd(MarketBeta$V1)),
                color = "red", linetype = "dotted", size = 1)+xlab("Market Betas")

ggplot(SMBLoading, aes(x = SMBLoading$V1)) +  
  geom_density(fill = "mediumseagreen", alpha = 0.25) +
  stat_function(fun = function(x) dnorm(x, mean = mean(SMBLoading$V1), 
                                        sd = sd(SMBLoading$V1)),
                color = "red", linetype = "dotted", size = 1)+xlab("SML loadings")

ggplot(HMLLoading, aes(x = HMLLoading$V1)) +  
  geom_density(fill = "mediumseagreen", alpha = 0.25) +
  stat_function(fun = function(x) dnorm(x, mean = mean(HMLLoading$V1), 
                                        sd = sd(HMLLoading$V1)),
                color = "red", linetype = "dotted", size = 1)+xlab("HML loadings") 

ggplot(Means, aes(x = Means$V1)) +  
  geom_density(fill = "mediumseagreen", alpha = 0.1) #+
  #stat_function(fun = function(x) dnorm(x, mean = mean(Means$V1), 
                                        #sd = sd(Means$V1)),
                #color = "red", linetype = "dotted", size = 1)+xlab("Risk-adjusted means")

ggplot(SDs, aes(x = SDs$V1)) +  
  geom_density(fill = "mediumseagreen", alpha = 0.1) 


