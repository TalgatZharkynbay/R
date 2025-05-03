options(scipen = 10, digits = 3)
library(portsort)
library(PerformanceAnalytics)
library(xts)
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
library(StatMeasures)
library(lmtest)
library(sandwich)
##################### ALL SORTS ################################
Monthly <-read_excel("C:/Users/talga/Desktop/Thesis/CRSP_Monthly_Clean.xlsx")
Monthly=xts(Monthly[, -1], order.by = Monthly$date)

momentum_signal=rollapplyr(Monthly, width=12 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)
momentum_signal=xts::lag.xts(momentum_signal, 1)#1-month skip
momentum_signal=na.omit(momentum_signal)

VRY <- read_excel("C:/Users/talga/Desktop/Thesis/RET_VRY_CLEAN.xlsx")
VRY=xts(VRY[,-1], order.by = VRY$date)
VRY=apply.monthly(VRY, matrixStats::colSds)
VRY=rollapplyr(VRY, width=3 , mean, partial = TRUE)
VRY=xts::lag.xts(VRY, 1)
VRY=na.omit(VRY)

VOL=read_excel("C:/Users/talga/Desktop/Thesis/VOL_CLEAN.xlsx")
VOL=xts(VOL[,-1], order.by = VOL$date)
VOL=apply.monthly(VOL, colMeans)
VOL=rollapplyr(VOL, width=3 , mean, partial = TRUE)
VOL=xts::lag.xts(VOL, 1)
VOL=na.omit(VOL)

VRY=VRY[ , order(colnames(VRY))]
VOL=VOL[ , order(colnames(VOL))]
Monthly=Monthly[ , order(colnames(Monthly))]
momentum_signal=momentum_signal[ , order(colnames(momentum_signal))]

# time(VOL)<- time(VOL) %>% as.yearmon() %>% as.Date()#questionable
# time(VRY)<- time(VRY) %>% as.yearmon() %>% as.Date()#questionable

VOL=VOL[, colnames(VOL) %in% colnames(Monthly)]
VRY=VRY[, colnames(VRY) %in% colnames(Monthly)]
VOL=VOL[, colnames(VOL) %in% colnames(VRY)]
VRY=VRY[, colnames(VRY) %in% colnames(VOL)]
Monthly=Monthly[, colnames(Monthly) %in% colnames(VOL)]
momentum_signal=momentum_signal[, colnames(momentum_signal) %in% colnames(VOL)]

VOL=VOL[paste0(time(momentum_signal)[1],"/")]
VRY=VRY[paste0(time(momentum_signal)[1],"/")]
Monthly=Monthly[paste0(time(momentum_signal)[1],"/")]

dimA = 0:10/10
dimB =0:3/3

Fa = momentum_signal; Fb = VOL; Fc = NULL
VOL.output = conditional.sort(Fa=Fa, Fb=Fb,
                                   R.Forward=Monthly,dimA=dimA, dimB = dimB)

Fa = momentum_signal; Fb = VRY; Fc = NULL
VRY.output = conditional.sort(Fa=Fa, Fb=Fb,
                                   R.Forward=Monthly,dimA=dimA, dimB = dimB)

# dimC=0:3/3;dimA=0:5/5
# Fa = momentum_signal; Fb = VOL; Fc = VRY
# Test = conditional.sort(Fa=Fa, Fb=Fb, Fc=Fc,
#                               R.Forward=Monthly,dimA=dimA, dimB = dimB, dimC=dimC)

# Fa = N_SIGNAL_momentum_signal; Fb = NULL; Fc = NULL
# Single_Sort.output = conditional.sort(Fa=Fa, R.Forward=N_SIGNAL_Monthly,dimA=dimA)

############### FF 3-Factor Alphas ##############################################
FF=read_excel("C:/Users/talga/Desktop/Thesis/FF_Monthly_Good.xlsx")
FF$Date=ymd(FF$Date)
FF=xts(FF[,-1], order.by = FF$Date)

# Going for Double Sorts
#VOL FIRST
Double_Sort_VOL.Portfolio=list()
fit2=list()
eq=list()
vcovHAC_NW=list()
coeftest=list()

for (i in 1:30) {
  Double_Sort_VOL.Portfolio[[i]]=VOL.output$returns[,i]
  time(Double_Sort_VOL.Portfolio[[i]])<- time(Double_Sort_VOL.Portfolio[[i]]) %>% 
    as.yearmon() %>% as.Date()
  Double_Sort_VOL.Portfolio[[i]]=merge(FF[4:nrow(FF),], Double_Sort_VOL.Portfolio[[i]])
  Double_Sort_VOL.Portfolio[[i]]=na.omit(Double_Sort_VOL.Portfolio[[i]])
  Double_Sort_VOL.Portfolio[[i]][,5]=Double_Sort_VOL.Portfolio[[i]][,5]-
    Double_Sort_VOL.Portfolio[[i]][,4]
  eq <- paste(noquote(paste0("X",i)),"~ MktxRF+SMB+HML")
  fit2[[i]]=lm(as.formula(eq),         
               data = Double_Sort_VOL.Portfolio[[i]]
  )
  vcovHAC_NW[[i]]=vcovHAC(fit2[[i]], weights = bwNeweyWest)
  coeftest[[i]]=coeftest(fit2[[i]], vcov. = vcovHAC_NW[[i]])
}

Alpha=vector()
for (i in 1:30) {
  Alpha[i]=coeftest[[i]][1,1]  
}
Alpha=as.data.frame(Alpha)
Alpha$Alpha=Alpha$Alpha*12

t_stat=vector()
for (i in 1:30) {
  t_stat[i]=coeftest[[i]][1,3]  
}
t_stat=as.data.frame(t_stat)

table.AnnualizedReturns(VOL.output$returns,scale = 252, geometric = TRUE)
turnover.output = portfolio.turnover(VOL.output)
turnover=as.data.frame(turnover.output$`Mean Turnover`)


Double_Sort_VOL.Portfolio = VOL.output$returns[,30] + (-1*VOL.output$returns[,20])
time(Double_Sort_VOL.Portfolio)<- time(Double_Sort_VOL.Portfolio) %>% 
  as.yearmon() %>% as.Date()
Double_Sort_VOL.Portfolio=merge(FF[4:nrow(FF),], Double_Sort_VOL.Portfolio)
Double_Sort_VOL.Portfolio=na.omit(Double_Sort_VOL.Portfolio)
Double_Sort_VOL.Portfolio[,5]=Double_Sort_VOL.Portfolio[,5]-Double_Sort_VOL.Portfolio[,4]
eq <- paste(noquote(paste0("X",10)),"~ MktxRF+SMB+HML")
fit2=lm(as.formula(eq),         
        data = Double_Sort_VOL.Portfolio
)
vcovHAC_NW=vcovHAC(fit2, weights = bwNeweyWest)
coeftest=coeftest(fit2, vcov. = vcovHAC_NW)
coeftest

#Now VRY regs:
Double_Sort_VRY.Portfolio=list()
fit2=list()
eq=list()
vcovHAC_NW=list()
coeftest=list()

for (i in 1:30) {
  Double_Sort_VRY.Portfolio[[i]]=VRY.output$returns[,i]
  time(Double_Sort_VRY.Portfolio[[i]])<- time(Double_Sort_VRY.Portfolio[[i]]) %>% 
    as.yearmon() %>% as.Date()
  Double_Sort_VRY.Portfolio[[i]]=merge(FF[4:nrow(FF),], Double_Sort_VRY.Portfolio[[i]])
  Double_Sort_VRY.Portfolio[[i]]=na.omit(Double_Sort_VRY.Portfolio[[i]])
  Double_Sort_VRY.Portfolio[[i]][,5]=Double_Sort_VRY.Portfolio[[i]][,5]-
    Double_Sort_VRY.Portfolio[[i]][,4]
  eq <- paste(noquote(paste0("X",i)),"~ MktxRF+SMB+HML")
  fit2[[i]]=lm(as.formula(eq),         
               data = Double_Sort_VRY.Portfolio[[i]]
  )
  vcovHAC_NW[[i]]=vcovHAC(fit2[[i]], weights = bwNeweyWest)
  coeftest[[i]]=coeftest(fit2[[i]], vcov. = vcovHAC_NW[[i]])
}

Alpha=vector()
for (i in 1:30) {
  Alpha[i]=coeftest[[i]][1,1]  
}
Alpha=as.data.frame(Alpha)
Alpha$Alpha=Alpha$Alpha*12

t_stat=vector()
for (i in 1:30) {
  t_stat[i]=coeftest[[i]][1,3]  
}
t_stat=as.data.frame(t_stat)

table.AnnualizedReturns(VRY.output$returns,scale = 252, geometric = TRUE)
turnover.output = portfolio.turnover(VRY.output)
turnover=as.data.frame(turnover.output$`Mean Turnover`)

Double_Sort_VRY.Portfolio = VRY.output$returns[,30] + (-1*VRY.output$returns[,21])
time(Double_Sort_VRY.Portfolio)<- time(Double_Sort_VRY.Portfolio) %>% 
  as.yearmon() %>% as.Date()
Double_Sort_VRY.Portfolio=merge(FF[4:nrow(FF),], Double_Sort_VRY.Portfolio)
Double_Sort_VRY.Portfolio=na.omit(Double_Sort_VRY.Portfolio)
Double_Sort_VRY.Portfolio[,5]=Double_Sort_VRY.Portfolio[,5]-Double_Sort_VRY.Portfolio[,4]
eq <- paste(noquote(paste0("X",20)),"~ MktxRF+SMB+HML")
fit2=lm(as.formula(eq),         
        data = Double_Sort_VRY.Portfolio
)
vcovHAC_NW=vcovHAC(fit2, weights = bwNeweyWest)
coeftest=coeftest(fit2, vcov. = vcovHAC_NW)
coeftest
