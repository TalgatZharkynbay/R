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
Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_YFonly_Clean.xlsx")

# Monthly <- read_excel("C:/Users/talga/Desktop/Thesis/RET_VRY_CLEAN.xlsx")
# Monthly=xts(Monthly[,-1], order.by = Monthly$date)
# Monthly=apply.monthly(Monthly, colSums)

Monthly <-read_excel("C:/Users/talga/Desktop/Thesis/CRSP_Monthly_Clean.xlsx")
Monthly=xts(Monthly[, -1], order.by = Monthly$date)

momentum_signal=rollapplyr(Monthly, width=12 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)
momentum_signal=xts::lag.xts(momentum_signal, 1)#1-month skip
momentum_signal=na.omit(momentum_signal)

############# NON ABS VARS ###################################################################
setwd("C:/Users/talga/Desktop/Thesis/Delta Number and Delta Fraction")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

for (i in 1:30) {
  df.list[[i]]$qtr.y= as.Date(as.yearqtr(df.list[[i]]$qtr.y, format='%YQ%q'), frac = 1)
  #df.list[[i]]$qtr.y= as.Date(as.yearqtr(df.list[[i]]$qtr.y, format='%YQ%q'), frac = 1)+days(31)
}

for (i in 1:30) {
  df.list[[i]]=df.list[[i]]%>%
    filter(abs(Delta_F)<1)
}

demand_signal=list()

for (i in 1:31) {
  demand_signal[[i]]=data.frame(matrix(0,nrow=nrow(Daily), ncol=length(df.list[[i]]$tic)))
  colnames(demand_signal[[i]])=df.list[[i]]$tic
  demand_signal[[i]]=xts(demand_signal[[i]], order.by = Daily$index)
  demand_signal[[i]]=apply.monthly(demand_signal[[i]], colSums)
  demand_signal[[i]]=demand_signal[[i]][paste0(df.list[[i]]$qtr.y[1]+days(1)
                                               ,"/",df.list[[i+1]]$qtr.y[1])]
}

demand_signal[[30]]=data.frame(matrix(0,nrow=nrow(Daily), ncol=length(df.list[[30]]$tic)))
colnames(demand_signal[[30]])=df.list[[30]]$tic
demand_signal[[30]]=xts(demand_signal[[30]], order.by = Daily$index)
demand_signal[[30]]=apply.monthly(demand_signal[[30]], colSums)
demand_signal[[30]]=demand_signal[[30]][paste0(df.list[[30]]$qtr.y[1]+days(1)
                                               ,"/",df.list[[30]]$qtr.y[1]+days(1)+months(3))]
N_SIGNAL=demand_signal
F_SIGNAL=demand_signal

for (i in 1:30) {
  for (j in 1:3) {
    N_SIGNAL[[i]][j,]=df.list[[i]]$Delta_N    
  }
}

for (i in 1:30) {
  for (j in 1:3) {
    F_SIGNAL[[i]][j,]=df.list[[i]]$Delta_F
  }
}

rm(df.list, demand_signal, Daily)
#Holy Shit
template <- function(xlist) {
  cn <- unique(unlist(lapply(xlist, colnames)))
  minIndex <- do.call(min, lapply(xlist, function(x) index(x[1L,])))
  xts(matrix(0,1,length(cn)), minIndex-1, dimnames=list(NULL, sort(cn)))
}

proc <- function(x, template) {
  neededCols <- !(colnames(template) %in% colnames(x))
  out <- merge(x, template[,neededCols], fill=0)
  out <- out[-1L,make.names(colnames(template))]
  attr(out, "dimnames") <- list(NULL, colnames(template))
  out
}

N_SIGNAL <- do.call(rbind, lapply(N_SIGNAL, proc, template=template(N_SIGNAL)))
F_SIGNAL <- do.call(rbind, lapply(F_SIGNAL, proc, template=template(F_SIGNAL)))

N_SIGNAL=N_SIGNAL[, colnames(N_SIGNAL) %in% colnames(Monthly)]
F_SIGNAL=F_SIGNAL[, colnames(F_SIGNAL) %in% colnames(Monthly)]

N_SIGNAL_Monthly=Monthly[, colnames(Monthly)%in% colnames(N_SIGNAL)]
F_SIGNAL_Monthly=Monthly[, colnames(Monthly) %in% colnames(F_SIGNAL)]

N_SIGNAL_momentum_signal=momentum_signal[, colnames(momentum_signal)%in% colnames(N_SIGNAL)]

F_SIGNAL_momentum_signal=momentum_signal[, colnames(momentum_signal)%in% colnames(F_SIGNAL)]

N_SIGNAL_Monthly=N_SIGNAL_Monthly["2013-04-30/2020-09-30"]
F_SIGNAL_Monthly=F_SIGNAL_Monthly["2013-04-30/2020-09-30"]

N_SIGNAL_momentum_signal=N_SIGNAL_momentum_signal["2013-04-30/2020-09-30"]
F_SIGNAL_momentum_signal=F_SIGNAL_momentum_signal["2013-04-30/2020-09-30"]

N_SIGNAL=N_SIGNAL["/2019-12-31"]
F_SIGNAL=F_SIGNAL["/2019-12-31"]


F_SIGNAL_momentum_signal=F_SIGNAL_momentum_signal[ , order(colnames(F_SIGNAL_momentum_signal))]
F_SIGNAL_Monthly=F_SIGNAL_Monthly[ , order(colnames(F_SIGNAL_Monthly))]
N_SIGNAL_momentum_signal=N_SIGNAL_momentum_signal[ , order(colnames(N_SIGNAL_momentum_signal))]
N_SIGNAL_Monthly=N_SIGNAL_Monthly[ , order(colnames(N_SIGNAL_Monthly))]

dimA = 0:10/10
dimB =0:3/3

Fa = N_SIGNAL_momentum_signal; Fb = N_SIGNAL; Fc = NULL
N_SIGNAL.output = conditional.sort(Fa=Fa, Fb=Fb,
                                       R.Forward=N_SIGNAL_Monthly,dimA=dimA, dimB = dimB)

Fa = F_SIGNAL_momentum_signal; Fb = F_SIGNAL; Fc = NULL
F_SIGNAL.output = conditional.sort(Fa=Fa, Fb=Fb,
                                       R.Forward=F_SIGNAL_Monthly,dimA=dimA, dimB = dimB)

Fa = N_SIGNAL_momentum_signal; Fb = NULL; Fc = NULL
Single_Sort.output = conditional.sort(Fa=Fa, R.Forward=N_SIGNAL_Monthly,dimA=dimA)
 
############### FF 3-Factor Alphas ##############################################
FF=read_excel("C:/Users/talga/Desktop/Thesis/FF_Monthly_Good.xlsx")
FF$Date=ymd(FF$Date)
FF=xts(FF[,-1], order.by = FF$Date)

#Starting with Single Sorts
Single_Sort.Portfolio=list()
fit2=list()
eq=list()
vcovHAC_NW=list()
coeftest=list()

for (i in 1:10) {
  Single_Sort.Portfolio[[i]]=Single_Sort.output$returns[,i]
  time(Single_Sort.Portfolio[[i]])<- time(Single_Sort.Portfolio[[i]]) %>% 
    as.yearmon() %>% as.Date()
  Single_Sort.Portfolio[[i]]=merge(FF[4:nrow(FF),], Single_Sort.Portfolio[[i]])
  Single_Sort.Portfolio[[i]]=na.omit(Single_Sort.Portfolio[[i]])
  Single_Sort.Portfolio[[i]][,5]=Single_Sort.Portfolio[[i]][,5]-Single_Sort.Portfolio[[i]][,4]
  eq <- paste(noquote(paste0("X",i)),"~ MktxRF+SMB+HML")
  fit2[[i]]=lm(as.formula(eq),         
               data = Single_Sort.Portfolio[[i]]
               )
  vcovHAC_NW[[i]]=vcovHAC(fit2[[i]], weights = bwNeweyWest)
  coeftest[[i]]=coeftest(fit2[[i]], vcov. = vcovHAC_NW[[i]])
}

Alpha=vector()
for (i in 1:10) {
  Alpha[i]=coeftest[[i]][1,1]  
}
Alpha=as.data.frame(Alpha)
Alpha$Alpha=Alpha$Alpha*12

t_stat=vector()
for (i in 1:10) {
  t_stat[i]=coeftest[[i]][1,3]  
}
t_stat=as.data.frame(t_stat)

table.AnnualizedReturns(Single_Sort.output$returns,scale = 252, geometric = TRUE)
turnover.output = portfolio.turnover(Single_Sort.output)
turnover=as.data.frame(turnover.output$`Mean Turnover`)

Single_Sort.Portfolio = Single_Sort.output$returns[,10] + (-1*Single_Sort.output$returns[,1])
time(Single_Sort.Portfolio)<- time(Single_Sort.Portfolio) %>% 
  as.yearmon() %>% as.Date()
Single_Sort.Portfolio=merge(FF[4:nrow(FF),], Single_Sort.Portfolio)
Single_Sort.Portfolio=na.omit(Single_Sort.Portfolio)
Single_Sort.Portfolio[,5]=Single_Sort.Portfolio[,5]-Single_Sort.Portfolio[,4]
eq <- paste(noquote(paste0("X",10)),"~ MktxRF+SMB+HML")
fit2=lm(as.formula(eq),         
             data = Single_Sort.Portfolio
)
vcovHAC_NW=vcovHAC(fit2, weights = bwNeweyWest)
coeftest=coeftest(fit2, vcov. = vcovHAC_NW)
coeftest

# Going for Double Sorts
#Delta_N_First
Double_Sort_N.Portfolio=list()
fit2=list()
eq=list()
vcovHAC_NW=list()
coeftest=list()

for (i in 1:30) {
  Double_Sort_N.Portfolio[[i]]=N_SIGNAL.output$returns[,i]
  time(Double_Sort_N.Portfolio[[i]])<- time(Double_Sort_N.Portfolio[[i]]) %>% 
    as.yearmon() %>% as.Date()
  Double_Sort_N.Portfolio[[i]]=merge(FF[4:nrow(FF),], Double_Sort_N.Portfolio[[i]])
  Double_Sort_N.Portfolio[[i]]=na.omit(Double_Sort_N.Portfolio[[i]])
  Double_Sort_N.Portfolio[[i]][,5]=Double_Sort_N.Portfolio[[i]][,5]-
    Double_Sort_N.Portfolio[[i]][,4]
  eq <- paste(noquote(paste0("X",i)),"~ MktxRF+SMB+HML")
  fit2[[i]]=lm(as.formula(eq),         
               data = Double_Sort_N.Portfolio[[i]]
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

table.AnnualizedReturns(N_SIGNAL.output$returns,scale = 252, geometric = TRUE)
turnover.output = portfolio.turnover(N_SIGNAL.output)
turnover=as.data.frame(turnover.output$`Mean Turnover`)


Double_Sort_N.Portfolio = N_SIGNAL.output$returns[,10] + (-1*N_SIGNAL.output$returns[,1])
time(Double_Sort_N.Portfolio)<- time(Double_Sort_N.Portfolio) %>% 
  as.yearmon() %>% as.Date()
Double_Sort_N.Portfolio=merge(FF[4:nrow(FF),], Double_Sort_N.Portfolio)
Double_Sort_N.Portfolio=na.omit(Double_Sort_N.Portfolio)
Double_Sort_N.Portfolio[,5]=Double_Sort_N.Portfolio[,5]-Double_Sort_N.Portfolio[,4]
eq <- paste(noquote(paste0("X",10)),"~ MktxRF+SMB+HML")
fit2=lm(as.formula(eq),         
        data = Double_Sort_N.Portfolio
)
vcovHAC_NW=vcovHAC(fit2, weights = bwNeweyWest)
coeftest=coeftest(fit2, vcov. = vcovHAC_NW)
coeftest

#Now Delta_F regs:
Double_Sort_F.Portfolio=list()
fit2=list()
eq=list()
vcovHAC_NW=list()
coeftest=list()

for (i in 1:30) {
  Double_Sort_F.Portfolio[[i]]=F_SIGNAL.output$returns[,i]
  time(Double_Sort_F.Portfolio[[i]])<- time(Double_Sort_F.Portfolio[[i]]) %>% 
    as.yearmon() %>% as.Date()
  Double_Sort_F.Portfolio[[i]]=merge(FF[4:nrow(FF),], Double_Sort_F.Portfolio[[i]])
  Double_Sort_F.Portfolio[[i]]=na.omit(Double_Sort_F.Portfolio[[i]])
  Double_Sort_F.Portfolio[[i]][,5]=Double_Sort_F.Portfolio[[i]][,5]-
    Double_Sort_F.Portfolio[[i]][,4]
  eq <- paste(noquote(paste0("X",i)),"~ MktxRF+SMB+HML")
  fit2[[i]]=lm(as.formula(eq),         
               data = Double_Sort_F.Portfolio[[i]]
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

table.AnnualizedReturns(F_SIGNAL.output$returns,scale = 252, geometric = TRUE)
turnover.output = portfolio.turnover(F_SIGNAL.output)
turnover=as.data.frame(turnover.output$`Mean Turnover`)

Double_Sort_F.Portfolio = F_SIGNAL.output$returns[,10] + (-1*F_SIGNAL.output$returns[,2])
time(Double_Sort_F.Portfolio)<- time(Double_Sort_F.Portfolio) %>% 
  as.yearmon() %>% as.Date()
Double_Sort_F.Portfolio=merge(FF[4:nrow(FF),], Double_Sort_F.Portfolio)
Double_Sort_F.Portfolio=na.omit(Double_Sort_F.Portfolio)
Double_Sort_F.Portfolio[,5]=Double_Sort_F.Portfolio[,5]-Double_Sort_F.Portfolio[,4]
eq <- paste(noquote(paste0("X",10)),"~ MktxRF+SMB+HML")
fit2=lm(as.formula(eq),         
        data = Double_Sort_F.Portfolio
)
vcovHAC_NW=vcovHAC(fit2, weights = bwNeweyWest)
coeftest=coeftest(fit2, vcov. = vcovHAC_NW)
coeftest
