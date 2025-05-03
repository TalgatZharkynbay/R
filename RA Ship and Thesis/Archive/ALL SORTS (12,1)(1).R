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
Monthly=xts(Daily[, -1], order.by = Daily$index)
Monthly=apply.monthly(Monthly, colSums) 

momentum_signal=rollapplyr(Monthly, width=12 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)
momentum_signal=xts::lag.xts(momentum_signal, 1)#1-month skip
momentum_signal=na.omit(momentum_signal)

############### ABS VARS ########################################################
setwd("C:/Users/talga/Desktop/Thesis/ABS Delta Number and  ABS Delta Fraction")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

for (i in 1:30) {
  df.list[[i]]$qtr.y= as.Date(as.yearqtr(df.list[[i]]$qtr.y, format='%YQ%q'), frac = 1)
}

for (i in 1:30) {
  df.list[[i]]=df.list[[i]]%>%
    filter(Delta_F_ABS<1)
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

ABS_N_SIGNAL=demand_signal
ABS_F_SIGNAL=demand_signal

for (i in 1:30) {
  for (j in 1:3) {
    ABS_N_SIGNAL[[i]][j,]=df.list[[i]]$Delta_N_ABS
  }
}

for (i in 1:30) {
  for (j in 1:3) {
    ABS_F_SIGNAL[[i]][j,]=df.list[[i]]$Delta_F_ABS
  }
}

############# NON ABS VARS ###################################################################

setwd("C:/Users/talga/Desktop/Thesis/Delta Number and Delta Fraction")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)


for (i in 1:30) {
  df.list[[i]]$qtr.y= as.Date(as.yearqtr(df.list[[i]]$qtr.y, format='%YQ%q'), frac = 1)
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

rm(df.list, demand_signal)
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

ABS_N_SIGNAL <- do.call(rbind, lapply(ABS_N_SIGNAL, proc, template=template(ABS_N_SIGNAL)))
ABS_F_SIGNAL <- do.call(rbind, lapply(ABS_F_SIGNAL, proc, template=template(ABS_F_SIGNAL)))
N_SIGNAL <- do.call(rbind, lapply(N_SIGNAL, proc, template=template(N_SIGNAL)))
F_SIGNAL <- do.call(rbind, lapply(F_SIGNAL, proc, template=template(F_SIGNAL)))

# The Big Question (?)
# ABS_N_SIGNAL=as.data.table(ABS_N_SIGNAL)
# ABS_N_SIGNAL=as.data.frame(ABS_N_SIGNAL)
# ABS_N_SIGNAL <- ABS_N_SIGNAL[,!colSums(ABS_N_SIGNAL==0)]
# ABS_N_SIGNAL=xts(ABS_N_SIGNAL[,-1], order.by = ABS_N_SIGNAL$index)

# ABS_F_SIGNAL=as.data.table(ABS_F_SIGNAL)
# ABS_F_SIGNAL=as.data.frame(ABS_F_SIGNAL)
# ABS_F_SIGNAL <- ABS_F_SIGNAL[,!colSums(ABS_F_SIGNAL==0)]
# ABS_F_SIGNAL=xts(ABS_F_SIGNAL[,-1], order.by = ABS_F_SIGNAL$index)

# N_SIGNAL=as.data.table(N_SIGNAL)
# N_SIGNAL=as.data.frame(N_SIGNAL)
# N_SIGNAL <- N_SIGNAL[,!colSums(N_SIGNAL==0)]
# N_SIGNAL=xts(N_SIGNAL[,-1], order.by = N_SIGNAL$index)

# F_SIGNAL=as.data.table(F_SIGNAL)
# F_SIGNAL=as.data.frame(F_SIGNAL)
# F_SIGNAL <- F_SIGNAL[,!colSums(F_SIGNAL==0)]
# F_SIGNAL=xts(F_SIGNAL[,-1], order.by = F_SIGNAL$index)


ABS_N_SIGNAL=ABS_N_SIGNAL[, colnames(ABS_N_SIGNAL) %in% colnames(Monthly)]
ABS_F_SIGNAL=ABS_F_SIGNAL[, colnames(ABS_F_SIGNAL) %in% colnames(Monthly)]
N_SIGNAL=N_SIGNAL[, colnames(N_SIGNAL) %in% colnames(Monthly)]
F_SIGNAL=F_SIGNAL[, colnames(F_SIGNAL) %in% colnames(Monthly)]

ABS_N_SIGNAL_Monthly=Monthly[, colnames(Monthly) %in% colnames(ABS_N_SIGNAL)]
ABS_F_SIGNAL_Monthly=Monthly[, colnames(Monthly) %in% colnames(ABS_F_SIGNAL)]
N_SIGNAL_Monthly=Monthly[, colnames(Monthly)%in% colnames(N_SIGNAL)]
F_SIGNAL_Monthly=Monthly[, colnames(Monthly) %in% colnames(F_SIGNAL)]

ABS_N_SIGNAL_momentum_signal=momentum_signal[, colnames(momentum_signal)
                                             %in% colnames(ABS_N_SIGNAL)]
ABS_F_SIGNAL_momentum_signal=momentum_signal[, colnames(momentum_signal)
                                             %in% colnames(ABS_F_SIGNAL)]
N_SIGNAL_momentum_signal=momentum_signal[, colnames(momentum_signal)%in% colnames(N_SIGNAL)]
F_SIGNAL_momentum_signal=momentum_signal[, colnames(momentum_signal)
                                             %in% colnames(F_SIGNAL)]

ABS_N_SIGNAL_Monthly=ABS_N_SIGNAL_Monthly["2013-04-30/2020-09-30"]
ABS_F_SIGNAL_Monthly=ABS_F_SIGNAL_Monthly["2013-04-30/2020-09-30"]
N_SIGNAL_Monthly=N_SIGNAL_Monthly["2013-04-30/2020-09-30"]
F_SIGNAL_Monthly=F_SIGNAL_Monthly["2013-04-30/2020-09-30"]

ABS_N_SIGNAL_momentum_signal=ABS_N_SIGNAL_momentum_signal["2013-04-30/2020-09-30"]
ABS_F_SIGNAL_momentum_signal=ABS_F_SIGNAL_momentum_signal["2013-04-30/2020-09-30"]
N_SIGNAL_momentum_signal=N_SIGNAL_momentum_signal["2013-04-30/2020-09-30"]
F_SIGNAL_momentum_signal=F_SIGNAL_momentum_signal["2013-04-30/2020-09-30"]

dimA = 0:10/10
dimB =0:3/3

Fa = ABS_N_SIGNAL_momentum_signal; Fb = ABS_N_SIGNAL; Fc = NULL
ABS_N_SIGNAL.output = conditional.sort(Fa=Fa, Fb=Fb,
                                       R.Forward=ABS_N_SIGNAL_Monthly,dimA=dimA, dimB = dimB)

Fa = ABS_F_SIGNAL_momentum_signal; Fb = ABS_F_SIGNAL; Fc = NULL
ABS_F_SIGNAL.output = conditional.sort(Fa=Fa, Fb=Fb,
                                       R.Forward=ABS_F_SIGNAL_Monthly,dimA=dimA, dimB = dimB)

Fa = N_SIGNAL_momentum_signal; Fb = N_SIGNAL; Fc = NULL
N_SIGNAL.output = conditional.sort(Fa=Fa, Fb=Fb,
                                       R.Forward=N_SIGNAL_Monthly,dimA=dimA, dimB = dimB)

Fa = F_SIGNAL_momentum_signal; Fb = F_SIGNAL; Fc = NULL
F_SIGNAL.output = conditional.sort(Fa=Fa, Fb=Fb,
                                       R.Forward=F_SIGNAL_Monthly,dimA=dimA, dimB = dimB)

Fa = N_SIGNAL_momentum_signal; Fb = NULL; Fc = NULL
Single_Sort.output = conditional.sort(Fa=Fa, R.Forward=N_SIGNAL_Monthly,dimA=dimA)

# dimA = 0:10/10
# dimB =0:3/3
# dimC=0:3/3
# Fa = N_SIGNAL_momentum_signal; Fb = N_SIGNAL; Fc = F_SIGNAL
# Triple_Sorted.output = conditional.sort(Fa=Fa, Fb=Fb, Fc=Fc,
#                                    R.Forward=N_SIGNAL_Monthly,dimA=dimA, dimB = dimB,
#                                    dimC = dimC)

# portfolio.turnover(XSMOM.output)$`Mean Turnover`
# portfolio.frequency(XSMOM.output, rank = 1)
# portfolio.mean.size(XSMOM.output)
ABS_N_SIGNAL.Portfolio = ABS_N_SIGNAL.output$returns[,30] + (-1*ABS_N_SIGNAL.output$returns[,1])
ABS_F_SIGNAL.Portfolio = ABS_F_SIGNAL.output$returns[,30] + (-1*ABS_F_SIGNAL.output$returns[,1])
N_SIGNAL.Portfolio = N_SIGNAL.output$returns[,30] + (-1*N_SIGNAL.output$returns[,1])
F_SIGNAL.Portfolio = F_SIGNAL.output$returns[,30] + (-1*F_SIGNAL.output$returns[,1])
Single_Sort.Portfolio = Single_Sort.output$returns[,10] + (-1*Single_Sort.output$returns[,1])
#Triple_Sorted.Portfolio=Triple_Sorted.output$returns[,90] + (-1*Triple_Sorted.output$returns[,1])
 
Portfolios = cbind(ABS_N_SIGNAL.Portfolio,ABS_F_SIGNAL.Portfolio, N_SIGNAL.Portfolio,
F_SIGNAL.Portfolio, Single_Sort.Portfolio)

Portfolios = cbind(N_SIGNAL.Portfolio, Single_Sort.Portfolio)

#colnames(Portfolios) = c("ABS Number","ABS Fraction", "Number", "Fraction", "Single Sort")

colnames(Portfolios) = c("Ret and Number","Ret Single Sort")

chart.CumReturns(Portfolios, geometric = FALSE, legend.loc = "topleft",
                 main = "(12,1)")

table.AnnualizedReturns(ABS_N_SIGNAL.output$returns,scale = 365, geometric = FALSE)
table.AnnualizedReturns(ABS_F_SIGNAL.output$returns,scale = 365, geometric = FALSE)
table.AnnualizedReturns(N_SIGNAL.output$returns,scale = 365, geometric = FALSE)
table.AnnualizedReturns(F_SIGNAL.output$returns,scale = 365, geometric = FALSE)
table.AnnualizedReturns(Single_Sort.output$returns,scale = 365, geometric = FALSE)

# table.AnnualizedReturns(N_SIGNAL.Portfolio,scale = 365, geometric = FALSE)
# table.AnnualizedReturns(Single_Sort.Portfolio,scale = 365, geometric = FALSE)

############### FF 3-Factor Alphas ##############################################
FF=read_excel("C:/Users/talga/Desktop/Thesis/FF_Monthly_Good.xlsx")
FF$Date=ymd(FF$Date)
FF=xts(FF[,-1], order.by = FF$Date)
#
time(N_SIGNAL.Portfolio)<- time(N_SIGNAL.Portfolio) %>% as.yearmon() %>% as.Date()
N_SIGNAL.Portfolio=merge(FF[4:nrow(FF),], N_SIGNAL.Portfolio)
N_SIGNAL.Portfolio=na.omit(N_SIGNAL.Portfolio)
N_SIGNAL.Portfolio$X30=N_SIGNAL.Portfolio$X30-N_SIGNAL.Portfolio$RF
fit1=lm(X30~MktxRF+SMB+HML, data = N_SIGNAL.Portfolio)
summary(fit1)
vcovHAC_NW=vcovHAC(fit1, weights = bwNeweyWest)
coeftest(fit1, vcov. = vcovHAC_NW)

# time(Single_Sort.Portfolio)<- time(Single_Sort.Portfolio) %>% as.yearmon() %>% as.Date()
# Single_Sort.Portfolio=merge(FF[4:nrow(FF),], Single_Sort.Portfolio)
# Single_Sort.Portfolio=na.omit(Single_Sort.Portfolio)
# Single_Sort.Portfolio$X10=Single_Sort.Portfolio$X10-Single_Sort.Portfolio$RF
# fit2=lm(X10~MktxRF+SMB+HML, data = Single_Sort.Portfolio)
# summary(fit2)
# vcovHAC_NW=vcovHAC(fit2, weights = bwNeweyWest)
# coeftest(fit2, vcov. = vcovHAC_NW)

