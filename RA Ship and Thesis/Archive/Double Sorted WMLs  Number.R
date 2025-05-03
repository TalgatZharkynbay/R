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
##################### WML (11,1,1) Double Sorted Alpha ################################
Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_YFonly_Clean.xlsx")
FF=read_excel("C:/Users/talga/Desktop/Thesis/FF_Monthly_Good.xlsx")
FF$Date=ymd(FF$Date)
FF=xts(FF[,-1], order.by = FF$Date)
Monthly=xts(Daily[, -1], order.by = Daily$index)
Monthly=apply.monthly(Monthly, colSums) 

momentum_signal=rollapplyr(Monthly, width=11 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)
momentum_signal=xts::lag.xts(momentum_signal, 1)#1-month skip
momentum_signal=na.omit(momentum_signal)

setwd("C:/Users/talga/Desktop/Thesis/Delta Number and Delta Fraction")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

for (i in 1:30) {
  df.list[[i]]$qtr.y= as.Date(as.yearqtr(df.list[[i]]$qtr.y, format='%YQ%q'), frac = 1)
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

for (i in 1:30) {
  for (j in 1:3) {
    demand_signal[[i]][j,]=df.list[[i]]$Delta_N    
  }
}

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

delta_n_signal <- do.call(rbind, lapply(demand_signal, proc, template=template(demand_signal)))
res=as.data.table(delta_n_signal)
res=as.data.frame(res)
Test <- res[,!colSums(res==0)]
Test=xts(Test[,-1], order.by = Test$index)
delta_n_signal=Test

delta_n_signal=delta_n_signal[, colnames(delta_n_signal) %in% colnames(Monthly)]
Monthly=Monthly[, colnames(Monthly) %in% colnames(delta_n_signal)]
momentum_signal=momentum_signal[, colnames(momentum_signal) %in% colnames(delta_n_signal)]
Monthly=Monthly["2013-04-30/2020-09-30"]
momentum_signal=momentum_signal["2013-04-30/2020-09-30"]

Fa = momentum_signal; Fb = delta_n_signal; Fc = NULL
dimA = 0:10/10
dimB =0:3/3
XSMOM.output = conditional.sort(Fa=Fa, Fb=Fb,R.Forward=Monthly,dimA=dimA, dimB = dimB)

# portfolio.turnover(XSMOM.output)$`Mean Turnover`
# portfolio.frequency(XSMOM.output, rank = 1)
# portfolio.mean.size(XSMOM.output)
table.AnnualizedReturns(XSMOM.output$returns,scale = 365, geometric = FALSE)

LS.Portfolio = XSMOM.output$returns[,30] + (-1*XSMOM.output$returns[,1])#WML
skewness(LS.Portfolio)
sum(LS.Portfolio)
table.AnnualizedReturns(LS.Portfolio,scale = 365, geometric = FALSE)
chart.CumReturns(LS.Portfolio, geometric = FALSE, main = "XSMOM Long-Short Portfolio")

LS.Portfolio %>% 
  ggplot(aes(x=`30`)) +
  stat_density(geom = "line", alpha = 1) +
  ggtitle("WML(11,1,1)") +
  xlab("monthly returns") +
  ylab("distribution")

time(LS.Portfolio)<- time(LS.Portfolio) %>% as.yearmon() %>% as.Date()
LS.Portfolio=merge(FF[4:nrow(FF),], LS.Portfolio)
#LS.Portfolio=LS.Portfolio[-nrow(LS.Portfolio),]
#LS.Portfolio=LS.Portfolio[-nrow(LS.Portfolio),]
LS.Portfolio[, ncol(LS.Portfolio)]=LS.Portfolio[,ncol(LS.Portfolio)]-LS.Portfolio$RF
LS.Portfolio=na.omit(LS.Portfolio)

fit=lm(X30~MktxRF+SMB+HML, data = LS.Portfolio)
summary(fit)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest(fit, vcov. = vcovHAC_NW)

