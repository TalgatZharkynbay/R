options(scipen = 10, digits = 3)
library(portsort)
library(PerformanceAnalytics)
library(xts)
library(writexl)
library(lubridate)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(readxl)
library(readr)
library(data.table)
library(ggplot2)
library(StatMeasures)
library(lmtest)
library(sandwich)
library(highcharter)
##################### ALL THE DATA, MOMENTUM WITH SKIP ################################
Monthly <-read_excel("C:/Users/talga/Desktop/Thesis/RET.xlsx")
#momentum_signal <-read_excel("C:/Users/talga/Desktop/Thesis/RET_SIGNAL WITH SKIP.xlsx")
momentum_signal <-read_excel("C:/Users/talga/Desktop/Thesis/RET_SIGNAL NO SKIP.xlsx")
F_SIGNAL<-read_excel("C:/Users/talga/Desktop/Thesis/F_SIGNAL.xlsx")
N_SIGNAL<-read_excel("C:/Users/talga/Desktop/Thesis/N_SIGNAL.xlsx")
VRY<-read_excel("C:/Users/talga/Desktop/Thesis/VRY_SIGNAL.xlsx")
VOL<-read_excel("C:/Users/talga/Desktop/Thesis/VOL_SIGNAL.xlsx")
Monthly=xts(Monthly[, -1], order.by = Monthly$index)
Monthly=exp(Monthly)-1
momentum_signal=xts(momentum_signal[, -1], order.by = momentum_signal$index)
F_SIGNAL=xts(F_SIGNAL[, -1], order.by = F_SIGNAL$index)
N_SIGNAL=xts(N_SIGNAL[, -1], order.by = N_SIGNAL$index)
VRY=xts(VRY[, -1], order.by = VRY$index)
VOL=xts(VOL[, -1], order.by = VOL$index)
FF=read_excel("C:/Users/talga/Desktop/Thesis/FF_Monthly_Good.xlsx")
FF$Date=ymd(FF$Date)
FF=xts(FF[,-1], order.by = FF$Date)
FF=FF["2013-05-01/2019-12-01"]
F_SIGNAL=abs(F_SIGNAL)
N_SIGNAL=abs(N_SIGNAL)
##################### RET SINGLE SORTS ################################
rank_signal=t(apply(momentum_signal, 1, rank))
for (i in 1: nrow(rank_signal)) {
  rank_signal[i,]=decile(rank_signal[i,], decreasing = FALSE) #biggest values in decile 10   
}

rank_signal=xts(rank_signal, order.by = as.Date(rownames(rank_signal)))
Signals = replicate(n = 10,expr = {data.frame(matrix(, nrow=nrow(rank_signal), 
                                               ncol=ncol(rank_signal)))},simplify = F)
for (i in 1:10) {
  for (j in 1:ncol(rank_signal)) {
    Signals[[i]][,j]=ifelse(rank_signal[,j]==i, 1, 0)
    colnames(Signals[[i]])=colnames(rank_signal)
  }
}

Seq=rep(seq(1,79,3), each=3)
Seq=Seq[-81]
Port= replicate(n = 10,
                expr = {data.frame(matrix(, nrow=last(seq_along(Seq)),
                                          ncol=ncol(rank_signal)))},simplify = F)
WML=list()
for (j in 1:10) {
    for (i in 1:80) { #used to have 81 and i before
  Port[[j]][i,]=Monthly[i+1,]*xts(Signals[[j]][Seq[i],], order.by = index(Monthly)[i+1])
  WML[[j]]=xts(apply(Port[[j]], 1, sum)/length(which(Port[[j]][1,]!=0)),
               order.by = index(Monthly)[1:last(seq_along(Seq))+1])
}
}

fit=list()
vcovHAC_NW=list()
coeftest=list()
for (i in 1:10) {
  colnames(WML[[i]])=c("V1")
  #index((WML[[i]])=index((WML[[i]])+days(5)
  time(WML[[i]])<- time(WML[[i]]) %>%
    as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  fit[[i]]=lm(V1~MktxRF+SMB+HML,
         data = WML[[i]])
  vcovHAC_NW[[i]]=vcovHAC(fit[[i]], weights = bwNeweyWest)
  coeftest[[i]]=coeftest(fit[[i]], vcov. = vcovHAC_NW[[i]])
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
View(Alpha);View(t_stat)

Ten_Minus_1 = WML[[9]]-WML[[1]]
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest

##################### NUM SINGLE SORTS ################################
rm(FF, F_SIGNAL, VOL, VRY)
rank_signal=t(apply(N_SIGNAL, 1, rank))
for (i in 1: nrow(rank_signal)) {
  rank_signal[i,]=decile(rank_signal[i,], decreasing = FALSE) #biggest values in decile 10   
}

rank_signal=xts(rank_signal, order.by = as.Date(rownames(rank_signal)))
Signals = replicate(n = 10,expr = {data.frame(matrix(, nrow=nrow(rank_signal), 
                                                     ncol=ncol(rank_signal)))},simplify = F)
for (i in 1:10) {
  for (j in 1:ncol(rank_signal)) {
    Signals[[i]][,j]=ifelse(rank_signal[,j]==i, 1, 0)
    colnames(Signals[[i]])=colnames(rank_signal)
  }
}

Seq=rep(seq(1,79,3), each=3)
Seq=Seq[-81]
Port= replicate(n = 10,
                expr = {data.frame(matrix(, nrow=last(seq_along(Seq)),
                                          ncol=ncol(rank_signal)))},simplify = F)
WML=list()
for (j in 1:10) {
  for (i in 1:80) { #used to have 81 and i before
    Port[[j]][i,]=Monthly[i+1,]*xts(Signals[[j]][Seq[i],], order.by = index(Monthly)[i+1])
    WML[[j]]=xts(apply(Port[[j]], 1, sum)/length(which(Port[[j]][1,]!=0)),
                 order.by = index(Monthly)[1:last(seq_along(Seq))+1])
  }
}

fit=list()
vcovHAC_NW=list()
coeftest=list()
for (i in 1:10) {
  colnames(WML[[i]])=c("V1")
  #index((WML[[i]])=index((WML[[i]])+days(5)
  time(WML[[i]])<- time(WML[[i]]) %>%
    as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  fit[[i]]=lm(V1~MktxRF+SMB+HML,
              data = WML[[i]])
  vcovHAC_NW[[i]]=vcovHAC(fit[[i]], weights = bwNeweyWest)
  coeftest[[i]]=coeftest(fit[[i]], vcov. = vcovHAC_NW[[i]])
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
View(Alpha);View(t_stat)

Ten_Minus_1 = WML[[10]]-WML[[1]]
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest

Strategy=WML[[1]]$V1

getSymbols("SPY",
             src = 'yahoo',
             from = "2013-04-01",
             to = "2019-12-31",
             auto.assign = TRUE,
             warnings = FALSE) 
SPY=SPY$SPY.Adjusted

SPY <- to.monthly(SPY,
                             indexAt = "lastof",
                             OHLC = FALSE)
SPY <-
  Return.calculate(SPY,
                   method = "log") %>%
  na.omit()

time(SPY)<- time(SPY) %>%
  as.yearmon() %>% as.Date()

Graph=merge(Strategy, SPY)
colnames(Graph)=c("Strategy", "SPY")
Graph$Strategy=cumsum(Graph$Strategy)
Graph$SPY=cumsum(Graph$SPY)
highchart(type = "stock") %>%
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(Graph[, 1],
                name = colnames(Graph)[1]) %>%
  hc_add_series(Graph[, 2],
                name = colnames(Graph)[2]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)


##################### FRA SINGLE SORTS ################################
rank_signal=t(apply(F_SIGNAL, 1, rank))
for (i in 1: nrow(rank_signal)) {
  rank_signal[i,]=decile(rank_signal[i,], decreasing = FALSE) #biggest values in decile 10   
}

rank_signal=xts(rank_signal, order.by = as.Date(rownames(rank_signal)))
Signals = replicate(n = 10,expr = {data.frame(matrix(, nrow=nrow(rank_signal), 
                                                     ncol=ncol(rank_signal)))},simplify = F)
for (i in 1:10) {
  for (j in 1:ncol(rank_signal)) {
    Signals[[i]][,j]=ifelse(rank_signal[,j]==i, 1, 0)
    colnames(Signals[[i]])=colnames(rank_signal)
  }
}

Seq=rep(seq(1,79,3), each=3)
Seq=Seq[-81]
Port= replicate(n = 10,
                expr = {data.frame(matrix(, nrow=last(seq_along(Seq)),
                                          ncol=ncol(rank_signal)))},simplify = F)
WML=list()
for (j in 1:10) {
  for (i in 1:80) { #used to have 81 and i before
    Port[[j]][i,]=Monthly[i+1,]*xts(Signals[[j]][Seq[i],], order.by = index(Monthly)[i+1])
    WML[[j]]=xts(apply(Port[[j]], 1, sum)/length(which(Port[[j]][1,]!=0)),
                 order.by = index(Monthly)[1:last(seq_along(Seq))+1])
  }
}

fit=list()
vcovHAC_NW=list()
coeftest=list()
for (i in 1:10) {
  colnames(WML[[i]])=c("V1")
  #index((WML[[i]])=index((WML[[i]])+days(5)
  time(WML[[i]])<- time(WML[[i]]) %>%
    as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  fit[[i]]=lm(V1~MktxRF+SMB+HML,
              data = WML[[i]])
  vcovHAC_NW[[i]]=vcovHAC(fit[[i]], weights = bwNeweyWest)
  coeftest[[i]]=coeftest(fit[[i]], vcov. = vcovHAC_NW[[i]])
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
View(Alpha);View(t_stat)

Ten_Minus_1 = WML[[10]]-WML[[1]]
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest

##################### VOL SINGLE SORTS ################################
rank_signal=t(apply(VOL, 1, rank))
for (i in 1: nrow(rank_signal)) {
  rank_signal[i,]=decile(rank_signal[i,], decreasing = FALSE) #biggest values in decile 10   
}

rank_signal=xts(rank_signal, order.by = as.Date(rownames(rank_signal)))
Signals = replicate(n = 10,expr = {data.frame(matrix(, nrow=nrow(rank_signal), 
                                                     ncol=ncol(rank_signal)))},simplify = F)
for (i in 1:10) {
  for (j in 1:ncol(rank_signal)) {
    Signals[[i]][,j]=ifelse(rank_signal[,j]==i, 1, 0)
    colnames(Signals[[i]])=colnames(rank_signal)
  }
}

Seq=rep(seq(1,79,3), each=3)
Seq=Seq[-81]
Port= replicate(n = 10,
                expr = {data.frame(matrix(, nrow=last(seq_along(Seq)),
                                          ncol=ncol(rank_signal)))},simplify = F)
WML=list()
for (j in 1:10) {
  for (i in 1:80) { #used to have 81 and i before
    Port[[j]][i,]=Monthly[i+1,]*xts(Signals[[j]][Seq[i],], order.by = index(Monthly)[i+1])
    WML[[j]]=xts(apply(Port[[j]], 1, sum)/length(which(Port[[j]][1,]!=0)),
                 order.by = index(Monthly)[1:last(seq_along(Seq))+1])
  }
}

fit=list()
vcovHAC_NW=list()
coeftest=list()
for (i in 1:10) {
  colnames(WML[[i]])=c("V1")
  #index((WML[[i]])=index((WML[[i]])+days(5)
  time(WML[[i]])<- time(WML[[i]]) %>%
    as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  fit[[i]]=lm(V1~MktxRF+SMB+HML,
              data = WML[[i]])
  vcovHAC_NW[[i]]=vcovHAC(fit[[i]], weights = bwNeweyWest)
  coeftest[[i]]=coeftest(fit[[i]], vcov. = vcovHAC_NW[[i]])
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
View(Alpha);View(t_stat)

Ten_Minus_1 = WML[[10]]-WML[[1]]
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest

##################### VRY SINGLE SORTS ################################
rank_signal=t(apply(VRY, 1, rank))
for (i in 1: nrow(rank_signal)) {
  rank_signal[i,]=decile(rank_signal[i,], decreasing = FALSE) #biggest values in decile 10   
}

rank_signal=xts(rank_signal, order.by = as.Date(rownames(rank_signal)))
Signals = replicate(n = 10,expr = {data.frame(matrix(, nrow=nrow(rank_signal), 
                                                     ncol=ncol(rank_signal)))},simplify = F)
for (i in 1:10) {
  for (j in 1:ncol(rank_signal)) {
    Signals[[i]][,j]=ifelse(rank_signal[,j]==i, 1, 0)
    colnames(Signals[[i]])=colnames(rank_signal)
  }
}

Seq=rep(seq(1,79,3), each=3)
Seq=Seq[-81]
Port= replicate(n = 10,
                expr = {data.frame(matrix(, nrow=last(seq_along(Seq)),
                                          ncol=ncol(rank_signal)))},simplify = F)
WML=list()
for (j in 1:10) {
  for (i in 1:80) { #used to have 81 and i before
    Port[[j]][i,]=Monthly[i+1,]*xts(Signals[[j]][Seq[i],], order.by = index(Monthly)[i+1])
    WML[[j]]=xts(apply(Port[[j]], 1, sum)/length(which(Port[[j]][1,]!=0)),
                 order.by = index(Monthly)[1:last(seq_along(Seq))+1])
  }
}

fit=list()
vcovHAC_NW=list()
coeftest=list()
for (i in 1:10) {
  colnames(WML[[i]])=c("V1")
  #index((WML[[i]])=index((WML[[i]])+days(5)
  time(WML[[i]])<- time(WML[[i]]) %>%
    as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  fit[[i]]=lm(V1~MktxRF+SMB+HML,
              data = WML[[i]])
  vcovHAC_NW[[i]]=vcovHAC(fit[[i]], weights = bwNeweyWest)
  coeftest[[i]]=coeftest(fit[[i]], vcov. = vcovHAC_NW[[i]])
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
View(Alpha);View(t_stat)

Ten_Minus_1 = WML[[10]]-WML[[1]]
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest

