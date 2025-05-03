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
##################### ALL THE DATA, MOMENTUM NO SKIP ################################
Monthly <-read_excel("C:/Users/talga/Desktop/Masters/R/MyScripts/RA Ship and Thesis/ULTRA CANONICAL ALL DIFFERENT/Monthly 1964-2012.xlsx")
momentum_signal <-read_excel("C:/Users/talga/Desktop/Masters/R/MyScripts/RA Ship and Thesis/ULTRA CANONICAL ALL DIFFERENT/RET SIGNAL NO SKIP 1964-2012.xlsx")
Monthly=xts(Monthly[, -1], order.by = Monthly$index)
momentum_signal=xts(momentum_signal[, -1], order.by = momentum_signal$index)
FF=read_excel("C:/Users/talga/Desktop/Masters/R/MyScripts/RA Ship and Thesis/ULTRA CANONICAL ALL DIFFERENT/FF 1964-2012.xlsx")
FF$Date=ymd(FF$Date)
FF=xts(FF[,-1], order.by = FF$Date)

##################### RET (3,3) SKIP ##############################################
rank_signal=t(apply(momentum_signal, 1, rank, ties.method = "first", na.last = "keep"))

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

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Port=list(one, two, three, four, five, six, seven, eight, nine, ten)
WML=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (j in 1:10) {
  for (i in 1:571) {
    Port[[j]][[i]]=rbind(Monthly[i+1,]*xts(Signals[[j]][i,], order.by = index(Monthly)[i+1]),
                         Monthly[i+2,]*xts(Signals[[j]][i,], order.by = index(Monthly)[i+2]),
                         Monthly[i+3,]*xts(Signals[[j]][i,], order.by = index(Monthly)[i+3]))
    
    
    WML[[j]][[i]]=xts(apply(Port[[j]][[i]], 1, sum)/length(which(Port[[j]][[i]][1,]!=0)),
                      order.by = index(Port[[j]][[i]]))
  }
}
rm(Signals); rm(Port)
Overlapped=list()
for (i in 1:10) {
  Overlapped[[i]]=do.call(merge, WML[[i]])
  Overlapped[[i]][is.na(Overlapped[[i]])]=0
  colnames(Overlapped[[i]])=c(1:571)
}

Final=list()
for (i in 1:10) {
  Final[[i]]=xts(apply(Overlapped[[i]], 1, sum)/3, order.by = index(Overlapped[[i]]))
}

sum(Final[[10]])
sum(FF$MktxRF)
sum(FF$RF)

colnames(Final[[1]])=c("V1")
time(Final[[1]])<- time(Final[[1]]) %>%
  as.yearmon() %>% as.Date()
Final[[1]]=merge(Final[[1]], FF)
Final[[1]]$V1=Final[[1]]$V1-Final[[1]]$RF
fit1=lm(V1~MktxRF+SMB+HML,data = Final[[1]])
vcovHAC_NW1=vcovHAC(fit1, weights = bwNeweyWest)
coeftest1=coeftest(fit1, vcov. = vcovHAC_NW1)
coeftest1
  
colnames(Final[[10]])=c("V1")
time(Final[[10]])<- time(Final[[10]]) %>%
  as.yearmon() %>% as.Date()
Final[[10]]=merge(Final[[10]], FF)
Final[[10]]$V1=Final[[10]]$V1-Final[[10]]$RF
fit10=lm(V1~MktxRF+SMB+HML,data = Final[[10]])
vcovHAC_NW10=vcovHAC(fit10, weights = bwNeweyWest)
coeftest10=coeftest(fit10, vcov. = vcovHAC_NW1)
coeftest10

Ten_Minus_1 = Final[[10]]-Final[[1]]
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%
  as.yearmon() %>% as.Date()
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
Ten_Minus_1$RF=FF$RF
Ten_Minus_1$V1=Ten_Minus_1$V1-Ten_Minus_1$RF
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest

