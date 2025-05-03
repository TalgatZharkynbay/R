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
Monthly <-read_excel("C:/Users/talga/Desktop/Thesis/Super Canon/RET.xlsx")
momentum_signal <-read_excel("C:/Users/talga/Desktop/Thesis/Super Canon/RET_SIGNAL NO SKIP.xlsx")
VRY<-read_excel("C:/Users/talga/Desktop/Thesis/Super Canon/VRY_SIGNAL.xlsx")
VOL<-read_excel("C:/Users/talga/Desktop/Thesis/Super Canon/VOL_SIGNAL.xlsx")
Monthly=xts(Monthly[, -1], order.by = Monthly$index)
momentum_signal=xts(momentum_signal[, -1], order.by = momentum_signal$index)
VRY=xts(VRY[, -1], order.by = VRY$index)
VOL=xts(VOL[, -1], order.by = VOL$index)
FF=read_excel("C:/Users/talga/Desktop/Thesis/FF_Monthly_Good.xlsx")
FF$Date=ymd(FF$Date)
FF=xts(FF[,-1], order.by = FF$Date)
FF=FF["2012-05-01/2019-12-01"]

##################### RET (3,3) SKIP ################################
rm(VOL, VRY)
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

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Port=list(one, two, three, four, five, six, seven, eight, nine, ten)
WML=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (j in 1:10) {
  for (i in 1:90) {
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
  colnames(Overlapped[[i]])=c(1:90)
}

Final=list()
for (i in 1:10) {
  Final[[i]]=xts(apply(Overlapped[[i]], 1, sum)/3, order.by = index(Overlapped[[i]]))
}

fit=list()
vcovHAC_NW=list()
coeftest=list()
for (i in 1:10) {
  colnames(Final[[i]])=c("V1")
  time(Final[[i]])<- time(Final[[i]]) %>%
    as.yearmon() %>% as.Date()
  Final[[i]]=merge(Final[[i]], FF)
  Final[[i]]$V1=Final[[i]]$V1-Final[[i]]$RF
  fit[[i]]=lm(V1~MktxRF+SMB+HML,
              data = Final[[i]])
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

Ten_Minus_1 = Final[[10]]-Final[[1]]
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%
  as.yearmon() %>% as.Date()
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest


##################### VOL (3,3) SKIP ################################
rm(momentum_signal, VRY)
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

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Port=list(one, two, three, four, five, six, seven, eight, nine, ten)
WML=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (j in 1:10) {
  for (i in 1:90) {
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
  colnames(Overlapped[[i]])=c(1:90)
}

Final=list()
for (i in 1:10) {
  Final[[i]]=xts(apply(Overlapped[[i]], 1, sum)/3, order.by = index(Overlapped[[i]]))
}

fit=list()
vcovHAC_NW=list()
coeftest=list()
for (i in 1:10) {
  colnames(Final[[i]])=c("V1")
  time(Final[[i]])<- time(Final[[i]]) %>%
    as.yearmon() %>% as.Date()
  Final[[i]]=merge(Final[[i]], FF)
  Final[[i]]$V1=Final[[i]]$V1-Final[[i]]$RF
  fit[[i]]=lm(V1~MktxRF+SMB+HML,
              data = Final[[i]])
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

Ten_Minus_1 = Final[[10]]-Final[[1]]
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%
  as.yearmon() %>% as.Date()
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest

##################### VRY (3,3) SKIP ################################
rm(momentum_signal, VOL)
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

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Port=list(one, two, three, four, five, six, seven, eight, nine, ten)
WML=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (j in 1:10) {
  for (i in 1:90) {
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
  colnames(Overlapped[[i]])=c(1:90)
}

Final=list()
for (i in 1:10) {
  Final[[i]]=xts(apply(Overlapped[[i]], 1, sum)/3, order.by = index(Overlapped[[i]]))
}

fit=list()
vcovHAC_NW=list()
coeftest=list()
for (i in 1:10) {
  colnames(Final[[i]])=c("V1")
  time(Final[[i]])<- time(Final[[i]]) %>%
    as.yearmon() %>% as.Date()
  Final[[i]]=merge(Final[[i]], FF)
  Final[[i]]$V1=Final[[i]]$V1-Final[[i]]$RF
  fit[[i]]=lm(V1~MktxRF+SMB+HML,
              data = Final[[i]])
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

Ten_Minus_1 = Final[[10]]-Final[[1]]
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%
  as.yearmon() %>% as.Date()
Ten_Minus_1$MktxRF=FF$MktxRF; Ten_Minus_1$SMB=FF$SMB; Ten_Minus_1$HML=FF$HML
fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest=coeftest(fit, vcov. = vcovHAC_NW)
coeftest