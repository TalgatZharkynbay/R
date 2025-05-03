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

##################### RET and VOL (3,3) SKIP ################################
rm(VRY)
rank_signal=t(apply(momentum_signal, 1, rank))
for (i in 1: nrow(rank_signal)) {
  rank_signal[i,]=decile(rank_signal[i,], decreasing = FALSE) #biggest values in decile 10   
}

rank_signal=xts(rank_signal, order.by = as.Date(rownames(rank_signal)))
rm(momentum_signal)
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
Choice=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (i in 1:10) {
  for (j in 1:93) { 
    Choice[[i]][[j]]=colnames(Signals[[i]][j,which(Signals[[i]][j,]==1)])
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (i in 1:10) {
  for (j in 1:93) {
    Choice_N[[i]][[j]]=VOL[, colnames(VOL) %in% Choice[[i]][[j]]][j,]
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N_SIGNAL=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (i in 1:10) {
  for (j in 1:93) {#used to be 27
    Choice_N_SIGNAL[[i]][[j]]=t(apply(Choice_N[[i]][[j]], 1, rank))
    Choice_N_SIGNAL[[i]][[j]]=t(ntile(Choice_N_SIGNAL[[i]][[j]], 3))
    colnames(Choice_N_SIGNAL[[i]][[j]])=colnames(Choice_N[[i]][[j]])
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
First_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Second_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Third_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Choice_N_SIGNAL_FINAL=list(First_Tercile,Second_Tercile,Third_Tercile)
rm(one, two, three, four, five, six, seven, eight, nine, ten,First_Tercile,
   Second_Tercile,Third_Tercile)

for (i in 1:3) {
  for (j in 1:10) {
    for (k in 1:93) {
      Choice_N_SIGNAL_FINAL[[i]][[j]][[k]]=ifelse(Choice_N_SIGNAL[[j]][[k]]==i, 1, 0)
    }
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
First_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Second_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Third_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Monthly_Second=list(First_Tercile,Second_Tercile,Third_Tercile)
rm(one, two, three, four, five, six, seven, eight, nine, ten,First_Tercile,
   Second_Tercile,Third_Tercile)

for (i in 1:3) {
  for (j in 1:10) {
    for (k in 1:93) {
      Monthly_Second[[i]][[j]][[k]]=Monthly[, colnames(Monthly) %in%
                                              colnames(Choice_N_SIGNAL_FINAL[[i]][[j]][[k]])]
    }
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
First_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Second_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Third_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Port=list(First_Tercile,Second_Tercile,Third_Tercile)
rm(one, two, three, four, five, six, seven, eight, nine, ten,First_Tercile,
   Second_Tercile,Third_Tercile)

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
First_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Second_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Third_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
WML=list(First_Tercile,Second_Tercile,Third_Tercile)
rm(one, two, three, four, five, six, seven, eight, nine, ten,First_Tercile,
   Second_Tercile,Third_Tercile)
rm(Signals, Choice, Choice_N, Choice_N_SIGNAL, VOL, Monthly)

for (i in 1:3) {
  for (j in 1:10) {
    for (z in 1:90) {
      
      Port[[i]][[j]][[z]]=rbind(Monthly_Second[[i]][[j]][[z]][z+1,]*xts(Choice_N_SIGNAL_FINAL[[i]][[j]][[z]], 
                                                                        order.by = 
                                                                          index(Monthly_Second[[i]][[j]][[z]])[z+1]),
                                
                                Monthly_Second[[i]][[j]][[z]][z+2,]*xts(Choice_N_SIGNAL_FINAL[[i]][[j]][[z]], 
                                                                        order.by = 
                                                                          index(Monthly_Second[[i]][[j]][[z]])[z+2]),
                                
                                Monthly_Second[[i]][[j]][[z]][z+3,]*xts(Choice_N_SIGNAL_FINAL[[i]][[j]][[z]], 
                                                                        order.by = 
                                                                          index(Monthly_Second[[i]][[j]][[z]])[z+3]))
      
      
      WML[[i]][[j]][[z]]=xts(apply(Port[[i]][[j]][[z]], 1, sum)/
                               length(which(Port[[i]][[j]][[z]][1,]!=0)),
                             order.by = index(Port[[i]][[j]][[z]]))
    }
  }
}
rm(Monthly_Second, Port, Choice_N_SIGNAL_FINAL)

one=list(); two=list(); three=list(); 
Overlapped=list(one,two,three)
rm(one, two, three)

for (i in 1:3) {
  for (j in 1:10) {
    Overlapped[[i]][[j]]=do.call(merge, WML[[i]][[j]])
    Overlapped[[i]][[j]][is.na(Overlapped[[i]][[j]])]=0
    colnames(Overlapped[[i]][[j]])=c(1:90)
  }
}

one=list(); two=list(); three=list(); 
Final=list(one,two,three)
rm(one, two, three)

for (i in 1:3) {
  for (j in 1:10) {
    Final[[i]][[j]]=xts(apply(Overlapped[[i]][[j]], 1, sum)/3, 
                        order.by = index(Overlapped[[i]][[j]]))
  }
}

First_fit=list()
First_vcovHAC_NW=list()
First_coeftest=list()
Second_fit=list()
Second_vcovHAC_NW=list()
Second_coeftest=list()
Third_fit=list()
Third_vcovHAC_NW=list()
Third_coeftest=list()
WML=list()
First_Alpha=vector()
First_t_stat=vector()
Second_Alpha=vector()
Second_t_stat=vector()
Third_Alpha=vector()
Third_t_stat=vector()
WML_Alphas=vector()
WML_tstats=vector()

for (i in 1:10) {
  WML[[i]]=Final[[1]][[i]]
  colnames(WML[[i]])=c("V1")
  time(WML[[i]])<- time(WML[[i]]) %>%as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  First_fit[[i]]=lm(V1~MktxRF+SMB+HML,data = WML[[i]])
  First_vcovHAC_NW[[i]]=vcovHAC(First_fit[[i]], weights = bwNeweyWest)
  First_coeftest[[i]]=coeftest(First_fit[[i]], vcov. = First_vcovHAC_NW[[i]])
}  

for (i in 1:10) {
  First_Alpha[i]=First_coeftest[[i]][1,1]  
}
First_Alpha=as.data.frame(First_Alpha)
First_Alpha$First_Alpha=First_Alpha$First_Alpha*12
for (i in 1:10) {
  First_t_stat[i]=First_coeftest[[i]][1,3]  
}
First_t_stat=as.data.frame(First_t_stat)


for (i in 1:10) {
  WML[[i]]=Final[[2]][[i]]
  colnames(WML[[i]])=c("V1")
  time(WML[[i]])<- time(WML[[i]]) %>%as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  Second_fit[[i]]=lm(V1~MktxRF+SMB+HML,data = WML[[i]])
  Second_vcovHAC_NW[[i]]=vcovHAC(Second_fit[[i]], weights = bwNeweyWest)
  Second_coeftest[[i]]=coeftest(Second_fit[[i]], vcov. = Second_vcovHAC_NW[[i]])
}  

for (i in 1:10) {
  Second_Alpha[i]=Second_coeftest[[i]][1,1]  
}
Second_Alpha=as.data.frame(Second_Alpha)
Second_Alpha$Second_Alpha=Second_Alpha$Second_Alpha*12
for (i in 1:10) {
  Second_t_stat[i]=Second_coeftest[[i]][1,3]  
}
Second_t_stat=as.data.frame(Second_t_stat)


for (i in 1:10) {
  WML[[i]]=Final[[3]][[i]]
  colnames(WML[[i]])=c("V1")
  time(WML[[i]])<- time(WML[[i]]) %>%as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  Third_fit[[i]]=lm(V1~MktxRF+SMB+HML,data = WML[[i]])
  Third_vcovHAC_NW[[i]]=vcovHAC(Third_fit[[i]], weights = bwNeweyWest)
  Third_coeftest[[i]]=coeftest(Third_fit[[i]], vcov. = Third_vcovHAC_NW[[i]])
}  

for (i in 1:10) {
  Third_Alpha[i]=Third_coeftest[[i]][1,1]  
}
Third_Alpha=as.data.frame(Third_Alpha)
Third_Alpha$Third_Alpha=Third_Alpha$Third_Alpha*12
for (i in 1:10) {
  Third_t_stat[i]=Third_coeftest[[i]][1,3]  
}
Third_t_stat=as.data.frame(Third_t_stat)

Ten_Minus_1=Final[[1]][[10]]-Final[[1]][[1]] 
colnames(Ten_Minus_1)=c("V1")
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%as.yearmon() %>% as.Date()
Ten_Minus_1=merge(Ten_Minus_1, FF)
Ten_Minus_1$V1=Ten_Minus_1$V1-Ten_Minus_1$RF
Ten_Minus_1_fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
Ten_Minus_1_vcovHAC_NW=vcovHAC(Ten_Minus_1_fit, weights = bwNeweyWest)
Ten_Minus_1_coeftest=coeftest(Ten_Minus_1_fit, vcov. = Ten_Minus_1_vcovHAC_NW)

Twenty_Minus_11=Final[[2]][[10]]-Final[[2]][[1]]
colnames(Twenty_Minus_11)=c("V1")
time(Twenty_Minus_11)<- time(Twenty_Minus_11) %>%as.yearmon() %>% as.Date()
Twenty_Minus_11=merge(Twenty_Minus_11, FF)
Twenty_Minus_11$V1=Twenty_Minus_11$V1-Twenty_Minus_11$RF
Twenty_Minus_11_fit=lm(V1~MktxRF+SMB+HML,data = Twenty_Minus_11)
Twenty_Minus_11_vcovHAC_NW=vcovHAC(Twenty_Minus_11_fit, weights = bwNeweyWest)
Twenty_Minus_11_coeftest=coeftest(Twenty_Minus_11_fit, vcov. = Twenty_Minus_11_vcovHAC_NW)

Thirty_Minus_21=Final[[3]][[10]]-Final[[3]][[1]]
colnames(Thirty_Minus_21)=c("V1")
time(Thirty_Minus_21)<- time(Thirty_Minus_21) %>%as.yearmon() %>% as.Date()
Thirty_Minus_21=merge(Thirty_Minus_21, FF)
Thirty_Minus_21$V1=Thirty_Minus_21$V1-Thirty_Minus_21$RF
Thirty_Minus_21_fit=lm(V1~MktxRF+SMB+HML,data = Thirty_Minus_21)
Thirty_Minus_21_vcovHAC_NW=vcovHAC(Thirty_Minus_21_fit, weights = bwNeweyWest)
Thirty_Minus_21_coeftest=coeftest(Thirty_Minus_21_fit, vcov. = Thirty_Minus_21_vcovHAC_NW)

WML_Alphas=c(Ten_Minus_1_coeftest[1,1], Twenty_Minus_11_coeftest[1,1],
             Thirty_Minus_21_coeftest[1,1])

WML_tstats=c(Ten_Minus_1_coeftest[1,3], Twenty_Minus_11_coeftest[1,3],
             Thirty_Minus_21_coeftest[1,3])

view(First_Alpha)
view(First_t_stat)
view(Second_Alpha)
view(Second_t_stat) 
view(Third_Alpha)
view(Third_t_stat) 
view(WML_Alphas)
view(WML_tstats)

##################### RET and VRY (3,3) SKIP ################################
rm(VOL)
rank_signal=t(apply(momentum_signal, 1, rank))
for (i in 1: nrow(rank_signal)) {
  rank_signal[i,]=decile(rank_signal[i,], decreasing = FALSE) #biggest values in decile 10   
}

rank_signal=xts(rank_signal, order.by = as.Date(rownames(rank_signal)))
rm(momentum_signal)
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
Choice=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (i in 1:10) {
  for (j in 1:93) { 
    Choice[[i]][[j]]=colnames(Signals[[i]][j,which(Signals[[i]][j,]==1)])
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (i in 1:10) {
  for (j in 1:93) {
    Choice_N[[i]][[j]]=VRY[, colnames(VRY) %in% Choice[[i]][[j]]][j,]
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N_SIGNAL=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)

for (i in 1:10) {
  for (j in 1:93) {#used to be 27
    Choice_N_SIGNAL[[i]][[j]]=t(apply(Choice_N[[i]][[j]], 1, rank))
    Choice_N_SIGNAL[[i]][[j]]=t(ntile(Choice_N_SIGNAL[[i]][[j]], 3))
    colnames(Choice_N_SIGNAL[[i]][[j]])=colnames(Choice_N[[i]][[j]])
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
First_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Second_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Third_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Choice_N_SIGNAL_FINAL=list(First_Tercile,Second_Tercile,Third_Tercile)
rm(one, two, three, four, five, six, seven, eight, nine, ten,First_Tercile,
   Second_Tercile,Third_Tercile)

for (i in 1:3) {
  for (j in 1:10) {
    for (k in 1:93) {
      Choice_N_SIGNAL_FINAL[[i]][[j]][[k]]=ifelse(Choice_N_SIGNAL[[j]][[k]]==i, 1, 0)
    }
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
First_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Second_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Third_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Monthly_Second=list(First_Tercile,Second_Tercile,Third_Tercile)
rm(one, two, three, four, five, six, seven, eight, nine, ten,First_Tercile,
   Second_Tercile,Third_Tercile)

for (i in 1:3) {
  for (j in 1:10) {
    for (k in 1:93) {
      Monthly_Second[[i]][[j]][[k]]=Monthly[, colnames(Monthly) %in%
                                              colnames(Choice_N_SIGNAL_FINAL[[i]][[j]][[k]])]
    }
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
First_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Second_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Third_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Port=list(First_Tercile,Second_Tercile,Third_Tercile)
rm(one, two, three, four, five, six, seven, eight, nine, ten,First_Tercile,
   Second_Tercile,Third_Tercile)

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
First_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Second_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
Third_Tercile=list(one, two, three, four, five, six, seven, eight, nine, ten)
WML=list(First_Tercile,Second_Tercile,Third_Tercile)
rm(one, two, three, four, five, six, seven, eight, nine, ten,First_Tercile,
   Second_Tercile,Third_Tercile)
rm(Signals, Choice, Choice_N, Choice_N_SIGNAL, VOL, Monthly)

for (i in 1:3) {
  for (j in 1:10) {
    for (z in 1:90) {
      
      Port[[i]][[j]][[z]]=rbind(Monthly_Second[[i]][[j]][[z]][z+1,]*xts(Choice_N_SIGNAL_FINAL[[i]][[j]][[z]], 
                                                                        order.by = 
                                                                          index(Monthly_Second[[i]][[j]][[z]])[z+1]),
                                
                                Monthly_Second[[i]][[j]][[z]][z+2,]*xts(Choice_N_SIGNAL_FINAL[[i]][[j]][[z]], 
                                                                        order.by = 
                                                                          index(Monthly_Second[[i]][[j]][[z]])[z+2]),
                                
                                Monthly_Second[[i]][[j]][[z]][z+3,]*xts(Choice_N_SIGNAL_FINAL[[i]][[j]][[z]], 
                                                                        order.by = 
                                                                          index(Monthly_Second[[i]][[j]][[z]])[z+3]))
      
      
      WML[[i]][[j]][[z]]=xts(apply(Port[[i]][[j]][[z]], 1, sum)/
                               length(which(Port[[i]][[j]][[z]][1,]!=0)),
                             order.by = index(Port[[i]][[j]][[z]]))
    }
  }
}
rm(Monthly_Second, Port, Choice_N_SIGNAL_FINAL)

one=list(); two=list(); three=list(); 
Overlapped=list(one,two,three)
rm(one, two, three)

for (i in 1:3) {
  for (j in 1:10) {
    Overlapped[[i]][[j]]=do.call(merge, WML[[i]][[j]])
    Overlapped[[i]][[j]][is.na(Overlapped[[i]][[j]])]=0
    colnames(Overlapped[[i]][[j]])=c(1:90)
  }
}

one=list(); two=list(); three=list(); 
Final=list(one,two,three)
rm(one, two, three)

for (i in 1:3) {
  for (j in 1:10) {
    Final[[i]][[j]]=xts(apply(Overlapped[[i]][[j]], 1, sum)/3, 
                        order.by = index(Overlapped[[i]][[j]]))
  }
}

First_fit=list()
First_vcovHAC_NW=list()
First_coeftest=list()
Second_fit=list()
Second_vcovHAC_NW=list()
Second_coeftest=list()
Third_fit=list()
Third_vcovHAC_NW=list()
Third_coeftest=list()
WML=list()
First_Alpha=vector()
First_t_stat=vector()
Second_Alpha=vector()
Second_t_stat=vector()
Third_Alpha=vector()
Third_t_stat=vector()
WML_Alphas=vector()
WML_tstats=vector()

for (i in 1:10) {
  WML[[i]]=Final[[1]][[i]]
  colnames(WML[[i]])=c("V1")
  time(WML[[i]])<- time(WML[[i]]) %>%as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  First_fit[[i]]=lm(V1~MktxRF+SMB+HML,data = WML[[i]])
  First_vcovHAC_NW[[i]]=vcovHAC(First_fit[[i]], weights = bwNeweyWest)
  First_coeftest[[i]]=coeftest(First_fit[[i]], vcov. = First_vcovHAC_NW[[i]])
}  

for (i in 1:10) {
  First_Alpha[i]=First_coeftest[[i]][1,1]  
}
First_Alpha=as.data.frame(First_Alpha)
First_Alpha$First_Alpha=First_Alpha$First_Alpha*12
for (i in 1:10) {
  First_t_stat[i]=First_coeftest[[i]][1,3]  
}
First_t_stat=as.data.frame(First_t_stat)


for (i in 1:10) {
  WML[[i]]=Final[[2]][[i]]
  colnames(WML[[i]])=c("V1")
  time(WML[[i]])<- time(WML[[i]]) %>%as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  Second_fit[[i]]=lm(V1~MktxRF+SMB+HML,data = WML[[i]])
  Second_vcovHAC_NW[[i]]=vcovHAC(Second_fit[[i]], weights = bwNeweyWest)
  Second_coeftest[[i]]=coeftest(Second_fit[[i]], vcov. = Second_vcovHAC_NW[[i]])
}  

for (i in 1:10) {
  Second_Alpha[i]=Second_coeftest[[i]][1,1]  
}
Second_Alpha=as.data.frame(Second_Alpha)
Second_Alpha$Second_Alpha=Second_Alpha$Second_Alpha*12
for (i in 1:10) {
  Second_t_stat[i]=Second_coeftest[[i]][1,3]  
}
Second_t_stat=as.data.frame(Second_t_stat)


for (i in 1:10) {
  WML[[i]]=Final[[3]][[i]]
  colnames(WML[[i]])=c("V1")
  time(WML[[i]])<- time(WML[[i]]) %>%as.yearmon() %>% as.Date()
  WML[[i]]=merge(WML[[i]], FF)
  WML[[i]]$V1=WML[[i]]$V1-WML[[i]]$RF
  Third_fit[[i]]=lm(V1~MktxRF+SMB+HML,data = WML[[i]])
  Third_vcovHAC_NW[[i]]=vcovHAC(Third_fit[[i]], weights = bwNeweyWest)
  Third_coeftest[[i]]=coeftest(Third_fit[[i]], vcov. = Third_vcovHAC_NW[[i]])
}  

for (i in 1:10) {
  Third_Alpha[i]=Third_coeftest[[i]][1,1]  
}
Third_Alpha=as.data.frame(Third_Alpha)
Third_Alpha$Third_Alpha=Third_Alpha$Third_Alpha*12
for (i in 1:10) {
  Third_t_stat[i]=Third_coeftest[[i]][1,3]  
}
Third_t_stat=as.data.frame(Third_t_stat)

Ten_Minus_1=Final[[1]][[10]]-Final[[1]][[1]] 
colnames(Ten_Minus_1)=c("V1")
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%as.yearmon() %>% as.Date()
Ten_Minus_1=merge(Ten_Minus_1, FF)
Ten_Minus_1$V1=Ten_Minus_1$V1-Ten_Minus_1$RF
Ten_Minus_1_fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
Ten_Minus_1_vcovHAC_NW=vcovHAC(Ten_Minus_1_fit, weights = bwNeweyWest)
Ten_Minus_1_coeftest=coeftest(Ten_Minus_1_fit, vcov. = Ten_Minus_1_vcovHAC_NW)

Twenty_Minus_11=Final[[2]][[10]]-Final[[2]][[1]]
colnames(Twenty_Minus_11)=c("V1")
time(Twenty_Minus_11)<- time(Twenty_Minus_11) %>%as.yearmon() %>% as.Date()
Twenty_Minus_11=merge(Twenty_Minus_11, FF)
Twenty_Minus_11$V1=Twenty_Minus_11$V1-Twenty_Minus_11$RF
Twenty_Minus_11_fit=lm(V1~MktxRF+SMB+HML,data = Twenty_Minus_11)
Twenty_Minus_11_vcovHAC_NW=vcovHAC(Twenty_Minus_11_fit, weights = bwNeweyWest)
Twenty_Minus_11_coeftest=coeftest(Twenty_Minus_11_fit, vcov. = Twenty_Minus_11_vcovHAC_NW)

Thirty_Minus_21=Final[[3]][[10]]-Final[[3]][[1]]
colnames(Thirty_Minus_21)=c("V1")
time(Thirty_Minus_21)<- time(Thirty_Minus_21) %>%as.yearmon() %>% as.Date()
Thirty_Minus_21=merge(Thirty_Minus_21, FF)
Thirty_Minus_21$V1=Thirty_Minus_21$V1-Thirty_Minus_21$RF
Thirty_Minus_21_fit=lm(V1~MktxRF+SMB+HML,data = Thirty_Minus_21)
Thirty_Minus_21_vcovHAC_NW=vcovHAC(Thirty_Minus_21_fit, weights = bwNeweyWest)
Thirty_Minus_21_coeftest=coeftest(Thirty_Minus_21_fit, vcov. = Thirty_Minus_21_vcovHAC_NW)

WML_Alphas=c(Ten_Minus_1_coeftest[1,1], Twenty_Minus_11_coeftest[1,1],
             Thirty_Minus_21_coeftest[1,1])

WML_tstats=c(Ten_Minus_1_coeftest[1,3], Twenty_Minus_11_coeftest[1,3],
             Thirty_Minus_21_coeftest[1,3])

view(First_Alpha)
view(First_t_stat)
view(Second_Alpha)
view(Second_t_stat) 
view(Third_Alpha)
view(Third_t_stat) 
view(WML_Alphas)
view(WML_tstats)

