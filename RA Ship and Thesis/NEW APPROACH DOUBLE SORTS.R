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
##################### RET AND NUM ################################
rm(VOL,VRY, F_SIGNAL)
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
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
    
  Choice[[i]][[j]]=colnames(Signals[[i]][Seq[j],which(Signals[[i]][Seq[j],]==1)])
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
    Choice_N[[i]][[j]]=N_SIGNAL[, colnames(N_SIGNAL) %in% Choice[[i]][[j]]][Seq[j],]
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N_SIGNAL=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
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
    for (k in 1:27) {
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
    for (k in 1:27) {
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

Seq=rep(seq(1,27,1), each=3)
Seq=Seq[-81]

for (i in 1:3) {
  for (j in 1:10) {
      for (z in 1:80) {#used to be 81, and not z+1
        
      Port[[i]][[j]][z]=apply(Choice_N_SIGNAL_FINAL[[i]][[j]][[Seq[z]]]*
                           Monthly_Second[[i]][[j]][[Seq[z]]][z+1,], 1, sum)/
        sum(Choice_N_SIGNAL_FINAL[[i]][[j]][[Seq[z]]])
    }
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
  WML[[i]]=xts(unlist(Port[[1]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
  WML[[i]]=xts(unlist(Port[[2]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
  WML[[i]]=xts(unlist(Port[[3]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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

Ten_Minus_1=xts(unlist(Port[[1]][[10]])-unlist(Port[[1]][[1]]), 
                order.by = index(Monthly)[1:last(seq_along(Seq))+1])
colnames(Ten_Minus_1)=c("V1")
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%as.yearmon() %>% as.Date()
Ten_Minus_1=merge(Ten_Minus_1, FF)
Ten_Minus_1$V1=Ten_Minus_1$V1-Ten_Minus_1$RF
Ten_Minus_1_fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
Ten_Minus_1_vcovHAC_NW=vcovHAC(Ten_Minus_1_fit, weights = bwNeweyWest)
Ten_Minus_1_coeftest=coeftest(Ten_Minus_1_fit, vcov. = Ten_Minus_1_vcovHAC_NW)

Twenty_Minus_11=xts(unlist(Port[[2]][[10]])-unlist(Port[[2]][[1]]), 
                    order.by = index(Monthly)[1:last(seq_along(Seq))+1])
colnames(Twenty_Minus_11)=c("V1")
time(Twenty_Minus_11)<- time(Twenty_Minus_11) %>%as.yearmon() %>% as.Date()
Twenty_Minus_11=merge(Twenty_Minus_11, FF)
Twenty_Minus_11$V1=Twenty_Minus_11$V1-Twenty_Minus_11$RF
Twenty_Minus_11_fit=lm(V1~MktxRF+SMB+HML,data = Twenty_Minus_11)
Twenty_Minus_11_vcovHAC_NW=vcovHAC(Twenty_Minus_11_fit, weights = bwNeweyWest)
Twenty_Minus_11_coeftest=coeftest(Twenty_Minus_11_fit, vcov. = Twenty_Minus_11_vcovHAC_NW)

Thirty_Minus_21=xts(unlist(Port[[3]][[10]])-unlist(Port[[3]][[1]]),
                    order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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

##################### RET AND FRA ################################
rm(VOL,VRY, N_SIGNAL)
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
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
    
    Choice[[i]][[j]]=colnames(Signals[[i]][Seq[j],which(Signals[[i]][Seq[j],]==1)])
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
    Choice_N[[i]][[j]]=F_SIGNAL[, colnames(F_SIGNAL) %in% Choice[[i]][[j]]][Seq[j],]
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N_SIGNAL=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
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
    for (k in 1:27) {
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
    for (k in 1:27) {
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

Seq=rep(seq(1,27,1), each=3)
Seq=Seq[-81]

for (i in 1:3) {
  for (j in 1:10) {
    for (z in 1:80) {#used to be 81, and not z+1
      
      Port[[i]][[j]][z]=apply(Choice_N_SIGNAL_FINAL[[i]][[j]][[Seq[z]]]*
                                Monthly_Second[[i]][[j]][[Seq[z]]][z+1,], 1, sum)/
        sum(Choice_N_SIGNAL_FINAL[[i]][[j]][[Seq[z]]])
    }
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
  WML[[i]]=xts(unlist(Port[[1]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
  WML[[i]]=xts(unlist(Port[[2]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
  WML[[i]]=xts(unlist(Port[[3]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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

Ten_Minus_1=xts(unlist(Port[[1]][[10]])-unlist(Port[[1]][[1]]), 
                order.by = index(Monthly)[1:last(seq_along(Seq))+1])
colnames(Ten_Minus_1)=c("V1")
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%as.yearmon() %>% as.Date()
Ten_Minus_1=merge(Ten_Minus_1, FF)
Ten_Minus_1$V1=Ten_Minus_1$V1-Ten_Minus_1$RF
Ten_Minus_1_fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
Ten_Minus_1_vcovHAC_NW=vcovHAC(Ten_Minus_1_fit, weights = bwNeweyWest)
Ten_Minus_1_coeftest=coeftest(Ten_Minus_1_fit, vcov. = Ten_Minus_1_vcovHAC_NW)

Twenty_Minus_11=xts(unlist(Port[[2]][[10]])-unlist(Port[[2]][[1]]), 
                    order.by = index(Monthly)[1:last(seq_along(Seq))+1])
colnames(Twenty_Minus_11)=c("V1")
time(Twenty_Minus_11)<- time(Twenty_Minus_11) %>%as.yearmon() %>% as.Date()
Twenty_Minus_11=merge(Twenty_Minus_11, FF)
Twenty_Minus_11$V1=Twenty_Minus_11$V1-Twenty_Minus_11$RF
Twenty_Minus_11_fit=lm(V1~MktxRF+SMB+HML,data = Twenty_Minus_11)
Twenty_Minus_11_vcovHAC_NW=vcovHAC(Twenty_Minus_11_fit, weights = bwNeweyWest)
Twenty_Minus_11_coeftest=coeftest(Twenty_Minus_11_fit, vcov. = Twenty_Minus_11_vcovHAC_NW)

Thirty_Minus_21=xts(unlist(Port[[3]][[10]])-unlist(Port[[3]][[1]]),
                    order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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

##################### RET AND VOL ################################
rm(F_SIGNAL,VRY, N_SIGNAL)
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
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
    
    Choice[[i]][[j]]=colnames(Signals[[i]][Seq[j],which(Signals[[i]][Seq[j],]==1)])
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
    Choice_N[[i]][[j]]=VOL[, colnames(VOL) %in% Choice[[i]][[j]]][Seq[j],]
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N_SIGNAL=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
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
    for (k in 1:27) {
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
    for (k in 1:27) {
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

Seq=rep(seq(1,27,1), each=3)
Seq=Seq[-81]

for (i in 1:3) {
  for (j in 1:10) {
    for (z in 1:80) {#used to be 81, and not z+1
      
      Port[[i]][[j]][z]=apply(Choice_N_SIGNAL_FINAL[[i]][[j]][[Seq[z]]]*
                                Monthly_Second[[i]][[j]][[Seq[z]]][z+1,], 1, sum)/
        sum(Choice_N_SIGNAL_FINAL[[i]][[j]][[Seq[z]]])
    }
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
  WML[[i]]=xts(unlist(Port[[1]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
  WML[[i]]=xts(unlist(Port[[2]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
  WML[[i]]=xts(unlist(Port[[3]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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

Ten_Minus_1=xts(unlist(Port[[1]][[10]])-unlist(Port[[1]][[1]]), 
                order.by = index(Monthly)[1:last(seq_along(Seq))+1])
colnames(Ten_Minus_1)=c("V1")
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%as.yearmon() %>% as.Date()
Ten_Minus_1=merge(Ten_Minus_1, FF)
Ten_Minus_1$V1=Ten_Minus_1$V1-Ten_Minus_1$RF
Ten_Minus_1_fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
Ten_Minus_1_vcovHAC_NW=vcovHAC(Ten_Minus_1_fit, weights = bwNeweyWest)
Ten_Minus_1_coeftest=coeftest(Ten_Minus_1_fit, vcov. = Ten_Minus_1_vcovHAC_NW)

Twenty_Minus_11=xts(unlist(Port[[2]][[10]])-unlist(Port[[2]][[1]]), 
                    order.by = index(Monthly)[1:last(seq_along(Seq))+1])
colnames(Twenty_Minus_11)=c("V1")
time(Twenty_Minus_11)<- time(Twenty_Minus_11) %>%as.yearmon() %>% as.Date()
Twenty_Minus_11=merge(Twenty_Minus_11, FF)
Twenty_Minus_11$V1=Twenty_Minus_11$V1-Twenty_Minus_11$RF
Twenty_Minus_11_fit=lm(V1~MktxRF+SMB+HML,data = Twenty_Minus_11)
Twenty_Minus_11_vcovHAC_NW=vcovHAC(Twenty_Minus_11_fit, weights = bwNeweyWest)
Twenty_Minus_11_coeftest=coeftest(Twenty_Minus_11_fit, vcov. = Twenty_Minus_11_vcovHAC_NW)

Thirty_Minus_21=xts(unlist(Port[[3]][[10]])-unlist(Port[[3]][[1]]),
                    order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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

##################### RET AND VRY ################################
rm(F_SIGNAL,VOL, N_SIGNAL)
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
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
    
    Choice[[i]][[j]]=colnames(Signals[[i]][Seq[j],which(Signals[[i]][Seq[j],]==1)])
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
    Choice_N[[i]][[j]]=VRY[, colnames(VRY) %in% Choice[[i]][[j]]][Seq[j],]
  }
}

one=list(); two=list(); three=list(); four=list(); five=list(); six=list(); seven=list()
eight=list(); nine=list(); ten=list()
Choice_N_SIGNAL=list(one, two, three, four, five, six, seven, eight, nine, ten)
rm(one, two, three, four, five, six, seven, eight, nine, ten)
Seq=seq(1, 81, 3)

for (i in 1:10) {
  for (j in 1:27) {
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
    for (k in 1:27) {
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
    for (k in 1:27) {
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

Seq=rep(seq(1,27,1), each=3)
Seq=Seq[-81]

for (i in 1:3) {
  for (j in 1:10) {
    for (z in 1:80) {#used to be 81, and not z+1
      
      Port[[i]][[j]][z]=apply(Choice_N_SIGNAL_FINAL[[i]][[j]][[Seq[z]]]*
                                Monthly_Second[[i]][[j]][[Seq[z]]][z+1,], 1, sum)/
        sum(Choice_N_SIGNAL_FINAL[[i]][[j]][[Seq[z]]])
    }
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
  WML[[i]]=xts(unlist(Port[[1]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
  WML[[i]]=xts(unlist(Port[[2]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
  WML[[i]]=xts(unlist(Port[[3]][[i]]), order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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

Ten_Minus_1=xts(unlist(Port[[1]][[10]])-unlist(Port[[1]][[1]]), 
                order.by = index(Monthly)[1:last(seq_along(Seq))+1])
colnames(Ten_Minus_1)=c("V1")
time(Ten_Minus_1)<- time(Ten_Minus_1) %>%as.yearmon() %>% as.Date()
Ten_Minus_1=merge(Ten_Minus_1, FF)
Ten_Minus_1$V1=Ten_Minus_1$V1-Ten_Minus_1$RF
Ten_Minus_1_fit=lm(V1~MktxRF+SMB+HML,data = Ten_Minus_1)
Ten_Minus_1_vcovHAC_NW=vcovHAC(Ten_Minus_1_fit, weights = bwNeweyWest)
Ten_Minus_1_coeftest=coeftest(Ten_Minus_1_fit, vcov. = Ten_Minus_1_vcovHAC_NW)

Twenty_Minus_11=xts(unlist(Port[[2]][[10]])-unlist(Port[[2]][[1]]), 
                    order.by = index(Monthly)[1:last(seq_along(Seq))+1])
colnames(Twenty_Minus_11)=c("V1")
time(Twenty_Minus_11)<- time(Twenty_Minus_11) %>%as.yearmon() %>% as.Date()
Twenty_Minus_11=merge(Twenty_Minus_11, FF)
Twenty_Minus_11$V1=Twenty_Minus_11$V1-Twenty_Minus_11$RF
Twenty_Minus_11_fit=lm(V1~MktxRF+SMB+HML,data = Twenty_Minus_11)
Twenty_Minus_11_vcovHAC_NW=vcovHAC(Twenty_Minus_11_fit, weights = bwNeweyWest)
Twenty_Minus_11_coeftest=coeftest(Twenty_Minus_11_fit, vcov. = Twenty_Minus_11_vcovHAC_NW)

Thirty_Minus_21=xts(unlist(Port[[3]][[10]])-unlist(Port[[3]][[1]]),
                    order.by = index(Monthly)[1:last(seq_along(Seq))+1])
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
