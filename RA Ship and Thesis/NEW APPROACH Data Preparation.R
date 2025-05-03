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
##################### NEW RET SINGLE ################################
Monthly <-read_excel("C:/Users/talga/Desktop/Thesis/CRSP_Monthly_Clean_New.xlsx")
Monthly=xts(Monthly[, -1], order.by = Monthly$index)

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
for (i in 1:30) {
  df.list[[i]]=df.list[[i]]%>%
    filter(BTM.y>0)
}

demand_signal=list()

for (i in 1:27) {
  demand_signal[[i]]=data.frame(matrix(0,nrow=nrow(Monthly),ncol=length(df.list[[i]]$tic)))
  colnames(demand_signal[[i]])=df.list[[i]]$tic
  demand_signal[[i]]=xts(demand_signal[[i]], order.by = index(Monthly))
  demand_signal[[i]]=demand_signal[[i]][paste0(df.list[[i]]$qtr.y[1]+days(1)
                                               ,"/",df.list[[i+1]]$qtr.y[1])]
}

N_SIGNAL=demand_signal
F_SIGNAL=demand_signal

for (i in 1:27) {
  for (j in 1:3) {
    N_SIGNAL[[i]][j,]=df.list[[i]]$Delta_N    
  }
}

for (i in 1:27) {
  for (j in 1:3) {
    F_SIGNAL[[i]][j,]=df.list[[i]]$Delta_F
  }
}

#rm(df.list, demand_signal)
rm(demand_signal, df.list)

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

#REMEBER THE BIG QUESTION (!?)
F_SIGNAL=as.data.table(F_SIGNAL)
F_SIGNAL=as.data.frame(F_SIGNAL)
F_SIGNAL <- F_SIGNAL[,!colSums(F_SIGNAL==0)]
F_SIGNAL=xts(F_SIGNAL[,-1], order.by = F_SIGNAL$index)
N_SIGNAL=N_SIGNAL[, colnames(N_SIGNAL) %in% colnames(F_SIGNAL)]

Monthly=Monthly[ , order(colnames(Monthly))]
VRY <- read_excel("C:/Users/talga/Desktop/Thesis/VRY_CLEAN.xlsx")
VRY=xts(VRY[,-1], order.by = VRY$date)
VRY=apply.monthly(VRY, matrixStats::colSds)
#VRY=apply.monthly(VRY, colSums)
VRY=rollapplyr(VRY, width=3 , mean, partial = TRUE) #average monthly variance for 3mo
VRY=xts::lag.xts(VRY, 1)
VRY=na.omit(VRY)
VRY=VRY[ , order(colnames(VRY))]
VRY=VRY["2013-04-30/"]
#VRY=VRY["2012-12-31/"]

VOL=read_excel("C:/Users/talga/Desktop/Thesis/VOL_CLEAN.xlsx")
VOL=xts(VOL[,-1], order.by = VOL$date)
VOL=apply.monthly(VOL, colMeans)
VOL=rollapplyr(VOL, width=3 , mean, partial = TRUE)
VOL=xts::lag.xts(VOL, 1)
VOL=na.omit(VOL)
VOL=VOL[ , order(colnames(VOL))]
VOL=VOL["2013-04-30/"]

N_SIGNAL=N_SIGNAL[, colnames(N_SIGNAL) %in% colnames(Monthly)]
F_SIGNAL=F_SIGNAL[, colnames(F_SIGNAL) %in% colnames(Monthly)]
VRY=VRY[, colnames(VRY) %in% colnames(Monthly)]
VOL=VOL[, colnames(VOL) %in% colnames(Monthly)]

Monthly=Monthly[, colnames(Monthly) %in% colnames(N_SIGNAL)]
VRY=VRY[, colnames(VRY) %in% colnames(N_SIGNAL)]
VOL=VOL[, colnames(VOL) %in% colnames(N_SIGNAL)]

Monthly=Monthly[, colnames(Monthly) %in% colnames(VOL)]
VRY=VRY[, colnames(VRY) %in% colnames(VOL)]
N_SIGNAL=N_SIGNAL[, colnames(N_SIGNAL) %in% colnames(VOL)]
F_SIGNAL=F_SIGNAL[, colnames(F_SIGNAL) %in% colnames(VOL)]

momentum_signal=rollapplyr(Monthly, width=3 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)
momentum_signal_skip=xts::lag.xts(momentum_signal, 1)#1-month skip

momentum_signal=na.omit(momentum_signal)
momentum_signal_skip=na.omit(momentum_signal_skip)

Monthly=Monthly["2013-04-30/"]
momentum_signal=momentum_signal["2013-04-30/"]
momentum_signal_skip=momentum_signal_skip["2013-04-30/"]

Monthly=as.data.table(Monthly)
momentum_signal_skip=as.data.table(momentum_signal_skip)
momentum_signal=as.data.table(momentum_signal)
VRY=as.data.table(VRY)
VOL=as.data.table(VOL)
F_SIGNAL=as.data.table(F_SIGNAL)
N_SIGNAL=as.data.table(N_SIGNAL)

write_xlsx(Monthly, "C:/Users/talga/Desktop/RET.xlsx")
write_xlsx(momentum_signal_skip, "C:/Users/talga/Desktop/RET_SIGNAL WITH SKIP.xlsx")
write_xlsx(momentum_signal, "C:/Users/talga/Desktop/RET_SIGNAL NO SKIP.xlsx")
write_xlsx(VRY, "C:/Users/talga/Desktop/VRY_SIGNAL.xlsx")
write_xlsx(VOL, "C:/Users/talga/Desktop/VOL_SIGNAL.xlsx")
write_xlsx(F_SIGNAL, "C:/Users/talga/Desktop/F_SIGNAL.xlsx")
write_xlsx(N_SIGNAL, "C:/Users/talga/Desktop/N_SIGNAL.xlsx")







