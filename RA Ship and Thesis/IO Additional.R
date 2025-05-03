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
#Monthly=exp(Monthly)-1
momentum_signal=xts(momentum_signal[, -1], order.by = momentum_signal$index)
F_SIGNAL=xts(F_SIGNAL[, -1], order.by = F_SIGNAL$index)
N_SIGNAL=xts(N_SIGNAL[, -1], order.by = N_SIGNAL$index)
VRY=xts(VRY[, -1], order.by = VRY$index)
VOL=xts(VOL[, -1], order.by = VOL$index)
F_SIGNAL=abs(F_SIGNAL)
N_SIGNAL=abs(N_SIGNAL)

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
    filter(BTM.x>0)
}

for (i in 1:30) {
  df.list[[i]]$qtr.y<- df.list[[i]]$qtr.y %>% as.yearmon() %>% as.Date()
}

for (i in 1:30) {
  df.list[[i]]=df.list[[i]][df.list[[i]]$tic %in%  colnames(Monthly), c(1,8,9)]
}
Monthly=apply.quarterly(Monthly, colSums)


test=data.frame(matrix(, nrow=1432, ncol=6))
test$Monthly=t(Monthly[3,])
test$F_SIGNAL=t(F_SIGNAL[4,])
test$VOL=t(log(VOL[3,]))
test$VRY=t(log(VRY[3,]))
test$BTM.x=log(df.list[[2]]$BTM.x)
test$Size.x=log(df.list[[2]]$Size.x)
test$Monthly_Old=t(Monthly[2,])

fit=lm(Monthly~Monthly_Old+Monthly_Old:F_SIGNAL+Monthly_Old:VOL+Monthly_Old:VRY+
         Size.x+BTM.x, data = test)

summary(fit)


