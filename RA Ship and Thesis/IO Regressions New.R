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
################# Data ######################################
VRY<-read_excel("C:/Users/talga/Desktop/Thesis/VRY_SIGNAL.xlsx")
VOL<-read_excel("C:/Users/talga/Desktop/Thesis/VOL_SIGNAL.xlsx")
VRY <- VRY[,!colSums(VRY==0)]
VOL=VOL[, colnames(VOL) %in% colnames(VRY)]
VRY=xts(VRY[,-1], order.by = VRY$index)
VOL=xts(VOL[,-1], order.by = VOL$index)
F_SIGNAL<-read_excel("C:/Users/talga/Desktop/Thesis/F_SIGNAL.xlsx")
N_SIGNAL<-read_excel("C:/Users/talga/Desktop/Thesis/N_SIGNAL.xlsx")
F_SIGNAL=xts(F_SIGNAL[, -1], order.by = F_SIGNAL$index)
N_SIGNAL=xts(N_SIGNAL[, -1], order.by = N_SIGNAL$index)
F_SIGNAL=abs(F_SIGNAL)
N_SIGNAL=abs(N_SIGNAL)
N_SIGNAL=N_SIGNAL[, colnames(N_SIGNAL) %in% colnames(VRY)]
F_SIGNAL=F_SIGNAL[, colnames(F_SIGNAL) %in% colnames(VRY)]

################# CORR VOL, VRY ######################################
rho=vector()
iterator=seq(1,81,3)

N_SIGNAL_Q=N_SIGNAL[1:27,]
F_SIGNAL_Q=F_SIGNAL[1:27,]
VOL_Q=VOL[1:27,]
VRY_Q=VRY[1:27,]

for (i in 1:27) {
  N_SIGNAL_Q[i,]=N_SIGNAL[iterator[i],]
  VRY_Q[i,]=VRY[iterator[i],]
  F_SIGNAL_Q[i,]=F_SIGNAL[iterator[i],]
  VOL_Q[i,]=VOL[iterator[i],]
}
rm(F_SIGNAL, N_SIGNAL, VOL, VRY)

for (i in 1:1430) {
  rho[i]=cor(VOL_Q[,i], VRY_Q[,i])
}
mean(rho)

for (i in 1:1430)   {
  rho[i]=cor(N_SIGNAL_Q[,i] , VRY_Q[,i])
}
mean(rho)

for (i in 1:1430)   {
  rho[i]=cor(N_SIGNAL_Q[,i] , VOL_Q[,i])
}
mean(rho)

for (i in 1:1430)   {
  rho[i]=cor(F_SIGNAL_Q[,i] , VRY_Q[,i])
}
mean(rho)

for (i in 1:1430)   {
  rho[i]=cor(F_SIGNAL_Q[,i] , VOL_Q[,i])
}
mean(rho)
################# NUM~VRY ######################################

fit_n_VRY=list()
fit_n_VOL=list()
fit_f_VRY=list()
fit_f_VOL=list()

for (i in 1:1430) {
  fit_n_VRY[[i]]=lm(N_SIGNAL_Q[,i]~log(VRY_Q[,i]))
}

VRY_delta_n_est=vector()
VRY_CONST_delta_n_est=vector()

for (i in 1:1430) {
  VRY_delta_n_est[i]=summary(fit_n_VRY[[i]])$coefficients[2,1]
  VRY_CONST_delta_n_est[i]=summary(fit_n_VRY[[i]])$coefficients[1,1]
}

mean(VRY_delta_n_est)
mean(VRY_delta_n_est)/(sd(VRY_delta_n_est)/sqrt(27))
mean(VRY_CONST_delta_n_est)
mean(VRY_CONST_delta_n_est)/(sd(VRY_CONST_delta_n_est)/sqrt(27))

################# NUM~VOL ######################################

for (i in 1:1430) {
  fit_n_VOL[[i]]=lm(N_SIGNAL_Q[,i]~log(VOL_Q[,i]))
}

VOL_delta_n_est=vector()
VOL_CONST_delta_n_est=vector()

for (i in 1:1430) {
  VOL_delta_n_est[i]=summary(fit_n_VOL[[i]])$coefficients[2,1]
  VOL_CONST_delta_n_est[i]=summary(fit_n_VOL[[i]])$coefficients[1,1]
}

mean(VOL_delta_n_est)
mean(VOL_delta_n_est)/(sd(VOL_delta_n_est)/sqrt(27))
mean(VOL_CONST_delta_n_est)
mean(VOL_CONST_delta_n_est)/(sd(VOL_CONST_delta_n_est)/sqrt(27))

################# FRA~VRY ######################################
for (i in 1:1430) {
  fit_f_VRY[[i]]=lm(F_SIGNAL_Q[,i]~log(VRY_Q[,i]))
}

VRY_delta_f_est=vector()
VRY_CONST_delta_f_est=vector()

for (i in 1:1430) {
  VRY_delta_f_est[i]=summary(fit_f_VRY[[i]])$coefficients[2,1]
  VRY_CONST_delta_f_est[i]=summary(fit_f_VRY[[i]])$coefficients[1,1]
}

mean(VRY_delta_f_est)
mean(VRY_delta_f_est)/(sd(VRY_delta_f_est)/sqrt(27))
mean(VRY_CONST_delta_f_est)
mean(VRY_CONST_delta_f_est)/(sd(VRY_CONST_delta_f_est)/sqrt(27))

################# FRA~VOL ######################################

for (i in 1:1430) {
  fit_f_VOL[[i]]=lm(F_SIGNAL_Q[,i]~log(VOL_Q[,i]))
}

VOL_delta_f_est=vector()
VOL_CONST_delta_f_est=vector()

for (i in 1:1430) {
  VOL_delta_f_est[i]=summary(fit_f_VOL[[i]])$coefficients[2,1]
  VOL_CONST_delta_f_est[i]=summary(fit_f_VOL[[i]])$coefficients[1,1]
}

mean(VOL_delta_f_est)
mean(VOL_delta_f_est)/(sd(VOL_delta_f_est)/sqrt(27))
mean(VOL_CONST_delta_f_est)
mean(VOL_CONST_delta_f_est)/(sd(VOL_CONST_delta_f_est)/sqrt(27))

################# NUM~VRY+VOL ######################################
fit_n_VRY_VOL=list()
fit_f_VRY_VOL=list()

for (i in 1:1430) {
  fit_n_VRY_VOL[[i]]=lm(N_SIGNAL_Q[,i]~log(VRY_Q[,i])+log(VOL_Q[,i]))
}

VRY_delta_n_est=vector()
VRY_CONST_delta_n_est=vector()
VOL_delta_n_est=vector()

for (i in 1:27) {
  VRY_delta_n_est[i]=summary(fit_n_VRY_VOL[[i]])$coefficients[2,1]
  VRY_CONST_delta_n_est[i]=summary(fit_n_VRY_VOL[[i]])$coefficients[1,1]
  VOL_delta_n_est[i]=summary(fit_n_VRY_VOL[[i]])$coefficients[3,1]

}

mean(VRY_delta_n_est)
mean(VRY_delta_n_est)/(sd(VRY_delta_n_est)/sqrt(27))

mean(VRY_CONST_delta_n_est)
mean(VRY_CONST_delta_n_est)/(sd(VRY_CONST_delta_n_est))

mean(VOL_delta_n_est)
mean(VOL_delta_n_est)/(sd(VOL_delta_n_est)/sqrt(27))

################# FRA~VRY+VOL ######################################

for (i in 1:1430) {
  fit_f_VRY_VOL[[i]]=lm(F_SIGNAL_Q[,i]~log(VRY_Q[,i])+log(VOL_Q[,i]))
}

VRY_delta_n_est=vector()
VRY_CONST_delta_n_est=vector()
VOL_delta_n_est=vector()


for (i in 1:27) {
  VRY_delta_n_est[i]=summary(fit_f_VRY_VOL[[i]])$coefficients[2,1]
  VRY_CONST_delta_n_est[i]=summary(fit_f_VRY_VOL[[i]])$coefficients[1,1]
  VOL_delta_n_est[i]=summary(fit_f_VRY_VOL[[i]])$coefficients[3,1]
  
}

mean(VRY_delta_n_est)
mean(VRY_delta_n_est)/(sd(VRY_delta_n_est)/sqrt(27))

mean(VRY_CONST_delta_n_est)
mean(VRY_CONST_delta_n_est)/(sd(VRY_CONST_delta_n_est))

mean(VOL_delta_n_est)
mean(VOL_delta_n_est)/(sd(VOL_delta_n_est)/sqrt(27))

