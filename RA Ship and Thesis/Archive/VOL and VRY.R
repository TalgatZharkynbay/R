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
library(reshape)
####################################################################
Data <-read.csv("C:/Users/talga/Desktop/Thesis/VOLandVAR.csv")
Data=Data[,-1]
Data$Turnover=Data$VOL/Data$SHROUT
Data=Data[,-c(3,5)]
Data$date=ymd(Data$date)
Data=Data[,-3] #first drop turnover, focus on the VRY

Data=Data%>%
  filter(Turnover!=0)

#Data=Data%>%
  #filter(RET!="C")

#Data=Data%>%
  #filter(RET!="B")

#Data$LogRET=as.numeric(as.character(Data$RET))
#Data$LogRET=log(1+Data$LogRET)
#Data=Data[, -3]
Data=reshape(Data, idvar = "date", timevar = "TICKER", direction = "wide")
colnames(Data)=gsub("^.*?\\.","", colnames(Data))

Test2=Data[, which(colMeans(!is.na(Data)) > 0.95)]
Test2 <- replace(Test2,is.na(Test2),0)
write_xlsx(Test2, "C:/Users/talga/Desktop/VOL_Clean.xlsx")
write_xlsx(Data, "C:/Users/talga/Desktop/VOL_RAW.xlsx")
##################################################################################
VRY <- read_excel("C:/Users/talga/Desktop/Thesis/RET_VRY_CLEAN.xlsx")
VRY=xts(VRY[,-1], order.by = VRY$date)
VRY=apply.monthly(VRY, matrixStats::colSds)
VRY=apply.quarterly(VRY, colMeans)

VOL=read_excel("C:/Users/talga/Desktop/Thesis/VOL_CLEAN.xlsx")
VOL=xts(VOL[,-1], order.by = VOL$date)
VOL=apply.quarterly(VOL, colMeans)






