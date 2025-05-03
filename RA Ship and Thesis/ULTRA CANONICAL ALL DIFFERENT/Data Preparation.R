options(scipen = 10, digits = 3)
library(writexl)
library(lubridate)
library(tidyverse)
library(quantmod)
library(highcharter)
library(tidyquant)
library(readxl)
library(readr)
library(data.table)
################## 1990-2005 #####################################
CRSP=read.csv("C:/Users/talga/Desktop/CHECK/1990-2005.csv")
CRSP$date=ymd(CRSP$date)
CRSP=CRSP[, -1]
CRSP$LogRET=as.numeric(as.character(CRSP$RET))
CRSP$LogRET=log(1+CRSP$LogRET)
CRSP=CRSP[, -3]
Test=reshape(CRSP, idvar = "date", timevar = "TICKER", direction = "wide")
colnames(Test)=gsub("^.*?\\.","", colnames(Test))
Test2=Test[, which(colMeans(!is.na(Test)) > 0.90)]
Test2 <- replace(Test2,is.na(Test2),0)
rm(Test, CRSP)
Test2=xts(Test2[,-1], order.by = Test2$date)
momentum_signal=rollapplyr(Test2, width=3 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)
momentum_signal=na.omit(momentum_signal)
Test2=Test2["1990-04-30/"]
momentum_signal=as.data.table(momentum_signal)
Test2=as.data.table(Test2)
write_xlsx(Test2, "C:/Users/talga/Desktop/Monthly.xlsx")
write_xlsx(Test2, "C:/Users/talga/Desktop/RET SIGNAL NO SKIP.xlsx")


################## 1964-2012 #####################################
CRSP=read.csv("C:/Users/talga/Desktop/CHECK/1964-2012.csv")
CRSP$date=ymd(CRSP$date)
CRSP=CRSP[, -1]
CRSP$LogRET=as.numeric(as.character(CRSP$RET))
CRSP=CRSP[-1,]
CRSP$LogRET=log(1+CRSP$LogRET)
CRSP=CRSP[, -3]
Test=reshape(CRSP, idvar = "date", timevar = "TICKER", direction = "wide")
colnames(Test)=gsub("^.*?\\.","", colnames(Test))

Test2=Test[, which(colMeans(!is.na(Test)) > 0.30)]
Test2 <- replace(Test2,is.na(Test2),0)
rm(Test, CRSP)
Test2$date=ymd(Test2$date)
Test2=xts(Test2[,-c(1:2)], order.by = Test2$date)
momentum_signal=rollapplyr(Test2, width=3 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)
momentum_signal=na.omit(momentum_signal)
Test2=Test2["1964-04-30/"]
momentum_signal=as.data.table(momentum_signal)
Test2=as.data.table(Test2)

write.csv(Test, "C:/Users/talga/Desktop/1964-2012 Dirty.csv")
write_xlsx(Test2, "C:/Users/talga/Desktop/1964-2012 Cleaner.xlsx")
write_xlsx(momentum_signal, "C:/Users/talga/Desktop/RET SIGNAL NO SKIP.xlsx")
