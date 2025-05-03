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
library(StatMeasures)
library(lmtest)
library(sandwich)
library(reshape)
####################################################################
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

for (i in 1:30) {
  df.list[[i]]$qtr.y<- df.list[[i]]$qtr.y %>% as.yearmon() %>% as.Date()
}

Monthly <-read_excel("C:/Users/talga/Desktop/Thesis/CRSP_Monthly_Clean.xlsx")
Monthly=xts(Monthly[, -1], order.by = Monthly$date)
Quarterly=apply.quarterly(Monthly, colSums);rm(Monthly)
Quarterly=Quarterly[ , order(colnames(Quarterly))]
time(Quarterly)<- time(Quarterly) %>% as.yearmon() %>% as.Date()

Test=list()
Test2=list()
fit_n=list()
fit_f=list()

for (i in 1:27) {
  Test[[i]]=df.list[[i]][, c(1,14,17,18,20,21,19,13)]
  Test2[[i]]=Quarterly[Test[[i]]$qtr.y[1]+months(3)]
  Test2[[i]]=Test2[[i]][, colnames(Test2[[i]]) %in% t(Test[[i]]$tic)]
  Test[[i]]=Test[[i]][t(Test[[i]]$tic)%in% colnames(Test2[[i]]), ]
  Test[[i]]$Rt_Plus_1=t(Test2[[i]][1,])
  #fit_n[[i]]=lm(Rt_Plus_1~Delta_N+log(Size.y)+log(BTM.y), data = Test[[i]])
  #fit_f[[i]]=lm(Rt_Plus_1~Delta_F+log(Size.y)+log(BTM.y), data = Test[[i]])
  fit_n[[i]]=lm(Delta_N~log(Size.y)+log(BTM.y), data = Test[[i]])
  fit_f[[i]]=lm(Delta_F~log(Size.y)+log(BTM.y), data = Test[[i]])
}

delta_f_tstat=vector()
delta_n_tstat=vector()
delta_f_est=vector()
delta_n_est=vector()

for (i in 1:27) {
  delta_f_tstat[i]=summary(fit_f[[i]])$coefficients[2,3]
  delta_n_tstat[i]=summary(fit_n[[i]])$coefficients[2,3]
  delta_f_est=summary(fit_f[[i]])$coefficients[2,1]
  delta_n_est=summary(fit_n[[i]])$coefficients[2,1]
}

mean(abs(delta_f_tstat))
mean(abs(delta_n_tstat))
mean(abs(delta_f_est))
mean(abs(delta_n_est))
############################################################################################
Credit <-read.csv("C:/Users/talga/Desktop/Thesis/Credit Rating Compustat.csv")
Credit=Credit%>%
  filter(splticrm!="")
Credit=Credit[,-1]
Credit$datadate=ymd(Credit$datadate)

Test=reshape(Credit, idvar = "datadate", timevar = "tic", direction = "wide")
colnames(Test)=gsub("^.*?\\.","", colnames(Test))
Test2=Test[, which(colMeans(!is.na(Test)) > 0.95)]
write_xlsx(Test2, "C:/Users/talga/Desktop/Credit Rating Compustat.xlsx")

Report <-read.csv("C:/Users/talga/Desktop/Thesis/Report Date Compustat.csv")
Report=Report[, c(9,13)]
Report$rdq=ymd(Report$rdq)
Report=na.omit(Report)
Report$quarter= Report$rdq%>%as.yearqtr() #HZ CHE DELAT's s etim

############################################################################################
Credit <-read_excel("C:/Users/talga/Desktop/Thesis/Credit Rating Compustat.xlsx")
Date=Credit$datadate
Credit=Credit[,-1]
#Credit=xts(Credit[,-1], order.by = Credit$datadate)

for (i in 1:nrow(Credit)) {
  Credit[i,][Credit[i,] == "AAA"] = 22
  Credit[i,][Credit[i,] == "AA+"] = 21
  Credit[i,][Credit[i,] == "AA"] = 20
  Credit[i,][Credit[i,] == "AA-"] = 19
  Credit[i,][Credit[i,] == "A+"] = 18
  Credit[i,][Credit[i,] == "A"] = 17
  Credit[i,][Credit[i,] == "A-"] = 16
  Credit[i,][Credit[i,] == "BBB+"] = 15
  Credit[i,][Credit[i,] == "BBB"] = 14
  Credit[i,][Credit[i,] == "BBB-"] = 13
  Credit[i,][Credit[i,] == "BB+"] = 12
  Credit[i,][Credit[i,] == "BB"] = 11
  Credit[i,][Credit[i,] == "BB-"] = 10
  Credit[i,][Credit[i,] == "B+"] = 9
  Credit[i,][Credit[i,] == "B"] = 8
  Credit[i,][Credit[i,] == "B-"] = 7
  Credit[i,][Credit[i,] == "CCC+"] = 6
  Credit[i,][Credit[i,] == "CCC"] = 5
  Credit[i,][Credit[i,] == "CCC-"] = 4
  Credit[i,][Credit[i,] == "CC"] = 3
  Credit[i,][Credit[i,] == "C"] = 2
  Credit[i,][Credit[i,] == "D"] = 1
}  

Test=Credit
for (i in 1:ncol(Credit)) {
  Test[,i]<- lapply(Credit[,i],as.numeric)
}
Test=xts(Test, order.by = Date)
Test=na.locf(Test)
Test=as.data.table(Test)
Test=Test[,-1]


Test2=Test

for (i in 2:nrow(Test)) {
  Test2[i-1,]=Test[i,]-Test[i-1,]
}
Test2=Test2[-nrow(Test2),]
Test2=xts(Test2, order.by = Date[2:62])
rm(Test, Credit)
Credit=Test2; rm(Test2)
##############################################################################################
setwd("C:/Users/talga/Desktop/Thesis/Delta Number and Delta Fraction")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

for (i in 1:30) {
  df.list[[i]]$qtr.y= as.Date(as.yearqtr(df.list[[i]]$qtr.y, format='%YQ%q'), frac = 1)
  df.list[[i]]$qtr.x= as.Date(as.yearqtr(df.list[[i]]$qtr.x, format='%YQ%q'), frac = 1)
  
}

for (i in 1:30) {
  df.list[[i]]=df.list[[i]]%>%
    filter(abs(Delta_F)<1)
}

for (i in 1:30) {
  df.list[[i]]=df.list[[i]]%>%
    filter(BTM.y>0)
}

# for (i in 1:30) {
#   df.list[[i]]$qtr.y<- df.list[[i]]$qtr.y %>% as.yearmon() %>% as.Date()
#   df.list[[i]]$qtr.x<- df.list[[i]]$qtr.x %>% as.yearmon() %>% as.Date()
#   
# }

Credit=Credit[ , order(colnames(Credit))]

Test=list()
Test2=list()
fit_n=list()
fit_f=list()
Bla=list()

for (i in 1:17) {
  Test[[i]]=df.list[[i]][, c(1,14,17,18,20,21,19,5)]
  Test2[[i]]=Credit[paste0(Test[[i]]$qtr.x[1]+days(1),'/',Test[[i]]$qtr.y[1])]
  Test2[[i]]=Test2[[i]][, colnames(Test2[[i]]) %in% t(Test[[i]]$tic)]
  Test[[i]]=Test[[i]][t(Test[[i]]$tic)%in% colnames(Test2[[i]]), ]
  Bla[[i]]=colSums(Test2[[i]])
  Test[[i]]$Change=Bla[[i]]
  Test[[i]]$Rt_Plus_1=t(Test2[[i]][1,])
  fit_n[[i]]=lm(Delta_N~Change, data = Test[[i]])
  fit_f[[i]]=lm(Delta_F~Change, data = Test[[i]])
}

delta_f_tstat=vector()
delta_n_tstat=vector()
delta_f_est=vector()
delta_n_est=vector()

for (i in 1:17) {
  delta_f_tstat[i]=summary(fit_f[[i]])$coefficients[2,3]
  delta_n_tstat[i]=summary(fit_n[[i]])$coefficients[2,3]
  delta_f_est=summary(fit_f[[i]])$coefficients[2,1]
  delta_n_est=summary(fit_n[[i]])$coefficients[2,1]
}

mean(abs(delta_f_tstat))
mean(abs(delta_n_tstat))
mean(abs(delta_f_est))
mean(abs(delta_n_est))

####################### VOL and VRY Attempt ##################################################
setwd("C:/Users/talga/Desktop/Thesis/Delta Number and Delta Fraction")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

for (i in 1:30) {
  df.list[[i]]$qtr.y= as.Date(as.yearqtr(df.list[[i]]$qtr.y, format='%YQ%q'), frac = 1)
  df.list[[i]]$qtr.x= as.Date(as.yearqtr(df.list[[i]]$qtr.x, format='%YQ%q'), frac = 1)
  
}

for (i in 1:30) {
  df.list[[i]]=df.list[[i]]%>%
    filter(abs(Delta_F)<1)
}

for (i in 1:30) {
  df.list[[i]]=df.list[[i]]%>%
    filter(BTM.y>0)
}

for (i in 1:30) {
  df.list[[i]]$qtr.y<- df.list[[i]]$qtr.y %>% as.yearmon() %>% as.Date()
}

VRY <- read_excel("C:/Users/talga/Desktop/Thesis/VRY_CLEAN.xlsx")
VRY=xts(VRY[,-1], order.by = VRY$date)
VRY=apply.monthly(VRY, matrixStats::colSds)
VRY=apply.quarterly(VRY, colMeans)
VOL=read_excel("C:/Users/talga/Desktop/Thesis/VOL_CLEAN.xlsx")
VOL=xts(VOL[,-1], order.by = VOL$date)
VOL=apply.quarterly(VOL, colMeans)
VRY=VRY[ , order(colnames(VRY))]
VOL=VOL[ , order(colnames(VOL))]
time(VOL)<- time(VOL) %>% as.yearmon() %>% as.Date()#questionable
time(VRY)<- time(VRY) %>% as.yearmon() %>% as.Date()#questionable
VOL=VOL[, colnames(VOL) %in% colnames(VRY)]
VRY=VRY[, colnames(VRY) %in% colnames(VOL)]


Test=list()
Test2=list()
Test3=list()
fit_n=list()
fit_f=list()

for (i in 1:28) {
  Test[[i]]=df.list[[i]][, c(1,14,20,21)]
  Test2[[i]]=VOL[Test[[i]]$qtr.y[1]]
  Test3[[i]]=VRY[Test[[i]]$qtr.y[1]]
  
  Test2[[i]]=Test2[[i]][, colnames(Test2[[i]]) %in% t(Test[[i]]$tic)]
  Test3[[i]]=Test3[[i]][, colnames(Test3[[i]]) %in% t(Test[[i]]$tic)]
  Test[[i]]=Test[[i]][t(Test[[i]]$tic)%in% colnames(Test3[[i]]), ]

  Test[[i]]$VOL=t(Test2[[i]][1,])
  Test[[i]]$VRY=t(Test3[[i]][1,])
}


for (i in 1:28) {
  Test[[i]]=Test[[i]]%>%
    filter(VOL!=0 & VRY!=0)
  #fit_n[[i]]=lm(abs(Delta_N)~log(VOL)+log(VRY), data = Test[[i]])
  fit_n[[i]]=lm(abs(Delta_N)~log(VRY), data = Test[[i]])
  #fit_f[[i]]=lm(abs(Delta_F)~log(VOL)+log(VRY), data = Test[[i]])
  fit_f[[i]]=lm(abs(Delta_F)~log(VRY), data = Test[[i]])
}

VOL_delta_f_tstat=vector()
VRY_delta_f_tstat=vector()
VOL_delta_n_tstat=vector()
VRY_delta_n_tstat=vector()

VOL_delta_f_est=vector()
VRY_delta_f_est=vector()
VOL_delta_n_est=vector()
VRY_delta_n_est=vector()

CONST_delta_f_tstat=vector()
CONST_delta_n_tstat=vector()
CONST_delta_f_est=vector()
CONST_delta_n_est=vector()

for (i in 1:28) {
  VOL_delta_f_tstat[i]=summary(fit_f[[i]])$coefficients[2,3]
  VRY_delta_f_tstat[i]=summary(fit_f[[i]])$coefficients[3,3]
  
  VOL_delta_n_tstat[i]=summary(fit_n[[i]])$coefficients[2,3]
  VRY_delta_n_tstat[i]=summary(fit_n[[i]])$coefficients[3,3]
  
  VOL_delta_f_est[i]=summary(fit_f[[i]])$coefficients[2,1]
  VRY_delta_f_est[i]=summary(fit_f[[i]])$coefficients[3,1]
  
  VOL_delta_n_est[i]=summary(fit_n[[i]])$coefficients[2,1]
  VRY_delta_n_est[i]=summary(fit_n[[i]])$coefficients[3,1]
  
  CONST_delta_f_tstat[i]=summary(fit_f[[i]])$coefficients[1,3]
  CONST_delta_n_tstat[i]=summary(fit_n[[i]])$coefficients[1,3]
  CONST_delta_f_est[i]=summary(fit_f[[i]])$coefficients[1,1]
  CONST_delta_n_est[i]=summary(fit_n[[i]])$coefficients[1,1] 
}


mean(abs(VOL_delta_f_tstat))
mean(abs(VRY_delta_f_tstat))
mean(VOL_delta_f_est)
mean(VRY_delta_f_est)
mean(abs(CONST_delta_f_tstat))
mean(CONST_delta_f_est)

mean(abs(VOL_delta_n_tstat))
mean(abs(VRY_delta_n_tstat))
mean(VOL_delta_n_est)
mean(VRY_delta_n_est)
mean(abs(CONST_delta_n_tstat))
mean(CONST_delta_n_est)

delta_f_RSQ=vector()
delta_n_RSQ=vector()
for (i in 1:28) {
  delta_f_RSQ[i]=summary(fit_f[[i]])$adj.r.squared
  delta_n_RSQ[i]=summary(fit_n[[i]])$adj.r.squared
}  

mean(delta_f_RSQ)
mean(delta_n_RSQ)

for (i in 1:28) {
  VRY_delta_f_est[i]=summary(fit_f[[i]])$coefficients[2,1]  
}

mean(VRY_delta_f_est)

