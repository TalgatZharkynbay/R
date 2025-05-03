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
library(ggplot2)
library(StatMeasures)
################### Huinya ##############################
# LogReturns <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_Clean.xlsx")
# LogReturns=xts(LogReturns[, -ncol(LogReturns)], order.by = LogReturns$index)[,-1]

# LongFormat=LogReturns%>%
#   gather("Symbol","Return",-Date)
# 
# Test=LongFormat%>%
#   filter(Symbol=="MMM" & year(Date)==2012)
# 
# ####################### Monthly WML (3,1,no skip) ##################################
#Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_Clean.xlsx")
Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_YFonly_Clean.xlsx")
Monthly=xts(Daily[, -1], order.by = Daily$index)
Monthly=apply.monthly(Monthly, colSums) 
#Monthly=Monthly[-1,]
#time(Monthly)=as.Date(as.yearmon(time(Monthly)))

momentum_signal=rollapplyr(Monthly, width=3 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)  
momentum_signal=na.omit(momentum_signal)

rank_signal=t(apply(momentum_signal, 1, rank))

for (i in 1: nrow(rank_signal)) {
  
  rank_signal[i,]=decile(rank_signal[i,], decreasing = TRUE) #smallest values in decile 10   
}

for (i in 1:nrow(rank_signal)) {
  rank_signal[i,][rank_signal[i,] == 10] = -1
  rank_signal[i,][rank_signal[i,] == 9] = 0
  rank_signal[i,][rank_signal[i,] == 8] = 0
  rank_signal[i,][rank_signal[i,] == 7] = 0
  rank_signal[i,][rank_signal[i,] == 6] = 0
  rank_signal[i,][rank_signal[i,] == 5] = 0
  rank_signal[i,][rank_signal[i,] == 4] = 0
  rank_signal[i,][rank_signal[i,] == 3] = 0
  rank_signal[i,][rank_signal[i,] == 2] = 0
  rank_signal[i,][rank_signal[i,] == 1] = 1
}  

Port=rank_signal*Monthly[-c(1:3),]
WML=(apply(Port, 1, sum)/length(which(Port[1,]!=0)))*2
WML=xts(WML, order.by = index(Port))
plot(cumsum(WML))
sum(WML)
skewness(WML)

WML=as.data.table(WML)
WML %>% 
  ggplot(aes(x = V1)) +
  stat_density(geom = "line", alpha = 1) +
  ggtitle("WML(3,1)") +
  xlab("monthly returns") +
  ylab("distribution") 


# ####################### Monthly WML (3,3,no skip) ################################
#Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_Clean.xlsx")
Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_YFonly_Clean.xlsx")
Monthly=xts(Daily[, -1], order.by = Daily$index)
Monthly=apply.monthly(Monthly, colSums) #should have the st-reversal isolation here, no first week

momentum_signal=rollapplyr(Monthly, width=3 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)  
momentum_signal=na.omit(momentum_signal)

rank_signal=t(apply(momentum_signal, 1, rank))

for (i in 1: nrow(rank_signal)) {
  
  rank_signal[i,]=decile(rank_signal[i,], decreasing = TRUE) #smallest values in decile 10   
}

for (i in 1:nrow(rank_signal)) {
  rank_signal[i,][rank_signal[i,] == 10] = -1
  rank_signal[i,][rank_signal[i,] == 9] = 0
  rank_signal[i,][rank_signal[i,] == 8] = 0
  rank_signal[i,][rank_signal[i,] == 7] = 0
  rank_signal[i,][rank_signal[i,] == 6] = 0
  rank_signal[i,][rank_signal[i,] == 5] = 0
  rank_signal[i,][rank_signal[i,] == 4] = 0
  rank_signal[i,][rank_signal[i,] == 3] = 0
  rank_signal[i,][rank_signal[i,] == 2] = 0
  rank_signal[i,][rank_signal[i,] == 1] = 1
}  

Monthly=Monthly[-c(1:3),]
Port=list()
WML=list()

for (i in 1:100) {
    Port[[i]]=rbind(Monthly[i,]*rank_signal[i,], Monthly[i+1,]*rank_signal[i,],
          Monthly[i+2,]*rank_signal[i,])
    
    WML[[i]]=xts(apply(Port[[i]], 1, sum)/length(which(Port[[i]][1,]!=0)),
                 order.by = index(Port[[i]]))
}

Overlapped=do.call(merge, WML)  
colnames(Overlapped)=c(1:100)
Overlapped[is.na(Overlapped)]=0
WML_NoSkip=xts(apply(Overlapped, 1, sum)/3, order.by = index(Overlapped))

plot(WML_NoSkip)
plot(cumsum(WML_NoSkip))
sum(WML_NoSkip)
# getSymbols("^GSPC", from="2012-07-30", to="2020-10-06	")
# GSPC=na.omit(Return.calculate(GSPC$GSPC.Adjusted, method = "log"))
# plot(cumsum(GSPC))

WML_NoSkip__Long=as.data.table(WML_NoSkip)
WML_NoSkip__Long %>% 
  ggplot(aes(x = V1)) +
  stat_density(geom = "line", alpha = 1) +
  ggtitle("WML(3,3)") +
  xlab("monthly returns") +
  ylab("distribution") 
skewness(WML_NoSkip)

######################## Monthly WML (3,3, 1 week skip) ################################
Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_Clean.xlsx")
Monthly=xts(Daily[, -1], order.by = Daily$index)
Monthly=apply.monthly(Monthly, colSums) #should have the st-reversal isolation here, no first week

momentum_signal=rollapplyr(Monthly, width=3 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)  
momentum_signal=na.omit(momentum_signal)

rank_signal=t(apply(momentum_signal, 1, rank))

for (i in 1: nrow(rank_signal)) {
  
  rank_signal[i,]=decile(rank_signal[i,], decreasing = TRUE) #smallest values in decile 10   
}

for (i in 1:nrow(rank_signal)) {
  rank_signal[i,][rank_signal[i,] == 10] = -1
  rank_signal[i,][rank_signal[i,] == 9] = 0
  rank_signal[i,][rank_signal[i,] == 8] = 0
  rank_signal[i,][rank_signal[i,] == 7] = 0
  rank_signal[i,][rank_signal[i,] == 6] = 0
  rank_signal[i,][rank_signal[i,] == 5] = 0
  rank_signal[i,][rank_signal[i,] == 4] = 0
  rank_signal[i,][rank_signal[i,] == 3] = 0
  rank_signal[i,][rank_signal[i,] == 2] = 0
  rank_signal[i,][rank_signal[i,] == 1] = 1
}  

Monthly_skip=xts(Daily[, -1], order.by = Daily$index)
Monthly_skip[.indexmday(Monthly_skip) == 1,]=0
Monthly_skip[.indexmday(Monthly_skip) == 2,]=0
Monthly_skip[.indexmday(Monthly_skip) == 3,]=0
Monthly_skip[.indexmday(Monthly_skip) == 4,]=0
Monthly_skip[.indexmday(Monthly_skip) == 5,]=0

Monthly_skip=apply.monthly(Monthly_skip, colSums)
Monthly_skip=Monthly_skip[-c(1:3),]

Port=list()
WML=list()

for (i in 1:100) {
  Port[[i]]=rbind(Monthly_skip[i,]*rank_signal[i,], Monthly_skip[i+1,]*rank_signal[i,],
                  Monthly_skip[i+2,]*rank_signal[i,])
  
  WML[[i]]=xts(apply(Port[[i]], 1, sum)/length(which(Port[[i]][1,]!=0)),
               order.by = index(Port[[i]]))
}

Overlapped=do.call(merge, WML)  
colnames(Overlapped)=c(1:98)
Overlapped[is.na(Overlapped)]=0
WML_With_Skip=xts(apply(Overlapped, 1, sum)/3, order.by = index(Overlapped))

plot(WML_With_Skip)
plot(cumsum(WML_With_Skip))
WML_With_Skip_Long=as.data.table(WML_With_Skip)
WML_With_Skip_Long %>% 
  ggplot(aes(x = V1)) +
  stat_density(geom = "line", alpha = 1) +
  ggtitle("WML(3,3)") +
  xlab("monthly returns") +
  ylab("distribution") 
skewness(WML_With_Skip)
