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
library(qmao)
###########################################################################
setwd("C:/Users/talga/Desktop/Thesis/Unique Stocks and N")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

intersect=list()
for (i in 1:31) {
  intersect[[i]]=c(df.list[[i]]$stock_id)
}

for (i in 1:31) {
  assign(paste(gsub("\\..*","", file.list[i])), intersect[[i]])
}

unique=c(`2012Q4`, `2013Q1`, `2014Q1`,`2014Q2`,`2014Q3`,`2014Q4`,
         `2013Q2`,`2013Q3`,`2013Q4`,`2015Q1`,`2015Q2`,`2015Q3`,`2015Q4`,
         `2016Q1`,`2016Q2`,`2016Q3`,`2016Q4`,`2017Q1`,`2017Q2`,`2017Q3`,`2017Q4`,
         `2017Q1`,`2017Q2`,`2017Q3`,`2017Q4`, `2018Q1`,`2018Q2`,`2018Q3`,`2018Q4`,
         `2019Q1`,`2019Q2`,`2019Q3`,`2019Q4`,`2020Q1`,`2020Q2`)
unique=unique(unique)

# Prices<-read_excel("C:/Users/talga/Desktop/Thesis/Prices_Even_Cleaner.xlsx")
# Returns=Prices[, colnames(Prices) %in% unique] #Have rets for 1662 stocks(need 884)
# Returns$Date=Prices$Date
# NotAvailabe=subset(unique,  !unique %in% colnames(Returns))

Symbols=unique
Returns_2 <-
  getSymbols(Symbols,
             src = 'yahoo',
             from = "2012-01-01",
             to = "2020-10-06",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge)


Returns=xts(Returns[, -ncol(Returns)],order.by = Returns$Date)

Testtt=merge(Returns, Returns_2)
Testtt=Testtt["2012-04/"]
PricesUniverse_Dirty=as.data.table(Testtt)

LogReturns_Daily=Return.calculate(Testtt, method = "log")
LogReturns_Daily=as.data.table(LogReturns_Daily)[-1,]
LogReturns_Daily=as.data.frame(LogReturns_Daily)

LogReturns_Daily_Clean=LogReturns_Daily[, which(colMeans(!is.na(LogReturns_Daily)) > 0.9)]
LogReturns_Daily_Clean <- replace(LogReturns_Daily_Clean,is.na(LogReturns_Daily_Clean),0)

Testtt_Monthly=as.data.table(Testtt)
Testtt_Monthly=as.data.frame(Testtt_Monthly)
Testtt_Monthly=Testtt_Monthly[, which(colMeans(!is.na(Testtt_Monthly)) > 0.9)]
Testtt_Monthly <- replace(Testtt_Monthly,is.na(Testtt_Monthly),0)
Testtt_Monthly=xts(Testtt_Monthly[, -1], order.by = Testtt_Monthly$index)
Testtt_Monthly=to.monthly(Testtt_Monthly,
                           indexAt = "startof",
                           OHLC = FALSE)

LogReturns_Monthly=Return.calculate(Testtt_Monthly, method = "log")[-1,]
LogReturns_Monthly=as.data.table(LogReturns_Monthly)[-1,]


write_xlsx(LogReturns_Daily, "C:/Users/talga/Desktop/LogReturns_daily.xlsx")
write_xlsx(LogReturns_Monthly, "C:/Users/talga/Desktop/LogReturns_Monthly.xlsx")
write_xlsx(PricesUniverse_Dirty, "C:/Users/talga/Desktop/PricesUniverse_Dirty.xlsx")
write_xlsx(LogReturns_Daily_Clean, "C:/Users/talga/Desktop/LogReturns_Daily_Clean.xlsx")
write_xlsx(LogReturns_Monthly, "C:/Users/talga/Desktop/LogReturns_Monthly_Clean.xlsx")

########################## Super Clean, Yahoo Only ####################################

setwd("C:/Users/talga/Desktop/Thesis/Unique Stocks and N")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

intersect=list()
for (i in 1:31) {
  intersect[[i]]=c(df.list[[i]]$stock_id)
}

for (i in 1:31) {
  assign(paste(gsub("\\..*","", file.list[i])), intersect[[i]])
}

unique=c(`2012Q4`, `2013Q1`, `2014Q1`,`2014Q2`,`2014Q3`,`2014Q4`,
         `2013Q2`,`2013Q3`,`2013Q4`,`2015Q1`,`2015Q2`,`2015Q3`,`2015Q4`,
         `2016Q1`,`2016Q2`,`2016Q3`,`2016Q4`,`2017Q1`,`2017Q2`,`2017Q3`,`2017Q4`,
         `2017Q1`,`2017Q2`,`2017Q3`,`2017Q4`, `2018Q1`,`2018Q2`,`2018Q3`,`2018Q4`,
         `2019Q1`,`2019Q2`,`2019Q3`,`2019Q4`,`2020Q1`,`2020Q2`)
unique=unique(unique)

# Prices<-read_excel("C:/Users/talga/Desktop/Thesis/Prices_Even_Cleaner.xlsx")
# Returns=Prices[, colnames(Prices) %in% unique] #Have rets for 1662 stocks(need 884)
# Returns$Date=Prices$Date
# NotAvailabe=subset(unique,  !unique %in% colnames(Returns))

Symbols=unique[1:546]
Symbols2=unique[547:1000]
Symbols3=unique[1001:1500]
Symbols4=unique[1501:2000]
Symbols5=unique[2001:2546]

# Returns_1 <-
#   getSymbols(Symbols,
#              src = 'yahoo',
#              from = "2012-01-01",
#              to = "2020-11-08",
#              auto.assign = TRUE,
#              warnings = FALSE) %>%
#   map(~Ad(get(.))) %>%
#   reduce(merge) 
e=new.env()
getSymbols(Symbols5, from = "2012-01-01", to = "2020-11-08", env = e)
Returns_1=do.call(merge, lapply(e, Ad))
        
#Returns_1=Return.calculate(Returns_1, method = "log")[-1,]
Part_1=as.data.table(Returns_1)
write_xlsx(Part_1, "C:/Users/talga/Desktop/Part5.xlsx")

Part_1=read_excel("C:/Users/talga/Desktop/Part1.xlsx")
Part_2=read_excel("C:/Users/talga/Desktop/Part2.xlsx")
Part_3=read_excel("C:/Users/talga/Desktop/Part3.xlsx")
Part_4=read_excel("C:/Users/talga/Desktop/Part4.xlsx")
Part_5=read_excel("C:/Users/talga/Desktop/Part5.xlsx")

Part_1=xts(Part_1[, -1], order.by = Part_1$index)
Part_2=xts(Part_2[, -1], order.by = Part_2$index)
Part_3=xts(Part_3[, -1], order.by = Part_3$index)
Part_4=xts(Part_4[, -1], order.by = Part_4$index)
Part_5=xts(Part_5[, -1], order.by = Part_5$index)

Prices=merge(Part_1,Part_2,Part_3,Part_4,Part_5)
Test=Return.calculate(Prices, method = "log")
Test=as.data.table(Test)
write_xlsx(Test, "C:/Users/talga/Desktop/LogReturns_Daily_YFonly.xlsx")

#################### Cleaning YF ONLY ##############################################
Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_YFonly.xlsx")
NoFullStops=gsub("\\..*","", colnames(Daily))
colnames(Daily)=NoFullStops
Daily=Daily[-1,]
write_xlsx(Daily, "C:/Users/talga/Desktop/LogReturns_Daily_YFonly.xlsx")


Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_YFonly.xlsx")
LogReturns_Daily_Clean=Daily[, which(colMeans(!is.na(Daily)) > 0.9)]
LogReturns_Daily_Clean <- replace(LogReturns_Daily_Clean,is.na(LogReturns_Daily_Clean),0)
write_xlsx(LogReturns_Daily_Clean, "C:/Users/talga/Desktop/LogReturns_Daily_YFonly_Clean.xlsx")


#################### CRSP ULTRA CLEAN ##############################################
CRSP=read.csv("C:/Users/talga/Desktop/Thesis/CRSP.csv")
CRSP$date=ymd(CRSP$date)
CRSP=CRSP[, -1]
CRSP$LogRET=as.numeric(as.character(CRSP$RET))
CRSP$LogRET=log(1+CRSP$LogRET)
CRSP=CRSP[, -3]
Test=reshape(CRSP, idvar = "date", timevar = "TICKER", direction = "wide")
colnames(Test)=gsub("^.*?\\.","", colnames(Test))

Test2=Test[, which(colMeans(!is.na(Test)) > 0.95)]
Test2 <- replace(Test2,is.na(Test2),0)
write_xlsx(Test2, "C:/Users/talga/Desktop/CRSP_Monthly_Clean.xlsx")
