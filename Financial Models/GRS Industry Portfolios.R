# Starting #
library(xts)
library(zoo)
library(GRS.test)
options(digits = 3)

####### Data Tailoring ##########

Data=read.csv(url("https://www.dropbox.com/s/x8eldqliqarhrhc/GRS_Data.csv?raw=1"),
              sep = ";")

Data$Date=Data[,1]

Data=Data[, -1]

Data$Date=seq(as.Date("1963-07-01"),
              + as.Date("2018-12-01"),by="months")

Data=xts(Data[1:35], order.by = Data$Date)


#CAPM GRS:
Returns_Matrix=Data[, 6:35]

Mkt_Rf=Data[, 1]

CAPM_GRS=GRS.test(Returns_Matrix,Mkt_Rf)

c(CAPM_GRS$GRS.stat, CAPM_GRS$GRS.pval)

#FF 3 Factors GRS:
FF_3_Factors=Data[, 1:3]

FF_3_GRS=GRS.test(Returns_Matrix, FF_3_Factors)

c(FF_3_GRS$GRS.stat, FF_3_GRS$GRS.pval)


