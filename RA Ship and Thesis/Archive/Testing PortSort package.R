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
##############################################################################
data(Factors)
s = 21
k = 1
R.Forward = Factors[[1]]; R.Lag = Factors[[2]]; V.Lag = Factors[[3]]
XSMOM = R.Lag
XSMOM[1:nrow(XSMOM),1:ncol(XSMOM)] <- NA

for (i in 1:ncol(R.Lag)){
  
  for (t in (s + 1):nrow(R.Lag)){
    XSMOM[t,i] =  sum(R.Lag[(t-s):(t-1-k),i])
    
  }
}
# Remove the formation period (s) by using na.omit
XSMOM = na.omit(XSMOM)
# Re-subset R.Forward
R.Forward = R.Forward[(s + 1):nrow(R.Forward), ]

# Specify the factors we need - specify Fb and Fc as NULL
Fa = XSMOM; Fb = NULL; Fc = NULL

#Specify the dimension of the sort - let's use quintiles
dimA = 0:5/5
# Run either the conditional or unconditional sort function (for univariate sorts there is no 
#difference)
XSMOM.output = conditional.sort(Fa=Fa,R.Forward=R.Forward,dimA=dimA)
Returns=XSMOM.output$returns

# Let's now investigate the risk and return profiles of the sub-portfolios
table.AnnualizedReturns(XSMOM.output$returns,scale = 365, geometric = FALSE)
portfolio.turnover(XSMOM.output)$`Mean Turnover`
portfolio.frequency(XSMOM.output, rank = 1)
portfolio.mean.size(XSMOM.output)

# Following the methodology of Jegadeesh and Titman, 1993, we will now form a long-short, 
# zero-cost portfolio which initiates a long position in the high momentum sub-portfolio 
# (portfolio 5) and a short position in the low momentum sub-portfolio (portfolio 1)

LS.Portfolio = XSMOM.output$returns[,5] + (-1*XSMOM.output$returns[,1])#WML
skewness(LS.Portfolio)
# Investigate risk and return
table.AnnualizedReturns(LS.Portfolio,scale = 365, geometric = FALSE)
# We can now plot the back-tested results
chart.CumReturns(LS.Portfolio, geometric = FALSE, main = "XSMOM Long-Short Portfolio")

################# Double Sorted Example #####################################################
data(Factors)
R.Forward = Factors[[1]]; R.Lag = Factors[[2]]; V.Lag = Factors[[3]]
Fa = R.Lag; Fb = V.Lag

#Specify the dimension of the sort - let's try a 3x3 sort (3 breakpoints for each factor)
dimA = 0:3/3
dimB = 0:3/3
# Run both the conditional and unconditional sort
sort.con = conditional.sort(Fa=Fa,Fb=Fb,R.Forward = R.Forward,dimA=dimA,dimB=dimB)
sort.uncon = unconditional.sort(Fa=Fa,Fb=Fb,R.Forward = R.Forward,dimA=dimA,dimB=dimB)

# Let's now investigate the risk and return profiles of the sub-portfolios
table.AnnualizedReturns(sort.con$returns,scale = 365, geometric = FALSE)
table.AnnualizedReturns(sort.uncon$returns,scale = 365, geometric = FALSE)


Conditonal.LS.Portfolio = sort.con$returns[,1] + (-1*sort.con$returns[,9])
Unconditonal.LS.Portfolio = sort.uncon$returns[,1] + (-1*sort.uncon$returns[,9])

Portfolios = cbind(Conditonal.LS.Portfolio,Unconditonal.LS.Portfolio)
colnames(Portfolios) = c("Conditional","Unconditional")
# Chart the logarithmic cumulative returns
chart.CumReturns(Portfolios, geometric = FALSE, legend.loc = "topleft",
                 main = "Sorting Comparison")


###################### Replicating my methodology ##############################################
Daily <- read_excel("C:/Users/talga/Desktop/Thesis/LogReturns_Daily_YFonly_Clean.xlsx")
FF=read_excel("C:/Users/talga/Desktop/Thesis/FF_Monthly_Good.xlsx")
FF$Date=ymd(FF$Date)
FF=xts(FF[,-1], order.by = FF$Date)
Monthly=xts(Daily[, -1], order.by = Daily$index)
Monthly=apply.monthly(Monthly, colSums) 

momentum_signal=rollapplyr(Monthly, width=3 , sum, partial = TRUE)
momentum_signal=xts::lag.xts(momentum_signal, 1)  
momentum_signal=na.omit(momentum_signal)
Monthly=Monthly[-c(1:3),]


Fa = momentum_signal; Fb = NULL; Fc = NULL
#Specify the dimension of the sort - let's use quintiles
dimA = 0:10/10
# Run either the conditional or unconditional sort function (for univariate sorts there is no 
#difference)
XSMOM.output = conditional.sort(Fa=Fa,R.Forward=Monthly,dimA=dimA)
#Returns=XSMOM.output$returns

# Let's now investigate the risk and return profiles of the sub-portfolios
#table.AnnualizedReturns(XSMOM.output$returns,scale = 365, geometric = FALSE)
portfolio.turnover(XSMOM.output)$`Mean Turnover`
portfolio.frequency(XSMOM.output, rank = 1)
portfolio.mean.size(XSMOM.output)

# Following the methodology of Jegadeesh and Titman, 1993, we will now form a long-short, 
# zero-cost portfolio which initiates a long position in the high momentum sub-portfolio 
# (portfolio 5) and a short position in the low momentum sub-portfolio (portfolio 1)

LS.Portfolio = XSMOM.output$returns[,10] + (-1*XSMOM.output$returns[,1])#WML
skewness(LS.Portfolio)
sum(LS.Portfolio)
# Investigate risk and return
table.AnnualizedReturns(LS.Portfolio,scale = 365, geometric = FALSE)
# We can now plot the back-tested results
chart.CumReturns(LS.Portfolio, geometric = FALSE, main = "XSMOM Long-Short Portfolio")

LS.Portfolio %>% 
  ggplot(aes(x=`10`)) +
  stat_density(geom = "line", alpha = 1) +
  ggtitle("WML(3,1)") +
  xlab("monthly returns") +
  ylab("distribution")

time(LS.Portfolio)<- time(LS.Portfolio) %>% as.yearmon() %>% as.Date()
LS.Portfolio=merge(FF[4:nrow(FF),], LS.Portfolio)
LS.Portfolio=LS.Portfolio[-nrow(LS.Portfolio),]
LS.Portfolio=LS.Portfolio[-nrow(LS.Portfolio),]
LS.Portfolio$X10=LS.Portfolio$X10-LS.Portfolio$RF

fit=lm(X10~MktxRF+SMB+HML, data = LS.Portfolio)
summary(fit)
vcovHAC_NW=vcovHAC(fit, weights = bwNeweyWest)
coeftest(fit, vcov. = vcovHAC_NW)
