library(tidyverse)
library(quantmod)
library(quadprog)
library(highcharter)
library(tidyquant)
# Load the Aggressive

symbols=c("VRTX","TSLA", "VRSN", "BAT","AAPL")

Prices <-
  getSymbols(symbols,
             src = 'yahoo',
             from="2010-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

# Returns
Prices_monthly <- to.monthly(Prices,
                             indexAt = "lastof",
                             OHLC = FALSE)
Returns_Aggressive <-
  Return.calculate(Prices_monthly,
                   method = "log") %>%
  na.omit()

# # Market
# Market <-
#   getSymbols("SPY",
#              src = 'yahoo',
#              from = "2010-12-31",
#              auto.assign = TRUE,
#              warnings = FALSE) %>%
#   map(~Ad(get(.))) %>%
#   reduce(merge) %>%
#   `colnames<-`("SPY")
# 
# Market_monthly <- to.monthly(Market,
#                              indexAt = "lastof",
#                              OHLC = FALSE)
# Returns_Market <-
#   Return.calculate(Market_monthly,
#                    method = "log") %>%
#   na.omit()


# Bonds
Bonds <-
  getSymbols("FBNDX",
             src = 'yahoo',
             from = "2010-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  na.omit()%>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`("FBNDX")

Bonds_monthly <- to.monthly(Bonds,
                             indexAt = "lastof",
                             OHLC = FALSE)
Returns_Bonds <-
  Return.calculate(Bonds_monthly,
                   method = "log") %>%
  na.omit()

# Risk-free:
getSymbols('DGS3MO', src = "FRED")

Returns_Risk_Free=DGS3MO[nrow(DGS3MO)]/12

# Equity Investment
Aggressive_VCOV=cov(Returns_Aggressive)

Aggressive_Mean=matrix(apply(Returns_Aggressive,2,mean))

rownames(Aggressive_Mean)<-symbols

colnames(Aggressive_Mean)<-c("Aggressive_Mean")

Aggressive_Min<-min(Aggressive_Mean)

Aggressive_Max<-max(Aggressive_Mean)

increments=100

Aggressive_Target<-seq(Aggressive_Min,Aggressive_Max,
                       length=increments)

Aggressive_Target_SD<-rep(0,length=increments)

Aggressive_Weights<-matrix(0,nrow=increments,
                           ncol=length(Aggressive_Mean))
for (i in 1:increments){
   Dmat<-2*Aggressive_VCOV
   
   dvec<-c(rep(0,length(Aggressive_Mean)))
   
   Amat<-cbind(rep(1,length(Aggressive_Mean)),Aggressive_Mean,
                 diag(1,nrow=ncol(Returns_Aggressive)))
   
   bvec<-c(1,Aggressive_Target[i],rep(0,ncol(Returns_Aggressive)))
   
   soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
   
   Aggressive_Target_SD[i]<-sqrt(soln$value)
   Aggressive_Weights[i,]<-soln$solution}

colnames(Aggressive_Weights)<-symbols

Aggressive_Portfolio<-data.frame(cbind(Aggressive_Target,
                                      Aggressive_Target_SD,
                                      Aggressive_Weights))

Aggressive_Min_Var<-subset(Aggressive_Portfolio,
                           Aggressive_Portfolio$Aggressive_Target_SD
                           ==min(Aggressive_Portfolio$Aggressive_Target_SD))

Aggressive_Portfolio$Sharpe<-(Aggressive_Portfolio$Aggressive_Target-
                               as.vector(Returns_Risk_Free))/
  Aggressive_Portfolio$Aggressive_Target_SD

Aggressive_Tangency<-subset(Aggressive_Portfolio,
                            Aggressive_Portfolio$Sharpe==
                              max(Aggressive_Portfolio$Sharpe))


# Debt and Equity Investement
