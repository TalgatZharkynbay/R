library(tidyverse)
library(quantmod)
library(quadprog)
library(highcharter)
library(tidyquant)

# Market Index
symbols=c("SPY", "MSFT", "AAPL", "TSLA", "VRTX")
#symbols=c("NFLX", "AAPL", "TSLA", "IBM", "SPY")


Market <-
  getSymbols(symbols,
             src = 'yahoo',
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

Market_monthly <- to.monthly(Market,
                             indexAt = "lastof",
                             OHLC = FALSE)
Returns_Market <-
  Return.calculate(Market_monthly,
                   method = "log") %>%
  na.omit()


# # Bonds
# Bonds <-
#   getSymbols("FBNDX",
#              src = 'yahoo',
#              from = "2010-12-31",
#              auto.assign = TRUE,
#              warnings = FALSE) %>%
#   na.omit()%>%
#   map(~Ad(get(.))) %>%
#   reduce(merge) %>%
#   `colnames<-`("FBNDX")
# 
# Bonds_monthly <- to.monthly(Bonds,
#                             indexAt = "lastof",
#                             OHLC = FALSE)
# Returns_Bonds <-
#   Return.calculate(Bonds_monthly,
#                    method = "log") %>%
#   na.omit()

# Risk-free:
getSymbols('DGS3MO', src = "FRED")

Returns_Risk_Free=DGS3MO[nrow(DGS3MO)]/12/100



#Optimal Risky:

Returns_DE=Returns_Market

DE_VCOV=cov(Returns_DE)

DE_Mean=matrix(apply(Returns_DE,2,mean))

rownames(DE_Mean)<-symbols

colnames(DE_Mean)<-c("DE_Mean")

DE_Min<-min(DE_Mean)

DE_Max<-max(DE_Mean)

increments=1000

# DE_Target<-seq(DE_Min,DE_Max,
#                length=increments)

DE_Target<-seq(DE_Min,2*DE_Max,
                length=increments)

DE_Target_SD<-rep(0,length=increments)

DE_Weights<-matrix(0,nrow=increments,
                   ncol=length(DE_Mean))
for (i in 1:increments){
  Dmat<-2*DE_VCOV
  
  dvec<-c(rep(0,length(DE_Mean)))
  
  # Amat<-cbind(rep(1,length(DE_Mean)),DE_Mean,
  #             diag(1,nrow=ncol(Returns_DE)))
  Amat<-cbind(rep(1,length(DE_Mean)),DE_Mean)
  
  #bvec<-c(1,DE_Target[i],rep(0,ncol(Returns_DE)))
  
  bvec<-c(1,DE_Target[i])
  
  soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
  
  DE_Target_SD[i]<-sqrt(soln$value)
  DE_Weights[i,]<-soln$solution}

colnames(DE_Weights)<-symbols

DE_Portfolio<-data.frame(cbind(DE_Target,
                               DE_Target_SD,
                               DE_Weights))
DE_Portfolio=DE_Portfolio%>%
  filter(DE_Target_SD!=0)


DE_Min_Var<-subset(DE_Portfolio,
                   DE_Portfolio$DE_Target_SD
                   ==min(DE_Portfolio$DE_Target_SD))

DE_Portfolio$Sharpe<-(DE_Portfolio$DE_Target-
                        as.vector(Returns_Risk_Free))/
  DE_Portfolio$DE_Target_SD

DE_Tangency<-subset(DE_Portfolio,
                    DE_Portfolio$Sharpe==
                      max(DE_Portfolio$Sharpe))


DE_Complete=rbind(DE_Tangency, 
                  c(as.numeric(Returns_Risk_Free)*12, 
                    rep(0, ncol(DE_Tangency)-1))
                  )

# Complete Portfolio

A=8

w=subset(DE_Tangency, select=symbols)

E_Rp=sum(colMeans(Returns_DE)*w)

SD=StdDev(Returns_DE, weights = as.numeric(w))

Risk_Weight=as.numeric((E_Rp-Returns_Risk_Free*12)/(A*SD^2))

Riskless_Weight=1-Risk_Weight

Risk_Weight; Riskless_Weight

Complete_Portfolio_Ret=as.numeric(Risk_Weight*sum(colMeans(Returns_DE)*w)+
  Riskless_Weight*Returns_Risk_Free*12)

Complete_Portfolio_SD=sqrt((Risk_Weight^2)*(SD^2))

Portfolio_SR=as.numeric((Complete_Portfolio_Ret-Returns_Risk_Free*12)/
  Complete_Portfolio_SD)


# Market_SR <-SharpeRatio(Returns_Market$SPY,
#                               Rf = Returns_Risk_Free/12/100,
#                               FUN = "StdDev") %>%
#   `colnames<-`("Market_SR")


# colnames(Portfolio_SR)=c("Portfolio_SR")


Complete_Portfolio=data.frame(cbind(Complete_Portfolio_Ret,
                         Complete_Portfolio_SD))

# A better plot of EF:

highchart() %>%
  hc_add_series(DE_Portfolio, "line", 
       hcaes(x = DE_Target_SD, y = DE_Target)
      , name="Efficient Frontier")%>%
  hc_yAxis(title = list(text = "Return"))%>%
  hc_xAxis(title = list(text = "Risk"))%>%
  hc_add_series(DE_Min_Var, "line", 
                hcaes(x = DE_Min_Var$DE_Target_SD, 
                      y = DE_Min_Var$DE_Target)
                ,name="Minimum Variance Portfolio")%>%
  hc_add_series(DE_Tangency, "line", 
                hcaes(x = DE_Tangency$DE_Target_SD, 
                      y = DE_Tangency$DE_Target)
                ,name="Tangency Portfolio")%>%
  hc_add_series(DE_Complete, "line", 
                hcaes(x = DE_Complete$DE_Target_SD, 
                      y = DE_Complete$DE_Target)
                ,name="Capital Allocation Line")%>%
  hc_add_series(Complete_Portfolio, "line", 
                hcaes(x = Complete_Portfolio$V2, 
                      y = Complete_Portfolio$Complete_Portfolio_Ret)
                ,name="Complete Portfolio")

############### Weights ###########

w=cbind(Riskless_Weight, Risk_Weight, w, Portfolio_SR)

w=round(w, 2)
