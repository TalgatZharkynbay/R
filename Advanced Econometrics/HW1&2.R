options(digits = 3)
library(quantmod)
library(readr)
library(lubridate)
library(highcharter)
library(stats)
library(fBasics)
library(ggplot2)
library(reshape2)
library(tidyquant)

#Exercise:
ibm <- read_table2("d-ibmvwewsp6203.txt", 
                               col_names = FALSE,)

ibm=ibm[, 1:2]
colnames(ibm)=c("Date", "Simple_Ret")
ibm$Log_Ret=log(1+ibm$Simple_Ret)
ibm$Date=ymd(ibm$Date)


ibm=xts(ibm[, 2:3], order.by = ibm$Date)
ibm=ibm["1962/1997"]

highchart(type = "stock") %>%
  hc_title(text = "ibm Daily Returns") %>%
  hc_add_series(ibm$Simple_Ret,
                name = "Simple") %>%
  hc_add_series(ibm$Log_Ret,
                name = "Log") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

getSymbols("IBM",
               src = 'yahoo',
               from = "1962-07-03",
               to = "2017-12-31")

IBM=IBM$IBM.Adjusted
IBM$Log_Ret=diff(log(IBM$IBM.Adjusted))
IBM$Simple_Ret=exp(IBM$Log_Ret)-1

highchart(type = "stock") %>%
  hc_title(text = "ibm Daily Returns") %>%
  hc_add_series(IBM$Simple_Ret,
                name = "Simple") %>%
  hc_add_series(IBM$Log_Ret,
                name = "Log") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

#Exercise I:
ftse <- read_table2("ftse.txt")
head(ftse)

ftse_Log_Ret=diff(log(ftse$INDEX))
ftse_Simple_Ret=exp(ftse_Log_Ret)-1

head(ftse_Log_Ret)
head(ftse_Simple_Ret)

y_bar=mean(ftse_Log_Ret)
y_sd=sd(ftse_Log_Ret)
r_bar=mean(ftse_Simple_Ret)

exp(mean(ftse_Log_Ret)+0.5*(sd(ftse_Log_Ret))^2)-1
log(1+mean(ftse_Simple_Ret))-0.5*(sd(ftse_Log_Ret))^2
#Roughly the same at ex-ante if log returns are assumed to be normal
#From ex-post perspective if t=311, the index at t is given by:
t=311
P_t=ftse[1,2]*exp(y_bar*t)

P_t;ftse[312,2]

p_t=ftse[1,2]*(1+r_bar)^t
p_t

#Exercise 4:
USD_KZT<- read_csv("USD_KZT Historical Data.csv")
View(USD_KZT)
USD_KZT=USD_KZT[, 1:2]
USD_KZT$Date=mdy(USD_KZT$Date)
USD_KZT=xts(USD_KZT[,2], order.by = USD_KZT$Date)
USD_KZT_Log_Ret=na.omit(diff(log(USD_KZT$Price)))
colnames(USD_KZT_Log_Ret)=c("Log_Ret")

basicStats(USD_KZT_Log_Ret)

T=length(USD_KZT_Log_Ret)
s4=kurtosis(USD_KZT_Log_Ret$Log_Ret)
t4=s4/sqrt(24/T)
t4 #reject zero kurtosis


ggplot(USD_KZT_Log_Ret, aes(x = USD_KZT_Log_Ret$Log_Ret)) + 
  geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, args = list(mean = mean(USD_KZT_Log_Ret$Log_Ret), 
                                         sd = sd(USD_KZT_Log_Ret$Log_Ret)))

qqnorm(USD_KZT_Log_Ret, ylab = "Returns")

f=density(USD_KZT_Log_Ret, kernel = "epanechnikov")

plot(f)
##################################################################
USD_KZT_Log_Ret_Monthly <- to.monthly(USD_KZT_Log_Ret,
                             indexAt = "lastof",
                             OHLC = FALSE)

basicStats(USD_KZT_Log_Ret_Monthly)

ggplot(USD_KZT_Log_Ret_Monthly, aes(x = USD_KZT_Log_Ret_Monthly$Log_Ret)) + 
  geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, args = list(mean = mean(USD_KZT_Log_Ret_Monthly$Log_Ret), 
                                         sd = sd(USD_KZT_Log_Ret_Monthly$Log_Ret)))



T=length(USD_KZT_Log_Ret_Monthly)
s4=kurtosis(USD_KZT_Log_Ret_Monthly$Log_Ret)
t4=s4/sqrt(24/T)
t4 #reject zero kurtosis


qqnorm(USD_KZT_Log_Ret_Monthly, ylab = "Returns")

g=density(USD_KZT_Log_Ret_Monthly, kernel = "epanechnikov")

plot(g)

######################################################
#Homework2:
#Exercise 1:
nflx <- read_table2("d-nflx3dx0913.txt")
View(nflx)
basicStats(nflx$nflx)
basicStats(nflx$vwretd)
basicStats(nflx$ewretd)
basicStats(nflx$sprtrn)

nflx_log=log(1+nflx[, 3:6])
basicStats(nflx_log$nflx)
basicStats(nflx_log$vwretd)
basicStats(nflx_log$ewretd)
basicStats(nflx_log$sprtrn)

mean_nflx_log=mean(nflx_log$nflx)
st_error=sd(nflx_log$nflx)/sqrt(nrow(nflx_log))
t_statistic=mean_nflx_log/st_error

t_statistic
qt(0.975, df=nrow(nflx_log)-1)

ggplot(melt(nflx_log[, c(1,4)]),aes(x=value, fill=variable)) + 
  geom_density(alpha=0.25)

#Exercise 2:
ge <- read_table2("m-ge3dx8113.txt")
View(ge)
basicStats(ge$ge)
basicStats(ge$vwretd)
basicStats(ge$ewretd)
basicStats(ge$sprtrn)

ge_log=log(1+ge[, 3:6])
basicStats(ge_log$ge)
basicStats(ge_log$vwretd)
basicStats(ge_log$ewretd)
basicStats(ge_log$sprtrn)

mean_ge_log=mean(ge_log$ge)
st_error=sd(ge_log$ge)/sqrt(nrow(ge_log))
t_statistic=mean_ge_log/st_error
t_statistic
qt(0.975, df=nrow(ge_log)-1)

ggplot(melt(ge_log[, c(1,4)]),aes(x=value, fill=variable)) + 
  geom_density(alpha=0.25)

#Exercise 3:
t.test(ge_log$ge, alternative = c("two.sided"), conf.level = 0.95,
       mu=0)

s3=skewness(ge_log$ge)
T=length(ge_log$ge)
t3=s3/sqrt(6/T)
t3
pp=2*(pnorm(t3))
pp #reject zero skewness
#kurtosis
s4=kurtosis(ge_log$ge)
t4=s4/sqrt(24/T)
t4 #reject zero kurtosis

#Exercise 4 already done before
#Exercise 5:
us_jap <- read_table2("d-exjpus.txt")
View(us_jap)

us_jap=us_jap$fx[us_jap$fx!=0]

us_jap_Log_Ret=data.frame(diff(log(us_jap)))

colnames(us_jap_Log_Ret)=c("LR")

basicStats(us_jap_Log_Ret)

ggplot(us_jap_Log_Ret, aes(x = us_jap_Log_Ret$LR)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density()

#Exercise 6:
symbols <- c("DJI")

Dow <-
  getSymbols(symbols,
             src = 'yahoo',
             from = "2006-04-20",
             to = "2016-04-20",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

Dow_Returns <-
  Return.calculate(Dow,
                   method = "log") %>%
  na.omit()

chartSeries(Dow_Returns, theme = "white")

basicStats(Dow_Returns)
