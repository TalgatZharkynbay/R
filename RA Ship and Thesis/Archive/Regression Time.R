options(scipen = 10, digits = 3)
library(tidyverse)
library(quantmod)
library(highcharter)
library(tidyquant)
library(readxl)
library(writexl)
library(ggplot2)
############################################################################
# getwd()
setwd("C:/Users/talga/Desktop/Masters/RAship/Data2")

file.list <- list.files(pattern='*.xlsx')
df.list <- lapply(file.list, read_excel)

for (i in 1:20) {
  assign(paste(gsub("\\..*","", file.list[i])), df.list[[i]])
}

#############################################################################
#Data=Reduce(function(x, y) merge(x, y, all=TRUE),
            #list(Adelante, Adirondack, Altimeter, AmericanAsset, DorsalCapital,
             #    GREENHAVEN, GreenStreet, Impala, ORBIMED, PARCAPITAL,
              #   PARUSFINANCE, PolarCapital, ResolutionCapital, SECURITYCAPITAL,
               #  SlatePath, Tekla))

#Data=na.omit(Data)

# write_xlsx(Data, "BetaData.xlsx")

#Data=read_excel("BetaData.xlsx")
#Data$Date=ymd(Data$Date)
#Data=xts(Data[2:17], order.by = Data$Date)

#Fama=read_excel("FamaDaily.xlsx")
#Fama$Date=ymd(Fama$Date)
#Fama=xts(Fama[2:5], order.by = Fama$Date)

#Final=na.omit(merge(Data, Fama))

#for (i in 1:16) {
  #Final[,i]=Final[,i]-Final[,20]
#}

#Final=Final[,1:19]

#Sixteen = colnames(Final[,1:16])

#FF_Alphas <- list()
# 
# for (i in seq_along(Sixteen)) {
#   eq <- paste(Sixteen[i],"~ Mkt.RF+SMB+HML")
#   fit <- lm(as.formula(eq), data= Final)
#   FF_Alphas[[i]]=fit$coefficients[1]
# }  
# 
# for (i in 1:16) {
#      assign(paste(Sixteen[i]), FF_Alphas[[i]])
# }

# 
# MyAlphas=data.frame(Adelante.Capital.Management.LLC, 
#                     Adirondack.Research...Management.Inc.,
#                     Altimeter.Capital.Management..LLC,
#                     Dorsal.Capital.Management..LLC,
#                     American.Asset.Management.Inc.,
#                     Green.Street.Investors..LLC,
#                     GREENHAVEN.ASSOCIATES.INC,
#                     Impala.Asset.Management.LLC,
#                     ORBIMED.ADVISORS.LLC,
#                     PAR.CAPITAL.MANAGEMENT.INC,
#                     PARUS.FINANCE..UK..Ltd,
#                     Polar.Capital.LLP,
#                     Resolution.Capital.Ltd,
#                     SECURITY.CAPITAL.RESEARCH...MANAGEMENT.INC,
#                     Slate.Path.Capital.LP,
#                     Tekla.Capital.Management.LLC)
# 
# MyAlphas=data.frame(t(MyAlphas))
# MyAlphas$t=rownames(MyAlphas)
# colnames(MyAlphas)=c("FF Alphas", "Company Name")
# write_xlsx(MyAlphas, "MyAlphas.xlsx")

FamaDaily$Date=ymd(FamaDaily$Date)
FamaDaily=xts(FamaDaily[2:5], order.by = FamaDaily$Date)

ORBIMED$Date=ymd(`Slate Path`$Date)
ORBIMED=xts(ORBIMED[,2], order.by = ORBIMED$Date)
ORBIMED=na.omit(merge(ORBIMED, FamaDaily))
ORBIMED[,1]=ORBIMED[,1]-ORBIMED[,5]
Fit=lm(ORBIMED[,1]~ORBIMED[,2])
Alpha=Fit$coefficients[1]
Alpha
