options(scipen = 10, digits = 3)
library(tidyverse)
library(quantmod)
library(highcharter)
library(tidyquant)
library(readxl)
library(writexl)
#######################################################################################

Data=read_xlsx("WW_25.xlsx")
#Data$Date= as.Date(as.yearqtr(Data$Date, format='%YQ%q'))

Data$Date= as.Date(as.yearqtr(Data$Date, format='%YQ%q'), frac = 1)+45
#Data=xts(Data[2:ncol(Data)], order.by = Data$Date)
Time=unique(Data$Date)
Name=unique(Data$Name)

for (k in 1:19) {
  for (j in 1:16) {
    for (i in first(which(Data$Date == Time[j] & Data$Name == Name[k])):
         last(which(Data$Date == Time[j] & Data$Name == Name[k]))) 
      {
      Data$VW[i]=Data$V[i]/sum(Data$V[first(which(Data$Date == Time[j] & Data$Name == Name[k])):
                                        last(which(Data$Date == Time[j] & Data$Name == Name[k]))])
    }
  }
}

##########################################################
listofdfs <- list()
Weights=list()
for (i in 1:15) {
  symbols <- Data$Symbol[first(which(Data$Date == Time[i] & Data$Name == Name[19])):
                           last(which(Data$Date == Time[i] & Data$Name == Name[19]))]
  listofdfs[[i]]= getSymbols(symbols,src = 'yahoo',
                             from = Data$Date[first(which(Data$Date == Time[i] & Data$Name == Name[1]))],
                             to = Data$Date[first(which(Data$Date == Time[i+1] & Data$Name == Name[1]))], 
                             auto.assign = TRUE,
                             warnings = FALSE) %>%
    map(~Ad(get(.))) %>%
    reduce(merge) %>%
    Return.calculate(method = "log") %>%
    na.omit() %>%
    setNames(gsub("\\..*","", names(.)))
  Weights[[i]]=subset(Data[first(which(Data$Date == Time[i] & Data$Name == Name[19])):
                            last(which(Data$Date == Time[i] & Data$Name == Name[19])),],
                     Symbol %in% colnames(listofdfs[[i]]))
  assign(paste("Port", i, sep = "."),
         data.frame(rowSums(data.frame(t(t(listofdfs[[i]])*Weights[[i]]$VW)))))
}

#################################################################

Final=rbind(Port.1, Port.2, Port.3, Port.4, Port.5, Port.6, Port.7, Port.8,
           Port.9, Port.10, Port.11, Port.12, Port.13, Port.14, Port.15)

colnames(Final)=c(Name[19])

Final=rownames_to_column(Final, var = "Date")

write_xlsx(Final, "C:/Users/talga/Desktop/Masters/RAship/Data2/WhaleRock.xlsx")




# symbols <- Data$Symbol[1:25]
# 
# Port_1 <-
#   getSymbols(symbols,
#              src = 'yahoo',
#              from = Data$Date[1],
#              to = Data$Date[25+1],
#              auto.assign = TRUE,
#              warnings = FALSE) %>%
#   map(~Ad(get(.))) %>%
#   reduce(merge) %>%
#   Return.calculate(method = "log") %>%
#   na.omit() %>%
#   setNames(gsub("\\..*","", names(.)))
# 
# 
# Test=subset(Data[1:25,], Symbol %in% colnames(Port_1))
# 
# Adelante_1=data.frame(t(t(Port_1)*Test$VW))
# 
# Adelante_1_C=data.frame(rowSums(Adelante_1))




