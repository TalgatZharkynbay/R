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
####################################################################################
Data <- na.omit(read_csv("C:/Users/talga/Desktop/Masters/RAship/Second/RAW/2020Q2.csv"))
#Unique_Stocks=unique(Data$name)

Stocks<-Data %>%
  group_by(name, stock_id) %>%
  summarise(total_count = sum(total_count), total_value=sum(total_value))%>%
  mutate(stock_id=gsub("\\..*","",stock_id))

write_xlsx(Stocks, "C:/Users/talga/Desktop/Thesis/Unique Stocks/UniqueStocks_2020Q2.xlsx")


#############################################################################################
Least_Holdings=data.frame(sort(table(Data$company_name),decreasing=FALSE)[1000:2000])
Least_Names=as.character(Least_Holdings$Var1)
Longers=Data[Data$company_name %in% Least_Names,]
Longers_Cusip=data.frame(sort(table(Longers$cusip_id),
                              decreasing=TRUE)[1:100])
Longers_Cusip=as.data.frame(Longers_Cusip[, -2])
colnames(Longers_Cusip)=c("CUSIP")
Cusips=read_excel("C:/Users/talga/Desktop/Masters/RAship/Second/CLEAN/Cusip_Clean.xlsx")
Longers_Cusip=merge(Longers_Cusip, Cusips, by="CUSIP")

#write_xlsx(Least_Holdings, "C:/Users/talga/Desktop/Least_Holdings_2014Q4.xlsx")
#write_xlsx(Longers, "C:/Users/talga/Desktop/Longers_2020Q2_1000.xlsx")
write_xlsx(Longers_Cusip, "C:/Users/talga/Desktop/Consensus_2014Q4.xlsx")

Symbols=Longers_Cusip$RIC

Prices <-
  getSymbols(Symbols,
             src = 'yahoo',
             from = "2015-02-14",
             to = "2015-05-14",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) 

#Prices[is.na(Prices)] = 0
Prices$Talgat=rowMeans(Prices)
Portfolio=as.data.table(Prices)
write_xlsx(Portfolio, "C:/Users/talga/Desktop/Portfolio_2014Q4.xlsx")

Talgat_Mega_Port_EW=Prices$Talgat
getSymbols("^GSPC",
           src = 'yahoo',
           from = "2015-02-14",
           to = "2015-05-14",
           auto.assign = TRUE,
           warnings = FALSE) 
GSPC=GSPC$GSPC.Adjusted
Talgat_Mega_Port_EW$Market=GSPC$GSPC.Adjusted
Talgat_Mega_Port_EW$Talgat_IDX=Talgat_Mega_Port_EW$Talgat/
  as.numeric(Talgat_Mega_Port_EW$Talgat[1])
Talgat_Mega_Port_EW$Market_IDX=Talgat_Mega_Port_EW$Market/
  as.numeric(Talgat_Mega_Port_EW$Market[1])

highchart(type = "stock") %>%
  hc_title(text = "Testing the idea of Long Players") %>%
  hc_add_series(Talgat_Mega_Port_EW$Talgat_IDX,
                name = "Talgat") %>%
  hc_add_series(Talgat_Mega_Port_EW$Market_IDX,
                name = "Market") %>%
  #hc_add_series(Talgat_Mega_Port_EW$Market_DIV_IDX,
  #name = "Market with dividends") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)




