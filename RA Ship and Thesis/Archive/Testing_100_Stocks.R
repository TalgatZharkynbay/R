options(scipen = 10, digits = 3)
library(tidyverse)
library(quantmod)
library(highcharter)
library(tidyquant)
library(readxl)
library(writexl)
##############################################################
Tickers <- read_excel("C:/Users/talga/Desktop/Longers_2012Q4_100Stocks.xlsx")
Symbols=Tickers$Ticker

Prices <-
  getSymbols(Symbols,
             src = 'yahoo',
             from = "2012-02-14",
             to = "2013-05-15",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) 

Prices[is.na(Prices)] = 0

Prices$Talgat=rowMeans(Prices)

Talgat_Mega_Port_EW=Prices$Talgat

getSymbols("^GSPC",
           src = 'yahoo',
           from = "2012-02-14",
           to = "2013-05-15",
           auto.assign = TRUE,
           warnings = FALSE) 

GSPC=GSPC$GSPC.Adjusted

Talgat_Mega_Port_EW$Market=GSPC$GSPC.Adjusted

# getSymbols("SPY",
#            src = 'yahoo',
#            from = "2020-05-15",
#            to = "2020-08-14",
#            auto.assign = TRUE,
#            warnings = FALSE) 


#Talgat_Mega_Port_EW$Market_Dividens=SPY$SPY.Adjusted
############################################################################
Talgat_Mega_Port_EW$Talgat_IDX=Talgat_Mega_Port_EW$Talgat/
  as.numeric(Talgat_Mega_Port_EW$Talgat[1])

Talgat_Mega_Port_EW$Market_IDX=Talgat_Mega_Port_EW$Market/
  as.numeric(Talgat_Mega_Port_EW$Market[1])

#Talgat_Mega_Port_EW$Market_DIV_IDX=Talgat_Mega_Port_EW$Market_Dividens/
  #as.numeric(Talgat_Mega_Port_EW$Market_Dividens[1])

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
