options(scipen = 10, digits = 3)
library(tidyverse)
library(quantmod)
library(highcharter)
library(tidyquant)
library(readxl)
library(writexl)
library(ggplot2)
#################################################
setwd("C:/Users/talga/Desktop/Masters/RAship/Data2")

file.list <- list.files(pattern='*.xlsx')
df.list <- lapply(file.list, read_excel)

for (i in 1:20) {
  df.list[[i]]$Date=ymd(df.list[[i]]$Date)
}

for (i in 1:20) {
  assign(paste(gsub("\\..*","", file.list[i])), xts(df.list[[i]][,2],
                                                    order.by = df.list[[i]]$Date))
}

##### Sharpe Ratios and Rolling Sharpe #############
#Conatus=last(Conatus, 756)

Market <-
  getSymbols("SPY",
             src = 'yahoo',
             from = "2013-06-30",
             to = "2016-06-30",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`("SPY") 

Market <- Market %>%
  Return.calculate(
    method = "log") %>%
  na.omit()


JOHO=JOHO["2013-11-11/"]
rf=0.26/100
Ret=mean(Market)*252
sd=sd(Market)*sqrt(252)
Sharpe=(Ret-rf)/sd;Sharpe




#Just the returns
highchart(type = "stock") %>%
  hc_title(text = "Daily Log Returns") %>%
  hc_add_series(SlatePath,
                name = "Slate Path") %>%
  hc_add_series(GREENHAVEN,
                name = "Green Haven") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = FALSE) %>%
  hc_legend(enabled = TRUE)

#Histograms and shit
hc_hist <- hist(SlatePath,
                breaks = 50,
                plot = FALSE)


hchart(hc_hist, color = "cornflowerblue") %>%
  hc_title(text = "Slate Path") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = FALSE) %>%
  hc_legend(enabled = FALSE)

#Risks, rolling SDs, etc
# SlatePath_SD <-
#   StdDev(SlatePath)

window <- 30

SlatePath_Rolling_SD <-
  rollapply(SlatePath,
            FUN = sd,
            width = window) %>%
  na.omit() %>%
  `colnames<-`("SlatePath Rolling SD")

SlatePath_Rolling_SD_HC <-
  round(SlatePath_Rolling_SD, 4) * 100

GreenStreet_Rolling_SD <-
  rollapply(GreenStreet,
            FUN = sd,
            width = window) %>%
  na.omit() %>%
  `colnames<-`("GreenStreet Rolling SD")

GreenStreet_Rolling_SD_HC <-
  round(GreenStreet_Rolling_SD, 4) * 100

highchart(type = "stock") %>%
  hc_title(text = "30-Day Rolling Volatility") %>%
  hc_add_series(SlatePath_Rolling_SD_HC, name="Slate Path") %>%
  hc_add_series(GreenStreet_Rolling_SD_HC, name="Green Street") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"),
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled= FALSE) %>%
  hc_legend(enabled = TRUE)