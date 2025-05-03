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
###########################################################################
setwd("C:/Users/talga/Desktop/Thesis/13F Raw Data")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

for (i in 1:31) {
  df.list[[i]]$stock_id=gsub("\\..*","", df.list[[i]]$stock_id)
}

# Data=read_csv("C:/Users/talga/Desktop/Thesis/13F Raw Data/2012Q4.csv")
# Data$stock_id=gsub("\\..*","", Data$stock_id)

for (i in 1:31) {
  assign(paste(gsub("\\..*","", file.list[i])), na.omit(df.list[[i]]%>%
                                                          group_by(name, stock_id)%>%
                                                          summarise(total_value=sum(total_value), total_count=sum(total_count), n = n())))

}

# Data=na.omit(Data%>%
#   group_by(name, stock_id)%>%
#   summarise(total_value=sum(total_value), total_count=sum(total_count), n = n()))


dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) #all in one, pretty cool

output_csv <- function(data, names){
  folder_path <- "C:/Users/talga/Desktop/Thesis/Unique Stocks and Number/"

  write_csv(data, paste0(folder_path, "", names, ".csv"))
}

list(data = dfs,
     names = names(dfs)) %>%
  purrr::pmap(output_csv)

