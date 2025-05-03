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

setwd("C:/Users/talga/Desktop/Thesis/Unique Stocks")

file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

# for (i in 1:2) {
#   assign(paste(gsub("\\..*","", file.list[i])), df.list[[i]])
# }

for (i in 1:30) {
  assign(paste(file.list[i]),merge(df.list[[i]], df.list[[i+1]], by="name")%>%
           mutate(Delta_N_ABS=abs(n.y-n.x)))
}

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) #all in one, pretty cool

# Test=list()
# for (i in 1:30) {
#     Test[[i]]=dfs[[i]]%>%
#     filter(!Bought_Sold==0)
# }


output_csv <- function(data, names){
  folder_path <- "C:/Users/talga/Desktop/Thesis/Number ABS/"

  write_csv(data, paste0(folder_path, "", names, ".csv"))
}


list(data = dfs,
     names = names(dfs)) %>%
  purrr::pmap(output_csv)

# dfs %>% ### wrote all dfs in one excel file
#   writexl::write_xlsx(path = "C:/Users/talga/Desktop/Thesis/Unique Stocks Differences Between Quarters/test-excel.xlsx")

############################## Fraction and COMPUSTAT #################################
Compustat=read.csv("C:/Users/talga/Desktop/Thesis/COMPUSTAT.csv")
Compustat=Compustat[, c(2,9, 10, 11, 12, 14, 15, 17)]
Compustat=Compustat%>%
  filter(curcdq=="USD")
Test=na.locf(Compustat)
Test=Test%>%
  filter(cshoq!=0)
Test=Test[,-c(1,4)]
Test=Test%>%
  filter(tic!="")
Test$cshoq=Test$cshoq*1000
Test$Book=Test$ceqq
Test=Test[,-4]
Test$Size=Test$cshoq*Test$prccq
Test$BTM=Test$Book/Test$Size
write_xlsx(Test, "C:/Users/talga/Desktop/COMPUSTAT_CLEAN.xlsx")


######################################################################################
Compustat=read_excel("C:/Users/talga/Desktop/Thesis/COMPUSTAT_CLEAN.xlsx")
setwd("C:/Users/talga/Desktop/Thesis/Unique Stocks")
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)

Test2=list()
Test3=list()
Test=df.list[[1]]

for (i in 1:31) {
  df.list[[i]]=df.list[[i]][order(-df.list[[i]]$total_value),]
}

for (i in 1:31) {
  df.list[[i]]=df.list[[i]][c(1:50),c(1,2,3)]
}


for (i in 1:31) {
  df.list[[i]]$qtr=gsub("\\..*","", file.list[i])
  df.list[[i]]$tic=df.list[[i]]$stock_id
  Test2[[i]]=Compustat[Compustat$tic %in% df.list[[i]]$stock_id & Compustat$datacqtr %in% df.list[[i]]$qtr
                  , c(1,4,6,7,8)] 
  Test3[[i]]=merge(df.list[[i]][, 3:7], Test2[[i]], by="tic")
}

for (i in 1:31) {
  Test3[[i]]$fraction=Test3[[i]]$total_count/Test3[[i]]$cshoq
}


dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) #all in one, pretty cool


list(data = df.list,
     names = names(df.list)) %>%
  purrr::pmap(output_csv)

df.list %>% ### wrote all dfs in one excel file
  writexl::write_xlsx(path = "C:/Users/talga/Desktop/Top50/Top50.xlsx")


output_csv <- function(data, names){
  folder_path <- "C:/Users/talga/Desktop/Top50/"
  
  write_csv(data, paste0(folder_path, "", names, ".csv"))
}


list(data = df.list,
     names = gsub("\\..*","", file.list)) %>%
  purrr::pmap(output_csv)

#############################################################################################
setwd("C:/Users/talga/Desktop/Thesis/Unique Stocks and Many Others")

file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_csv)


for (i in 1:30) {
    assign(paste(file.list[i]),merge(df.list[[i]], df.list[[i+1]], by="tic")%>%
             mutate(Delta_N_ABS=abs(n.y-n.x))%>%
            mutate(Delta_F_ABS=abs(fraction.y-fraction.x)))
}

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) #all in one, pretty cool

output_csv <- function(data, names){
  folder_path <- "C:/Users/talga/Desktop/Thesis/ABS Delta Number and  ABS Delta Fraction/"
  
  write_csv(data, paste0(folder_path, "", names, ".csv"))
}

list(data = dfs,
     names = gsub("\\..*","", file.list[1:30])) %>%
  purrr::pmap(output_csv)
