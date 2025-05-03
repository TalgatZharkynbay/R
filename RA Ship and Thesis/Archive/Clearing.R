library(writexl)
library(lubridate)
library(PerformanceAnalytics)
# Names=colnames(CUSIP_final_list)
# Newnames=gsub("\\..*","", Names)
# colnames(CUSIP_final_list)=Newnames
# rm(Names)
# rm(Newnames)
# 
# Prices=CUSIP_final_list[, -which(names(CUSIP_final_list) %in% c("#N/A", ""))]
# Prices=Prices[, colSums(is.na(Prices)) != nrow(Prices)]
# 
# Prices1=Prices[, -grep("Invalid RIC.", Prices)]
# Prices2=Prices1[, -grep("#N/A", Prices1)]
# rm(Prices1)


Names=colnames(Prices)
Newnames=gsub("\\..*","", Names)
colnames(Prices)=Newnames

Prices1=Prices[, !duplicated(colnames(Prices))]

write_xlsx(PricesNoDouplicate, "C:/Users/talga/Desktop/PricesNoDouplicate.xlsx")
##################################################################################
Returns=PricesNoDouplicate[-c(2262:nrow(PricesNoDouplicate)), ]
Returns_1=Returns[, which(colMeans(!is.na(Returns))>0.96)]
Returns_1=Returns_1[-c(2206:nrow(Returns_1)), ]
write_xlsx(Returns_1, "C:/Users/talga/Desktop/Prices_Even_Cleaner.xlsx")

Returns_xts=xts(Returns_1, order.by = Returns_1$Date)
Returns_xts=Returns_xts["2020/"]
write_xlsx(as.data.frame(Returns_xts), "C:/Users/talga/Desktop/Returns_xts.xlsx")

###################################################################################
Cusip_Clean=Cusip_only[, 3:4]
Cusip_Clean$RIC=gsub("\\..*","", Cusip_Clean$RIC)
Cusip_Clean=Cusip_Clean[Cusip_Clean$RIC != "0", ]
Cusip_Clean=na.omit(Cusip_Clean)
write_xlsx(Cusip_Clean, "C:/Users/talga/Desktop/Cusip_Clean.xlsx")

####################################################################################
Names=unique(X2012Q4$company_name)
Least_Holdings=data.frame(sort(table(X2012Q4$company_name),decreasing=FALSE)[1000:2000])
write_xlsx(Least_Holdings, "C:/Users/talga/Desktop/Least_Holdings_2020Q2.xlsx")
Least_Names=as.character(Least_Holdings$Var1)

Longers=X2012Q4[X2012Q4$company_name %in% Least_Names,]
write_xlsx(Longers, "C:/Users/talga/Desktop/Longers_2020Q2_1000.xlsx")

Longers_Cusip=data.frame(sort(table(Longers$cusip_id),
                               decreasing=TRUE)[1:100])

write_xlsx(Longers_Cusip, "C:/Users/talga/Desktop/Longers_2012Q4_100Stocks.xlsx")
