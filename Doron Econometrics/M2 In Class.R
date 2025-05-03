library(car)
library(sandwich)
library(lmtest)
library(lfe)
library(plm)


#Data upload
CEOSAL1=read.table(file="CEOSAL1.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))
CEOSAL1=CEOSAL1[, c("V1", "V4", "V8", "V11")]
colnames(CEOSAL1)=c("SALARY", "ROE", "FIN_INDST", "LOG_SALARY")
head(CEOSAL1)

WAGE1=read.table(file="WAGE1.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))

Signals=read.csv(file="Signals_A_62016.csv", header=TRUE, as.is = TRUE, na.strings = c("NA",".",""))

#clear workspace
rm(list=ls(all.names = TRUE))
##