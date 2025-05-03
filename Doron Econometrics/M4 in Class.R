FERTIL=read.table(file="FERTIL.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))
colnames(FERTIL)=c("GFR", "PE", "YEAR")
FERTIL$WW2=ifelse(FERTIL$YEAR>=1941 & FERTIL$YEAR<=1945, 1, 0)
FERTIL$PILL=ifelse(FERTIL$YEAR>=1963,1,0)
require(Hmisc)
FERTIL=FERTIL[order(FERTIL$YEAR), ]
FERTIL$PE_Lag1=Lag(FERTIL$PE,  1)
FERTIL$PE_Lag2=Lag(FERTIL$PE,  2)

HSEINV=read.table(file="HSEINV.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))
colnames(HSEINV)=c("Year", "Inv", "Pop", "Price")

HSEINV$t=1:length(HSEINV$Year)#time trend

View(HSEINV)
