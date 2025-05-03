library(ggplot2)
library(quantmod)
library(tidyquant)
library(dplyr)
library(data.table)

Yield=c("DGS3MO","DGS6MO","DGS1", "DGS2", "DGS3", "DGS5",  
        "DGS7",  
        "DGS10",  
        "DGS20",  
        "DGS30")

getSymbols(Yield, src = "FRED")


treasury=na.omit(merge(DGS3MO,DGS6MO,DGS1, DGS2, DGS3, DGS5,  
                        DGS7,  
                        DGS10,  
                        DGS20,  
                        DGS30))


treasury=treasury["2000-11-24"]

par(mfcol=c(2,1))

treasury=t(treasury)

row.names(treasury)=c(0.25,0.5,1,2,3,5,7,10,20,30)

plot(treasury, type = "l", xlab = "TTM", ylab = "Yields")



#treasury=as.data.frame(t(na.omit(merge(DGS3MO,DGS6MO,DGS1, DGS2, DGS3, DGS5,  
                                       #DGS7,  
                                      # DGS10,  
                                      # DGS20,  
                                       #DGS30))))

#Test=na.omit(tq_get(c("DGS3MO","DGS6MO","DGS1", "DGS2", "DGS3", "DGS5",  
              # "DGS7",  
              # "DGS10",  
              # "DGS20",  
              # "DGS30"), get = "economic.data"))
  

#Test_1=dcast(Test, 
     # symbol ~ rowid(symbol, prefix = "hz"), 
     # value.var = "price")



#Test_1=Test %>%
  #group_by(symbol) %>%
  #mutate(Order = seq_along(symbol)) %>%
  #spread(key = Order, value = price)


#ggplot(data = treasury, aes(x=as.numeric(row.names(treasury)), y=treasury$`2010-01-11`,
                            #group=1))+
  #geom_line()


