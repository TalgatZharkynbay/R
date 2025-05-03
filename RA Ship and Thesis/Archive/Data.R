library(httr)
library(jsonlite)

Test=GET("http://tools.valuefokus.de/api/investors-for-cusip/?cusip=46641Q613&qtr=2020Q1")

data=fromJSON(rawToChar(Test$content))

data$quarter

Test2=GET("http://tools.valuefokus.de/api/investors-for-cusip/?cusip=46641Q613&qtr=2018Q4")

data2=fromJSON(rawToChar(Test2$content))

data2$quarter

###################################################################

Test3=GET("http://tools.valuefokus.de/api/investors-for-cusip/?cusip=037833100&qtr=2020Q2")

data3=fromJSON(rawToChar(Test3$content))

data3$quarter

##################################################################

Test4=GET("http://tools.valuefokus.de/api/investors-for-cusip/?cusip=037833100&qtr=2020Q2&filer_name=UBS Group AG")

data4=fromJSON(rawToChar(Test4$content))

data4$quarter
