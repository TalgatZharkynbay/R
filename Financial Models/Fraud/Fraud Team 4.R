library(lubridate)
library(xts)
#Reading in the data:

Prices=scan()
length(Prices)

#Creating some time series with 90 days, except saturdays and sundays:

Imaginary_Dates=seq.Date(from = as.Date("2020-04-20"), 
                         to = as.Date("2021-04-20"), 
                         by = 1)

Imaginary_Dates=Imaginary_Dates[-seq(from=7, to=length(Imaginary_Dates), by=7)]

Imaginary_Dates=Imaginary_Dates[-seq(from=6, to=length(Imaginary_Dates), by=6)]

Imaginary_Dates=Imaginary_Dates[1:90]
  
Prices=xts(Prices, order.by = Imaginary_Dates); colnames(Prices)="Price"

plot(Prices)
########### Testing the fraud ##########
Returns=na.omit(diff(log(Prices)))

plot(Returns)

Tuesday=ifelse(.indexwday(Returns)==2,
                       1,
                       0)

Fraud_check=lm(Returns$Price~Tuesday)

summary(Fraud_check)

# We can conclude that there is insignificant effect of Tuesday trading
# on the stock returns. Hence, there is no evidence of fraud.