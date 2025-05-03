library(quantmod)
library(readxl)
library(lubridate)
library(fOptions)
library(dplyr)
library(dygraphs)
######### Importing and tailoring data ##########
data=read_excel("C:/Users/talga/Desktop/Masters/Financial Modelling/Class4/computer exercise 3 studio.xls", 
                col_names = TRUE, sheet = "Returns")

data$Date=ymd(data$Date)

class(data$Date)

data=xts(data[, 2:4],order.by= as.Date(data$Date))
###### Creating log returns ########
data$Price_lag1=lag(data$Price, 1)

class(data$Price)

data$Ret=log(data$Price/data$Price_lag1)

data=na.omit(data); data=data[, -4]

##### Input Values #####
Spot=data$Price[-nrow(data$Price)]
Strike=29
RF_rate=data$RF[-nrow(data$RF)]
Sigma=data$Sigma[-nrow(data$Sigma)]
Time=as.numeric(rev((-difftime(data[1], 
                            data[-1], units="days"))/360)); Time
Div=0


########### Options Graph #########
d1=log(Spot/Strike)+((RF_rate+0.5*Sigma^2)*Time)/(Sigma*sqrt(Time))

d2=d1-(Sigma*sqrt(Time))

Call_Price_BSM=Spot*pnorm(d1,mean=0,sd=1)-
              Strike*exp(-RF_rate*Time)*pnorm(d2,mean=0,sd=1)

Put_Price_BSM=Call_Price_BSM-Spot+Strike*exp(-RF_rate*Time)

Options_BSM_Prices=cbind(Call_Price_BSM, 
                         Put_Price_BSM, data$Price)

dygraph(Options_BSM_Prices, main = "Option and Stock
        Prices Dynamics") %>% 
  dyRangeSelector() %>% 
  dySeries("Price", label = "Call Price") %>%
  dySeries("Price.1", label = "Put Price") %>%
  dySeries("Price.2", label = "Stock Price") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Greeks Values") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 4)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey")


####### Greeks ###########
Call_BSM_delta = GBSGreeks(Selection="delta",TypeFlag="c",S=Spot,
                           X=Strike,
                           Time=Time,r=RF_rate,b=RF_rate-Div,
                           sigma=Sigma)

Call_BSM_hamma = GBSGreeks(Selection="gamma",TypeFlag="c",S=Spot,
                           X=Strike,
                           Time=Time,r=RF_rate,b=RF_rate-Div,
                           sigma=Sigma)

Call_BSM_vega = GBSGreeks(Selection="vega",TypeFlag="c",S=Spot,
                           X=Strike,
                           Time=Time,r=RF_rate,b=RF_rate-Div,
                           sigma=Sigma)

Call_BSM_theta = GBSGreeks(Selection="theta",TypeFlag="c",S=Spot,
                          X=Strike,
                          Time=Time,r=RF_rate,b=RF_rate-Div,
                          sigma=Sigma)

Call_BSM_greeks = cbind(Call_BSM_delta,Call_BSM_hamma,Call_BSM_vega,
                        Call_BSM_theta)

colnames(Call_BSM_greeks)=c("Delta", "Hamma", "Vega", "Theta")

#Call_BSM_greeks=Call_BSM_greeks[-nrow(Call_BSM_greeks)]

########### Greeks Graph #########
dygraph(Call_BSM_greeks, main = "Greeks Dynamics") %>% 
  dyRangeSelector() %>% 
  dySeries("Delta", label = "Delta") %>%
  dySeries("Hamma", label = "Hamma") %>%
  dySeries("Vega", label = "Vega") %>%
  dySeries("Theta", label = "Theta") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Greeks Values") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 4)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey")

########## Delta Hedging ############
Purchase=diff(Call_BSM_delta)

Purchase[is.na(Purchase)]=Call_BSM_delta[is.na(Purchase)]

Cost=Purchase*Spot

Cumulative_Cost=cumsum(Cost)

Cum_Cost_Interest=Cumulative_Cost*exp(RF_rate*rev(Time))

Cost_of_Hedging=Cum_Cost_Interest-Strike; colnames(Cost_of_Hedging)=c("Cost of Hedging")

######### Graph #######
dygraph(Cost_of_Hedging, main = "Cost of Hedging") %>% 
  dyRangeSelector() %>% 
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Cost of Hedging") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 4)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey")

########### Combined ####################
library(htmltools)
Combined=list(dygraph(Options_BSM_Prices, main = "Option and Stock
        Prices Dynamics", group="CE3") %>% 
                dyRangeSelector() %>% 
                dySeries("Price", label = "Call Price") %>%
                dySeries("Price.1", label = "Put Price") %>%
                dySeries("Price.2", label = "Stock Price") %>%
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                dyAxis("y", label = "Greeks Values") %>%
                dyHighlight(highlightCircleSize = 5,
                            highlightSeriesOpts = list(strokeWidth = 4)) %>%
                dyOptions(axisLineColor = "navy", gridLineColor = "grey"),
              dygraph(Call_BSM_greeks, main = "Greeks Dynamics"
                      , group="CE3") %>% 
                dyRangeSelector() %>% 
                dySeries("Delta", label = "Delta") %>%
                dySeries("Hamma", label = "Hamma") %>%
                dySeries("Vega", label = "Vega") %>%
                dySeries("Theta", label = "Theta") %>%
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                dyAxis("y", label = "Greeks Values") %>%
                dyHighlight(highlightCircleSize = 5,
                            highlightSeriesOpts = list(strokeWidth = 4)) %>%
                dyOptions(axisLineColor = "navy", gridLineColor = "grey"),
              dygraph(Cost_of_Hedging, main = "Cost of Hedging"
                      , group="CE3") %>% 
                dyRangeSelector() %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                dyAxis("y", label = "Cost of Hedging") %>%
                dyHighlight(highlightCircleSize = 5,
                            highlightSeriesOpts = list(strokeWidth = 4)) %>%
                dyOptions(axisLineColor = "navy", gridLineColor = "grey"))

browsable(tagList(Combined))
