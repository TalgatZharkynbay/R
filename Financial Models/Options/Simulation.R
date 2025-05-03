library(lubridate)
library(quantmod)
library(dygraphs)
library(fOptions)
library(data.table)
##### Input Values #####
Spot=237 
paths=20
years=2
time_step = 0.016666667
rf_rate = 0.02
sigma=0.400394454
Strike=237
Div=0
#########################################
StockPaths<-function(Spot, paths, years, time_step, rf_rate, sigma)
{
  rows<-years/time_step
  sample<-matrix(0,nrow=(rows+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<-Spot
    for(j in 2:(rows+1))
    {
      sample[j,i]<-sample[j-1,i]*exp(time_step*(rf_rate-((sigma)^2)/2)+
                                       ((time_step)^.5)*rnorm(1)*sigma)
    }
  }
  return(sample)
}

Stonks=as.data.frame(StockPaths(Spot=Spot, paths=paths,  years=years, 
                                time_step = time_step,rf_rate = rf_rate, 
                                sigma=sigma))

#We assume trading at Weekends for simplicity to visualize the movement:

Imaginary_Dates=seq.Date(from = as.Date("2020-04-19"), 
                                   to = as.Date("2020-08-16"), 
                                   by = 1)

Stonks=xts(Stonks, order.by = Imaginary_Dates)

dygraph(Stonks, main = "Stock Path Tracing") %>% 
  dyRangeSelector() %>% 
  dyLegend(width=1000,show = "always", hideOnMouseOut = TRUE) %>%
  dyAxis("y", label = "Stock Price") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 4)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey")

########## Going for Deltas ################
############# For Call #############
Time=rev(seq(0, 2, by=0.016666667))

Call_BSM_delta = GBSGreeks(Selection="delta",TypeFlag="c",S=Stonks,
                           X=Strike,
                           Time=Time,r=rf_rate,b=rf_rate-Div,
                           sigma=sigma)

Purchase=diff(Call_BSM_delta)

Purchase[is.na(Purchase)]=Call_BSM_delta[1]

Cost=Purchase*Stonks

Cumulative_Cost=cumsum(Cost)

Cum_Cost_Interest=as.data.table(Cumulative_Cost*exp(rf_rate*rev(Time)))

Cum_Cost_Interest=Cum_Cost_Interest[,-1]

########## For Put ###############
Put_BSM_delta = GBSGreeks(Selection="delta",TypeFlag="p",S=Stonks,
                          X=Strike,
                          Time=Time,r=rf_rate,b=rf_rate-Div,
                          sigma=sigma)

Purchase_put=diff(Put_BSM_delta)

Purchase_put[is.na(Purchase_put)]=Put_BSM_delta[1]

Cost_put=Purchase_put*Stonks

Cumulative_Cost_Put=cumsum(Cost_put)

######## Now go for Call and Put Prices ###########

Stonks=as.data.table(Stonks); Stonks=Stonks[, -1]


Cost_of_hedging=as.numeric(ifelse(Stonks[nrow(Stonks),]>Strike,
                       Cum_Cost_Interest[nrow(Cum_Cost_Interest),]-Strike,
                       Cum_Cost_Interest[nrow(Cum_Cost_Interest),]))



Call_Price_Simulation=round(mean(Cost_of_hedging), digits=5); Call_Price_Simulation

##############################################
Cum_Cost_Interest_Put=as.data.table(Cumulative_Cost_Put*exp(rf_rate*rev(Time)))

Cum_Cost_Interest_Put=Cum_Cost_Interest_Put[,-1]


Cost_of_hedging_Put=as.numeric(ifelse(Stonks[nrow(Stonks),]<Strike,
                                      Cum_Cost_Interest_Put[nrow(Cum_Cost_Interest_Put),]+Strike,
                                      Cum_Cost_Interest_Put[nrow(Cum_Cost_Interest_Put),]))

                       

Put_Price_Simulation=round(mean(Cost_of_hedging_Put), digits=5); Put_Price_Simulation

table(Call_Price_Simulation, Put_Price_Simulation)

