library(quantmod)
library(tidyverse)
library(tibbletime)
library(scales)
library(Hmisc)
library(stringr)
library(highcharter)

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
# Yelda

test <-treasury %>%
  data.frame(Date = index(.))   %>%
  remove_rownames() %>%
  gather(TreasuryBond, Yields, -Date) %>%
  group_by(TreasuryBond)

treasury=treasury[as.Date(2016-11-15)]



PlotData=test %>%
  filter(Date==str_replace_all(string = Cs(2016-11-15), 
                               pattern=" ", repl=""))

ggplot(PlotData,
         aes(x = factor(TreasuryBond, levels =c("DGS3MO","DGS6MO","DGS1", "DGS2", "DGS3", "DGS5",  
                                                "DGS7",  
                                                "DGS10",  
                                                "DGS20",  
                                                "DGS30")), 
             y=Yields, group = 1)) +geom_line()+geom_point()+geom_text(aes(label = round(Yields, 3)),
                                                                         vjust = "outward", hjust = "inward",
                                                                         show.legend = FALSE)+
  xlab("Treasuries from 3 Month to 30 Years") +ylab("Yields in %")


# Term Spread wws
Term_Spread=na.omit(DGS30-DGS3MO)

highchart(type = "stock") %>%
  hc_title(text = "Term Spread between maturities") %>%
  hc_add_series(Thirty_three,
                name = "Term Spread",
                color = "cornflowerblue") %>%
  hc_yAxis(title = list(text = "Term Spread (%)"),
           opposite = FALSE, 
           plotLines = list(list(value = 0, color = "red", width = 2,
                                 dashStyle = "shortdash"))) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)
