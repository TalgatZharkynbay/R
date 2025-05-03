library(haven)
library(tidyverse)


crsp <- read_sas("crsp.SAS7BDAT", NULL)
view(crsp)

compustat <- read_sas("compustat.SAS7BDAT", NULL)
View(compustat)

ff <-read.csv("F-F_Research_Data_Factors.csv")
