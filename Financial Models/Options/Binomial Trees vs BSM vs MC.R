library(fOptions)
library(derivmkts)
library(ggplot2)
options(digits = 4)
##### Input Values #####
Spot=63
Strike=65
RF_rate=0.042
Sigma=0.32
Time=0.25
Div=0
#############################
#1) Analytical/BSM Solution
Call_BSM=GBSOption(TypeFlag="c",S=Spot,X=Strike,
                   Time=Time,r=RF_rate,b=RF_rate-Div,sigma=Sigma)

Call_BSM@price

Put_BSM=GBSOption(TypeFlag="p",S=Spot,X=Strike,
                  Time=Time,r=RF_rate,b=RF_rate-Div,sigma=Sigma)
Put_BSM@price

##################### Raw BSM check ####################:
d1=(log(Spot/Strike)+((RF_rate+0.5*Sigma^2)*Time))/(Sigma*sqrt(Time))
d2=d1-(Sigma*sqrt(Time))

Call_Raw_BSM=Spot*pnorm(d1,mean=0,sd=1)-
  Strike*exp(-RF_rate*Time)*pnorm(d2,mean=0,sd=1)

Put_Raw_BSM=Call_Raw_BSM-Spot+Strike*exp(-RF_rate*Time)

Call_Raw_BSM

Put_Raw_BSM

#2) Binomial Trees Solution
Call_CRR=CRRBinomialTreeOption(TypeFlag="ce",S=Spot,
                               X=Strike,Time=Time,r=RF_rate,
                               b=RF_rate-Div,
                               sigma=Sigma,n=100)

Put_CRR=CRRBinomialTreeOption(TypeFlag="pe",S=Spot,
                              X=Strike,Time=Time,r=RF_rate,
                              b=RF_rate-Div,
                              sigma=Sigma,n=100)

Call_CRR@price; Put_CRR@price

###### Plotting Trees ######
#1) Version 1
Call_CRR_Tree = BinomialTreeOption(TypeFlag="ce",S=Spot,
                                   X=Strike,Time=Time,r=RF_rate,
                                   b=RF_rate-Div,
                                   sigma=Sigma,n=5)

BinomialTreePlot(Call_CRR_Tree, dy = 1, cex = 0.8, ylim = c(-6, 7),
                 xlab = "n", ylab = "Option Value")
title(main = "Call Option Tree")

Put_CRR_Tree = BinomialTreeOption(TypeFlag="pe",S=Spot,
                                   X=Strike,Time=Time,r=RF_rate,
                                   b=RF_rate-Div,
                                   sigma=Sigma,n=5)

BinomialTreePlot(Put_CRR_Tree, dy = 1, cex = 0.8, ylim = c(-6, 7),
                 xlab = "n", ylab = "Option Value")
title(main = "Put Option Tree")

#2) Version 2 that is capable of plotting American Options
s=Spot; k=Strike; v=Sigma; r=RF_rate; tt=Time; d=Div; nstep=6

V1=binomplot(s, k, v, r, tt, d, nstep, american=FALSE, putopt=FALSE,
             plotarrows = TRUE, plotvalues = TRUE, 
             drawstrike = FALSE, pointsize = 4,
             crr = TRUE)

##########################################################


Test=binomopt(s, k, v, r, tt, d, nstep, american=FALSE, putopt=FALSE,
returntrees = TRUE, crr = TRUE, returngreeks = TRUE)

Tree=as.data.frame(Test$stree)

Multipliers=c(exp(r*seq(1, ncol(Tree), 1)))

Multipliers

MegaTest=as.data.frame(t(t(Tree)*Multipliers))


#################################################################
V2=binomplot(s, k, v, r, tt, d, nstep, american=FALSE, putopt=TRUE,
             plotarrows = TRUE, plotvalues = TRUE, 
             drawstrike = FALSE, pointsize = 4,
             crr = TRUE)

#3) Monte Carlo Method
num.sim=100000

R=(RF_rate-0.5*Sigma^2)*Time

SD=Sigma*sqrt(Time)

Spot_at_t= Spot*exp(R+SD*rnorm(num.sim,0,1))

#### Now, we can calculate the prices for Call and Put:
#Call:
Call_MC=pmax(0,Spot_at_t-Strike)

PV_Call_MC=Call_MC*(exp(-RF_rate*Time))

mean(PV_Call_MC)
#Put:
Put_MC=pmax(0,Strike-Spot_at_t)

PV_Put_MC=Put_MC*(exp(-RF_rate*Time))

mean(PV_Put_MC)

############## Compare ALL 3 METHODS ##############
BSM_vs_CRR_vs_MC=cbind(Call_BSM@price,Put_BSM@price, 
                       Call_CRR@price, Put_CRR@price,
                       mean(PV_Call_MC), mean(PV_Put_MC))

colnames(BSM_vs_CRR_vs_MC) <- c("Call BSM", "Put BSM", "Call CRR",
                                "Put CRR", "Call MC", "Put MC")
BSM_vs_CRR_vs_MC
