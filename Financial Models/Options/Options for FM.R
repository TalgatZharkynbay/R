library(fOptions)
##### Input Values #####
Spot=100
Strike=105
RF_rate=0.05
Sigma=0.25
Time=0.083333
Div=0.02
#############################
#1) Analytical/BSM Solution
Call_BSM=GBSOption(TypeFlag="c",S=Spot,X=Strike,
                   Time=Time,r=RF_rate,b=RF_rate-Div,sigma=Sigma)

Call_BSM

Put_BSM=GBSOption(TypeFlag="p",S=Spot,X=Strike,
                  Time=Time,r=RF_rate,b=RF_rate-Div,sigma=Sigma)
Put_BSM

###### Greeks ######
Call_BSM_delta = GBSGreeks(Selection="delta",TypeFlag="c",S=Spot,
                           X=Strike,
                           Time=Time,r=RF_rate,b=RF_rate-Div,
                           sigma=Sigma)

Call_BSM_gamma = GBSGreeks(Selection="gamma",TypeFlag="c",S=Spot,
                           X=Strike,Time=Time,r=RF_rate,b=RF_rate-Div,
                           sigma=Sigma)

Call_BSM_vega = GBSGreeks(Selection="vega",TypeFlag="c",S=Spot,
                          X=Strike,Time=Time,r=RF_rate,b=RF_rate-Div,
                          sigma=Sigma)

Call_BSM_theta = GBSGreeks(Selection="theta",TypeFlag="c",S=Spot,
                           X=Strike,Time=Time,r=RF_rate,b=RF_rate-Div,
                           sigma=Sigma)

Call_BSM_greeks = cbind(Call_BSM_delta,Call_BSM_gamma,Call_BSM_vega,
                        Call_BSM_theta)
Call_BSM_greeks
#############################
Put_BSM_delta = GBSGreeks(Selection="delta",TypeFlag="p",S=Spot,
                          X=Strike,
                          Time=Time,r=RF_rate,b=RF_rate-Div,
                          sigma=Sigma)

Put_BSM_gamma = GBSGreeks(Selection="gamma",TypeFlag="p",S=Spot,
                          X=Strike,Time=Time,r=RF_rate,b=RF_rate-Div,
                          sigma=Sigma)

Put_BSM_vega = GBSGreeks(Selection="vega",TypeFlag="p",S=Spot,
                         X=Strike,Time=Time,r=RF_rate,b=RF_rate-Div,
                         sigma=Sigma)

Put_BSM_theta = GBSGreeks(Selection="theta",TypeFlag="p",S=Spot,
                          X=Strike,Time=Time,r=RF_rate,b=RF_rate-Div,
                          sigma=Sigma)

Put_BSM_greeks = cbind(Put_BSM_delta,Put_BSM_gamma,Put_BSM_vega,
                       Put_BSM_theta)
Put_BSM_greeks

#2) Binomial Trees Solution
Call_CRR=CRRBinomialTreeOption(TypeFlag="ce",S=Spot,
                               X=Strike,Time=Time,r=RF_rate,
                               b=RF_rate-Div,
                               sigma=Sigma,n=100)

Put_CRR=CRRBinomialTreeOption(TypeFlag="pe",S=Spot,
                              X=Strike,Time=Time,r=RF_rate,
                              b=RF_rate-Div,
                              sigma=Sigma,n=100)

###### Plotting Trees ######
#1) Version 1
Call_CRR_Tree = BinomialTreeOption(TypeFlag="ce",S=Spot,
                                   X=Strike,Time=Time,r=RF_rate,
                                   b=RF_rate-Div,
                                   sigma=Sigma,n=5)

BinomialTreePlot(Call_CRR_Tree, dy = 1, cex = 0.8, ylim = c(-6, 7),
                 xlab = "n", ylab = "Option Value")
title(main = "Option Tree")

#2) Version 2 with American 
library(derivmkts)
s=Spot; k=Strike; v=Sigma; r=RF_rate; tt=Time; d=Div; nstep=10

V1=binomplot(s, k, v, r, tt, d, nstep, american=FALSE, putopt=FALSE,
          plotarrows = TRUE, plotvalues = TRUE, 
          drawstrike = TRUE, pointsize = 4,
          crr = TRUE)

V2=binomopt(s, k, v, r, tt, d, nstep, american=FALSE, putopt=FALSE,
         returnparams=TRUE, crr = TRUE, returntrees = TRUE)
V2$deltatree
plot(V2$deltatree)

###### Comparing BSM and CRR #######
BSM_vs_CRR=cbind(Call_BSM@price,Put_BSM@price, Call_CRR@price,
                 Put_CRR@price)

colnames(BSM_vs_CRR) <- c("Call BSM", "Put BSM", "Call CRR",
                          "Put CRR")
BSM_vs_CRR


#3) Monte Carlo Bullshit
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

BSM_vs_CRR_vs_MC=cbind(Call_BSM@price,Put_BSM@price, 
                       Call_CRR@price, Put_CRR@price,
                       mean(PV_Call_MC), mean(PV_Put_MC))

colnames(BSM_vs_CRR_vs_MC) <- c("Call BSM", "Put BSM", "Call CRR",
                          "Put CRR", "Call MC", "Put MC")
BSM_vs_CRR_vs_MC

#######   Paths simulation BS  ######
# GeometricBrownian<-function()
# {
#   paths<-10
#   count<-100000
#   interval<-5/count
#   mean<-0.06
#   sigma<-0.3
#   sample<-matrix(0,nrow=(count+1),ncol=paths)
#   for(i in 1:paths)
#   {
#     sample[1,i]<-100
#     for(j in 2:(count+1))
#     {
#       sample[j,i]<-sample[j-1,i]*exp(interval*(mean-((sigma)^2)/2)+((interval)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
#     }
#   }	
#   cat("E[W(2)] = ",mean(sample[2001,]),"\n")
#   cat("E[W(5)] = ",mean(sample[5001,]),"\n")
#   matplot(sample,main="Geometric Brownian",xlab="Time",ylab="Path",type="l")
# }
# GeometricBrownian()


##################### Raw BSM check ####################:
# d1=log(Spot/Strike)+((RF_rate+0.5*Sigma^2)*Time)/(Sigma*sqrt(Time))
# d2=d1-(Sigma*sqrt(Time))
# 
# Call_Raw_BSM=Spot*pnorm(d1,mean=0,sd=1)-
#   Strike*exp(-RF_rate*Time)*pnorm(d2,mean=0,sd=1)
# 
# Put_Raw_BSM=Call_Raw_BSM-Spot+Strike*exp(-RF_rate*Time)
# 
# Call_Raw_BSM;Put_Raw_BSM
