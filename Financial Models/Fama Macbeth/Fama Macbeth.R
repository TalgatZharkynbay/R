options(digits = 3)
library(xts)
library(zoo)

Data=read.csv(url("https://www.dropbox.com/s/jrifpgp2pvvyxhv/Fama_Macbeth_Data.csv?raw=1"))

head(Data)

class(Data$Date)

Data$Date=seq(as.Date("1970-01-01"),
              + as.Date("2019-09-01"),by="months")

Data=xts(Data[2:54], order.by = Data$Date)

#One_Beta=na.omit(rollapply(Data, width = 60, 
                    # FUN=function(x)
                    #  {
                    #   roll.reg=lm(Agric~MktxRF+SMB+HML,
                    #               data=as.data.frame(x))
                    #   return(roll.reg$coef[-1])
                    #  },
                    #   by.column = FALSE, align = "right"))

##############################################
getCoef <- function(Data, lhs, rhs) {
 coef(lm(paste(lhs, "~", rhs), Data))[2:4]
}

roll <- function(Data, lhs, rhs = "MktxRF + SMB+HML") {
  rollapplyr(Data, 60, FUN=getCoef, by.column = FALSE,
             lhs = lhs, rhs = rhs)
}

dim(Data)

Portfolios=colnames(Data)[5:53]

All_Betas=na.omit(as.data.frame(lapply(Portfolios, roll, Data = Data)))

Returns=as.data.frame(Data["1975-01-01/2019-09-01", -c(1:4)])

All_Betas_Mkt=All_Betas[row.names(All_Betas), c(seq(1, 145, by=3))]

All_Betas_SMB=All_Betas[row.names(All_Betas), c(seq(2, 146, by=3))]

All_Betas_HML=All_Betas[row.names(All_Betas), c(seq(3, 147, by=3))]
###################################################################

Factor_Premiums=data.frame(t(sapply(seq(nrow(Returns)), 
                          function(x) coeffs <- lm(unlist(Returns[x,]) ~ 
                                                     unlist(All_Betas_Mkt[x,])
                                                   +
                                                     unlist(All_Betas_SMB[x,])
                                                   + 
                                                   unlist(All_Betas_HML[x,]))$coefficients)))

colnames(Factor_Premiums)=c("Alpha", "Mkt-RF", "SMB", "HML")

Averaged_Values=cbind(mean(Factor_Premiums$Alpha), mean(Factor_Premiums$`Mkt-RF`),
      mean(Factor_Premiums$SMB), mean(Factor_Premiums$HML))

colnames(Averaged_Values)=c("Alpha", "Market Risk Premium", "Small Minus Big",
                            "High Minus Low")
Averaged_Values

SD=cbind(sd(Factor_Premiums$Alpha), sd(Factor_Premiums$`Mkt-RF`),
         sd(Factor_Premiums$SMB), sd(Factor_Premiums$HML))/sqrt(537)

colnames(SD)=c("Alpha", "Market Risk Premium", "Small Minus Big",
                            "High Minus Low")         
SD

T_stats=Averaged_Values/SD; T_stats
