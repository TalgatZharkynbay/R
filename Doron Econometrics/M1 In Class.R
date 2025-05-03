CEOSAL1=read.table(file="CEOSAL1.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))
CEOSAL1=CEOSAL1[, c("V1", "V4", "V8", "V11")]
colnames(CEOSAL1)=c("SALARY", "ROE", "FIN_INDST", "LOG_SALARY")
head(CEOSAL1)

WAGE1=read.table(file="WAGE1.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))

require(Hmisc)
require(GGally)

describe(CEOSAL1[, c("SALARY", "ROE", "FIN_INDST")])
pairs(CEOSAL1)
boxplot(CEOSAL1$SALARY~CEOSAL1$FIN_INDST)
boxplot(CEOSAL1$ROE~CEOSAL1$FIN_INDST)

MAT=as.matrix(CEOSAL1)
rcorr(MAT, type="pearson")
rcorr(MAT, type="spearman")

fit=lm(SALARY~ROE, data = CEOSAL1)
names(fit)
fit$coefficients
fit$coefficients["(Intercept)"]
summary(fit)

fit1=lm(SALARY~ROE+FIN_INDST, data = CEOSAL1)
summary(fit1)

fit2=lm(SALARY~ROE+FIN_INDST+ROE:FIN_INDST, data = CEOSAL1)
summary(fit2)

require(stargazer)
stargazer(fit, fit1, fit2, type = "html", out = "RegOut.htm")

Wage1_New=WAGE1[, c("V1","V2","V3","V4","V22")]
colnames(Wage1_New)=c("WAGE", "EDUC", "EXPER", "TENURE", "LogWage")
describe(Wage1_New)
pairs(Wage1_New)
ggpairs(Wage1_New)

fitw=lm(LogWage~EDUC+TENURE+EXPER, data=Wage1_New)
summaryfitw=summary(fitw)
summaryfitw

fitw$fitted.values[1:5]
Wage1_New$LogWage[1:5]
#convert the log back to normal numbers
#1) MOM
MOM_est=mean(exp(fitw$residuals))
MOM_est
MoM_FitVal=MOM_est*exp(fitw$fitted.values)
MoM_FitVal[1:5]

