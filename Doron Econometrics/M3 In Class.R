library(aod)
library(lmtest)
MROZ=read.table(file="MROZ.txt", sep="", header=FALSE, as.is = TRUE, na.strings = c("NA",".",""))
MROZ_New=MROZ[, c(1,3,4,5,6,19,20,22)]
colnames(MROZ_New)=c("INLF","KIDSLT6", "KIDSGE6", "AGE", "EDUC", "EXPER", 
                     "NWIFEINC", "EXPERSQ")
Fit_LPM=lm(INLF~NWIFEINC+EDUC+EXPER+EXPERSQ+AGE+KIDSLT6+KIDSGE6, data=MROZ_New)

Fit_Logit=glm(INLF~NWIFEINC+EDUC+EXPER+EXPERSQ+AGE+KIDSLT6+KIDSGE6, data=MROZ_New,
              family = binomial (link="logit"))

Fit_Probit=glm(INLF~NWIFEINC+EDUC+EXPER+EXPERSQ+AGE+KIDSLT6+KIDSGE6, data=MROZ_New,
              family = binomial (link="probit"))

summary(Fit_LPM)

summary(Fit_Logit)

summary(Fit_Probit)
