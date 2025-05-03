##########################################################################
# Problem Set 5
require(Hmisc)
require(stargazer)
require(car)
require(nlme)
require(lfe)
require(lmtest)
require(sandwich)
require(aod)
require(moments)
require(tseries)
##########################################################################
# q1
# i
setwd("C:/Users/ְיהמס/OneDrive - nu.edu.kz/MSF/Core 9/Econometrics/ps5")
Signals = read.csv('Signals_A_62016.csv', header=TRUE,as.is=TRUE,na.strings = c("NA",".",""))
Signals$PFormDate = as.Date(Signals$PFormDate, format = '%d/%m/%Y')

# ii
Signals$qNXF = ave(Signals$NXF,
                   Signals$PFormDate,
                   FUN= function(x) {cut(x, breaks=quantile(x, probs=seq(from=0, to=1,
                   by=0.2), na.rm=TRUE), include.lowest=TRUE, na.rm=TRUE)})

# iii
SignalsDF_2002 = Signals[Signals$PFormDate == '2002-06-30',]

# iv
mean(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1], na.rm = TRUE)
median(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1], na.rm = TRUE)
sd(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1], na.rm = TRUE)
skewness(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1], na.rm = TRUE)
kurtosis(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1], na.rm = TRUE)
cor(SignalsDF_2002[c('AvgMonRet','Size')][SignalsDF_2002$qNXF == 1,][,1],SignalsDF_2002[c('AvgMonRet','Size')][SignalsDF_2002$qNXF == 1,][,2], use = 'complete.obs', method = 'pearson')

mean(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5], na.rm = TRUE)
median(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5], na.rm = TRUE)
sd(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5], na.rm = TRUE)
skewness(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5], na.rm = TRUE)
kurtosis(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5], na.rm = TRUE)
cor(SignalsDF_2002[c('AvgMonRet','Size')][SignalsDF_2002$qNXF == 5,][,1],SignalsDF_2002[c('AvgMonRet','Size')][SignalsDF_2002$qNXF == 5,][,2], use = 'complete.obs', method = 'pearson')

# v
jarque.bera.test(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1])])
jarque.bera.test(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5])])

# vi
# median for qNXF = 1
X = SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1])]
orig_sample = X
n = length(orig_sample)

boot_samples = replicate(n=100000, sample(orig_sample, n, replace=TRUE), simplify=FALSE)
boot_estimates = sapply(boot_samples, FUN=median, simplify=TRUE)
se_boot = sd(boot_estimates) 
orig_sample_estimate = median(orig_sample)

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
                    orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method

# median for qNXF = 5
X = SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5])]
orig_sample = X
n = length(orig_sample)

boot_samples = replicate(n=100000, sample(orig_sample, n, replace=TRUE), simplify=FALSE)
boot_estimates = sapply(boot_samples, FUN=median, simplify=TRUE)
se_boot = sd(boot_estimates) 
orig_sample_estimate = median(orig_sample)

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
                    orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method

# difference between medians
X1 = SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1])]
X2 = SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5])]
orig_sample1 = X1
orig_sample2 = X2
n1 = length(orig_sample1)
n2 = length(orig_sample2)

boot_samples1 = replicate(n=100000, sample(orig_sample1, n1, replace=TRUE), simplify=FALSE)
boot_samples2 = replicate(n=100000, sample(orig_sample2, n2, replace=TRUE), simplify=FALSE)
boot_estimates = sapply(boot_samples1, FUN=median, simplify=TRUE)-sapply(boot_samples2, FUN=median, simplify=TRUE)
se_boot = sd(boot_estimates) 
orig_sample_estimate = median(orig_sample1)-median(orig_sample2)

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
                    orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method

# Pearson correlation for qNXF = 1
orig_sample = SignalsDF_2002[c('AvgMonRet','Size')][SignalsDF_2002$qNXF == 1,]
n = dim(orig_sample)[1]
orig_index = 1:n

boot_estimates = rep(NA, times=100000)
for(i in 1:100000){
  boot_index = sample(orig_index, n, replace=TRUE)
  boot_sample = orig_sample[boot_index, ]
  boot_estimates[i] = cor(boot_sample[ ,1], boot_sample[ ,2], use="complete.obs", method="pearson")
}

se_boot = sd(boot_estimates) 
orig_sample_estimate = cor(orig_sample[ ,1], orig_sample[ ,2], use="complete.obs", method="pearson")

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
                    orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method

# Pearson correlation for qNXF = 5
orig_sample = SignalsDF_2002[c('AvgMonRet','Size')][SignalsDF_2002$qNXF == 5,]
n = dim(orig_sample)[1]
orig_index = 1:n

boot_estimates = rep(NA, times=100000)
for(i in 1:100000){
  boot_index = sample(orig_index, n, replace=TRUE)
  boot_sample = orig_sample[boot_index, ]
  boot_estimates[i] = cor(boot_sample[ ,1], boot_sample[ ,2], use="complete.obs", method="pearson")
}

se_boot = sd(boot_estimates) 
orig_sample_estimate = cor(orig_sample[ ,1], orig_sample[ ,2], use="complete.obs", method="pearson")

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
                    orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method

# vii
orig_sample = SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1])]
n = length(orig_sample)
obs_val = median(orig_sample)
obs_val
c = median(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5], na.rm = TRUE)
trans_sample = orig_sample - obs_val + c
boot_samples = replicate(n=100000, sample(trans_sample, n, replace=TRUE), simplify=FALSE)
boot_estimates = sapply(boot_samples, FUN=median, simplify=TRUE)

Boot_p_val = (sum(boot_estimates <= c - obs_val) + sum(boot_estimates >= c + obs_val))/100000
Boot_p_val

# viii
orig_sample1 = SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 1])]
n1 = length(orig_sample1)
orig_sample2 = SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5][!is.na(SignalsDF_2002$AvgMonRet[SignalsDF_2002$qNXF == 5])]
n2 = length(orig_sample2)

obs_val1 = skewness(orig_sample1)
obs_val2 = skewness(orig_sample2)
obs_val_stat = obs_val1 - obs_val2

orig_sample_comb = c(orig_sample1, orig_sample2)
boot_estimates = rep(NA, times=100000)
for(i in 1:100000){
  boot_sample_comb = sample(orig_sample_comb, n1 + n2, replace=TRUE)
  boot_sample1 = boot_sample_comb[1:n1]
  boot_sample2 = boot_sample_comb[(n1+1):(n1+n2)]
  boot_estimates[i] = skewness(boot_sample1) - skewness(boot_sample2)
}
c = 0
Boot_p_val = (sum(boot_estimates <= c + obs_val_stat) + sum(boot_estimates >= c - obs_val_stat))/100000
Boot_p_val


##########################################################################
# q2
# i
SignalsDF_1992 = Signals[Signals$PFormDate == '1992-06-30',]

# ii
fit1 = lm(AvgMonRet ~ log(Size) + log(BTM) + ROE, data = SignalsDF_1992)
vcovHC0 = vcovHC(fit1, type = 'HC0')
coeftest(fit1, vcov. = vcovHC0)
coefci(fit1,"log(Size)",level=0.95,vcov.=vcovHC0)
coefci(fit1,"log(BTM)",level=0.95,vcov.=vcovHC0)
coefci(fit1,"ROE",level=0.95,vcov.=vcovHC0)

fit2 = felm(AvgMonRet ~ log(Size) + log(BTM) + ROE | 0 | 0 | BBHL, data=SignalsDF_1992)
vcovCluster = fit2$clustervcv
coeftest(fit2,vcov.=vcovCluster)
coefci(fit2,"log(Size)",level=0.95,vcov.=vcovCluster)
coefci(fit2,"log(BTM)",level=0.95,vcov.=vcovCluster)
coefci(fit2,"ROE",level=0.95,vcov.=vcovCluster)

# iii
jarque.bera.test(fit1$residuals)

# iv
skewness(fit1$residuals)
kurtosis(fit1$residuals)

# vi
orig_sample = SignalsDF_1992
n = dim(SignalsDF_1992)[1]
orig_index = 1:n
boot_estimatesBeta1 = rep(NA, times=10000)
boot_estimatesBeta2 = rep(NA, times=10000)
boot_estimatesBeta3 = rep(NA, times=10000)
for(i in 1:25000){
  boot_index = sample(orig_index, n, replace=TRUE)
  boot_sample = orig_sample[boot_index, ]
  fitx = lm(AvgMonRet ~ log(Size) + log(BTM) + ROE, data = boot_sample)
  boot_estimatesBeta1[i] = fitx$coefficients['log(Size)']
  boot_estimatesBeta2[i] = fitx$coefficients['log(BTM)']
  boot_estimatesBeta3[i] = fitx$coefficients['ROE']
}
### Using the standard error method
se_boot1 = sd(boot_estimatesBeta1) 
se_boot2 = sd(boot_estimatesBeta2) 
se_boot3 = sd(boot_estimatesBeta3) 

orig_sample_estimate1 = lm(AvgMonRet ~ log(Size) + log(BTM) + ROE, data = orig_sample)$coefficients['log(Size)']
orig_sample_estimate2 = lm(AvgMonRet ~ log(Size) + log(BTM) + ROE, data = orig_sample)$coefficients['log(BTM)']
orig_sample_estimate3 = lm(AvgMonRet ~ log(Size) + log(BTM) + ROE, data = orig_sample)$coefficients['ROE']

CI_95_se_method1 = c(orig_sample_estimate1 + qnorm(p=0.025, mean=0, sd=1)*se_boot1, 
                    orig_sample_estimate1 - qnorm(p=0.025, mean=0, sd=1)*se_boot1)
# Beta1
CI_95_se_method1

CI_95_se_method2 = c(orig_sample_estimate2 + qnorm(p=0.025, mean=0, sd=1)*se_boot2, 
                     orig_sample_estimate2 - qnorm(p=0.025, mean=0, sd=1)*se_boot2)
# Beta2
CI_95_se_method2

CI_95_se_method3 = c(orig_sample_estimate3 + qnorm(p=0.025, mean=0, sd=1)*se_boot3, 
                     orig_sample_estimate3 - qnorm(p=0.025, mean=0, sd=1)*se_boot3)
# Beta3
CI_95_se_method3

# Using the percentile method
qs_boot1 = quantile(boot_estimatesBeta1, probs=c(0.025, 0.975))
CI_95_p_method1 = unname(qs_boot1)
# Beta1
CI_95_p_method1

qs_boot2 = quantile(boot_estimatesBeta2, probs=c(0.025, 0.975))
CI_95_p_method2 = unname(qs_boot2)
# Beta2
CI_95_p_method2

qs_boot3 = quantile(boot_estimatesBeta3, probs=c(0.025, 0.975))
CI_95_p_method3 = unname(qs_boot3)
# Beta3
CI_95_p_method3