########################
######################## The Bootstrap approach to interval estimation in MLR
######################## ("Bootstrapping the sample" procedure)
########################

########################
######################## Module 5, Example Boot4, Bootstrapping the sample approach to 95% interval estimate for R-squared 
######################## from the MLR model of Wage and Education (alpha = 5%)
########################
###
### Set the working direction
###
# My PC
setwd(" ")


### Read the data 
WAGE1 = read.table(file="WAGE1.txt", header=FALSE, sep="", as.is=TRUE, na.strings=c("NA",".",""))

###  Rename the relevant columns and create a reduced data frame for analyses
WAGE1_New = WAGE1[, c("V1", "V2", "V3", "V4", "V22")]
colnames(WAGE1_New) = c("WAGE", "EDUC", "EXPER", "TENURE", "LogWAGE") 

str(WAGE1_New)
dim(WAGE1_New)

### Treat the data frame as the original sample and create an index vector to resample (p+1)-tuples
orig_sample = WAGE1_New

n = dim(WAGE1_New)[1]

orig_index = 1:n

### Generate 10,000 Bootstrap indexes and corresponding samples (data sets) of sizes n from the original sample
### Run the Wage and Education regression 10,000 times and obtain 10,000 Bootstrap estimates of the R-squared (one for each sample) 
### to find the Bootstrap sampling distribution of the R-squared. 
boot_estimates = rep(NA, times=10000)

for(i in 1:10000){
	boot_index = sample(orig_index, n, replace=TRUE)
	boot_sample = orig_sample[boot_index, ]
	boot_estimates[i] = summary(lm(LogWAGE ~ EDUC + EXPER + TENURE, data=boot_sample))$r.squared
}

### Bootstrap 95% confidence intervals for R-squared
### Using the standard error method
se_boot = sd(boot_estimates) 
orig_sample_estimate = summary(lm(LogWAGE ~ EDUC + EXPER + TENURE, data=orig_sample))$r.squared

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
	            orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

# Using the percentile method
qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method 

