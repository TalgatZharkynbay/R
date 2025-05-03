########################
######################## The Bootstrap approach to interval estimation
########################

########################
######################## Module 5, Example Boot3, the Bootstrap approach to 95% interval estimate for Pearson correlation coefficient (alpha = 5%)
######################## (The assumption is that the samples are drawn from jointly distributed RVs, i.e., dependent populations)
########################
### Generate two paired random samples (in reality, we will be given two characteristics that belong to the same population units)
X_Y = cbind(rnorm(n=25, mean=3, sd=1), rnorm(n=25, mean=2, sd=2))

orig_sample = X_Y

n = dim(X_Y)[1]

orig_index = 1:n


### Generate 100,000 Bootstrap indexes and corresponding samples (resamples) of sizes n from the original paired sample
### Compute 100,000 Bootstrap estimates of the estimator (one for each resample) to generate the Bootstrap sampling distribution of the estimator 
boot_estimates = rep(NA, times=100000)

for(i in 1:100000){
	boot_index = sample(orig_index, n, replace=TRUE)
	boot_sample = orig_sample[boot_index, ]
	boot_estimates[i] = cor(boot_sample[ ,1], boot_sample[ ,2], use="complete.obs", method="pearson")
}

### Bootstrap 95% confidence intervals
### Using the standard error method
se_boot = sd(boot_estimates) 
orig_sample_estimate = cor(orig_sample[ ,1], orig_sample[ ,2], use="complete.obs", method="pearson")

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
	            orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

### Using the percentile method
qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method 
