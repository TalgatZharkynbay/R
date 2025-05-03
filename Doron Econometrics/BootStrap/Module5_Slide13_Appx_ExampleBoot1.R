########################
######################## The Bootstrap approach to interval estimation
########################

########################
######################## Module 5, Example Boot1, the Bootstrap approach to 95% interval estimate for a population mean (alpha = 5%)
########################
### Generate a random sample (in reality, we will be given a random sample)
X = rnorm(n=25, mean=3, sd=1)
orig_sample = X

n = length(orig_sample)

### Generate 100,000 Bootstrap samples (resamples) of size n from the original sample
boot_samples = replicate(n=100000, sample(orig_sample, n, replace=TRUE), simplify=FALSE)

### Compute 100,000 Bootstrap estimates of the estimator (one for each resample) to generate the 
### the Bootstrap sampling distribution of the estimator 
boot_estimates = sapply(boot_samples, FUN=mean, simplify=TRUE)

### Bootstrap 95% confidence intervals
### Using the standard error method
se_boot = sd(boot_estimates) 
orig_sample_estimate = mean(orig_sample)

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
	            orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

### Because the standard normal dist is symmetric we can also use
# c(orig_sample_estimate - qnorm(p=0.975, mean=0, sd=1)*se_boot, 
#   orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)

### Using the percentile method
qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method 