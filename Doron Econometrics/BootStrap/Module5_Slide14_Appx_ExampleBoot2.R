########################
######################## The Bootstrap approach to interval estimation
########################

########################
######################## Module 5, Example Boot2, the Bootstrap approach to 95% interval estimate for differences in population means (alpha = 5%)
######################## (The assumption is that the samples are drawn from independent populations)
########################
### Generate two random samples (in reality, we will be given two random samples)
X = rnorm(n=25, mean=3, sd=1)
Y = rnorm(n=20, mean=2, sd=2)

orig_sample1 = X
orig_sample2 = Y

n1 = length(orig_sample1)
n2 = length(orig_sample2)

### Generate 100,000 Bootstrap samples (resamples) of sizes n1 and n2 from the original samples
boot_samples1 = replicate(n=100000, sample(orig_sample1, n1, replace=TRUE), simplify=FALSE)
boot_samples2 = replicate(n=100000, sample(orig_sample2, n2, replace=TRUE), simplify=FALSE)

### Compute 100000 Bootstrap estimates of the estimator (one for each resample) to generate the 
### the Bootstrap sampling distribution of the estimator 
boot_estimates = sapply(boot_samples1, FUN=mean, simplify=TRUE) - sapply(boot_samples2, FUN=mean, simplify=TRUE)

### Bootstrap 95% confidence intervals
### Using the standard error method
se_boot = sd(boot_estimates) 
orig_sample_estimate = mean(orig_sample1) - mean(orig_sample2)

CI_95_se_method = c(orig_sample_estimate + qnorm(p=0.025, mean=0, sd=1)*se_boot, 
	            orig_sample_estimate - qnorm(p=0.025, mean=0, sd=1)*se_boot)
CI_95_se_method

### Using the percentile method
qs_boot = quantile(boot_estimates, probs=c(0.025, 0.975))
CI_95_p_method = unname(qs_boot)
CI_95_p_method 