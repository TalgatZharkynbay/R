########################
######################## The Bootstrap approach to one-sample hypothesis testing
######################## (the illustration focuses on a two-sided test)
########################


########################
######################## Module 5, Example Boot5, Bootstrapping hypothesis testing of the null hypothesis that the population mean = 33.02 
########################
### Type in the original sample
X = c(28, -44, 29, 30, 26, 27, 22, 23, 33, 16, 24, 29, 24, 40, 21, 31, 34, -2, 25, 19)

orig_sample = X

n = length(orig_sample)

### Find the observed value of the test statistic (i.e., the statistic of interest)
obs_val = mean(orig_sample)
obs_val

### Create the transormed sample in which the null hypothesis is satisfied
c = 33.02
trans_sample = orig_sample - obs_val + c

mean(trans_sample)

### Generate 100,000 Bootstrap samples (resamples) of the original sample size from the transformed sample
boot_samples = replicate(n=100000, sample(trans_sample, n, replace=TRUE), simplify=FALSE)

### Compute 100,000 Bootstrap estimates of the test statistic (one for each resample) to generate the 
### the Bootstrap sampling distribution of the statistic of interest 
boot_estimates = sapply(boot_samples, FUN=mean, simplify=TRUE)

### Find the p-value. 
### The p-value is the probability of getting something as big or more extreme than what we observed.
### 21.75 (the observed mean) is 11.27 = 33.02 - 21.75 units away from the null hypothesis.
### So the p-value is the probability of being at least 11.27 units away from 33.02.
### Using the Bootstrap sampling distribution we find
Boot_p_val = (sum(boot_estimates <= c - 11.27) + sum(boot_estimates >= c + 11.27))/100000
Boot_p_val


# We estimate the p-value to be 0.006 (i.e., in 600 times out of 100,000)
# The probability of getting a sample average such extreme as this, assuming that the population mean is 33.02 < 5%.
# Thus, we reject the null hypothesis.