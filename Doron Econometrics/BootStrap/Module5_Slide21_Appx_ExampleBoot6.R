########################
######################## The Bootstrap approach to two-sample hypothesis testing
######################## (the illustration focuses on a two-sided tests)
########################

########################
######################## Module 5, Example Boot6, Bootstrapping hypothesis testing of the null hypothesis for independent populations
########################
##### To illustrate the idea, let's bring in "Mouse data"
##### These data provides survival times (in days) of 16 mice after a test surgery
##### 7 mice in the Treatment group (new medical treatment)
##### 9 mice in the Control group (no treatment)
##### We want to know whether the treatment prolonged survival, i.e., compare the means for the two groups.
##### That is we wnat to test the null hypothesis that the means are the same across the two groups.

### Type in the two samples 
X = c(94, 197, 16, 38, 99, 141, 23)
orig_sample1 = X 
n1 = length(orig_sample1)
n1

Y = c(52, 104, 146, 10, 51, 30, 40, 27, 46)
orig_sample2 = Y
n2 = length(orig_sample2)
n2

### Find the observed values of the test statistic (i.e., the statistic of interest) and its components
obs_val1 = mean(orig_sample1)
obs_val1

obs_val2 = mean(orig_sample2)
obs_val2

obs_val_stat = mean(orig_sample1) - mean(orig_sample2) 
obs_val_stat


#####
##### Approach 1: Permutation tests
### Step 1: Merge the two original samples into one sample of n1 + n2 observations
orig_sample_comb = c(orig_sample1, orig_sample2)

### Step 2: Generate 100,000 Bootstrap samples (resamples) of the combined sample size from the combined sample size.
### For each sample, calculate the mean of the first n1 observations, calculate the mean of the remaining n2 observations, 
### and the corresponding value of the test statistic.		
boot_estimates = rep(NA, times=100000)

for(i in 1:1000){
	boot_sample_comb = sample(orig_sample_comb, n1 + n2, replace=TRUE)
	boot_sample1 = boot_sample_comb[1:n1]
	boot_sample2 = boot_sample_comb[(n1+1):(n1+n2)]
	boot_estimates[i] = mean(boot_sample1) - mean(boot_sample2)
}

### Step 3: Find the p-value. 
### The p-value is the probability of getting something as big or more extreme than what we observed.
### 30.63 (the observed difference) is 30.63 = 30.63 - 0 units away from the null hypothesis (in our case of no difference).
### So the p-value is the probability of being at least 30.63 units away from 0.
### Using the Bootstrap sampling distribution
c = 0

Boot_p_val = (sum(boot_estimates <= c - 30.63) + sum(boot_estimates >= c + 30.63))/100000
Boot_p_val

# We estimate the p-value to be 0.272 
# I.e., in 27,200 times out of 100,000, we had a difference between sample averages such extreme as this, assuming the difference between population means is 0.
# Thus, we cannot reject the null hypothesis.





#####
##### Approach 2: Bootstrap Tests
#####
### Step 1: Create transormed samples in which the null hypothesis is satisfied
c = 0
obs_val_comb = (sum(orig_sample1) + sum(orig_sample2))/(n1 + n2)
obs_val_comb

trans_sample1 = orig_sample1 - obs_val1 + obs_val_comb
mean(trans_sample1)

trans_sample2 = orig_sample2 - obs_val2 + obs_val_comb
mean(trans_sample2)

### Generate 100,000 Bootstrap samples (resamples) of sizes n1 and n2, independently, from the transformed samples
boot_samples1 = replicate(n=100000, sample(trans_sample1, n1, replace=TRUE), simplify=FALSE)
boot_samples2 = replicate(n=100000, sample(trans_sample2, n2, replace=TRUE), simplify=FALSE)

### Compute 100,000 Bootstrap estimates of the test statistic (i.e., the statistic of interest) and find
### the Bootstrap sampling distribution of the statistic 
boot_estimates = sapply(boot_samples1, FUN=mean, simplify=TRUE) - sapply(boot_samples2, FUN=mean, simplify=TRUE)

### Find the p-value. 
### The p-value is the probability of getting something as big or more extreme than what we observed.
### 30.63 (the observed difference) is 30.63 = 30.63 - 0 units away from the null hypothesis (in our case of no difference).
### So the p-value is the probability of being at least 33.63 units away from 0.
### Using the Bootstrap sampling distribution 
Boot_p_val = (sum(boot_estimates <= c - 30.63) + sum(boot_estimates >= c + 30.63))/1000
Boot_p_val

# We estimate the p-value to be 0.225 
# I.e., in 22,500 times out of 100,000, we had a sample average such extreme as this, assuming the population means are the same.
# Thus, we do not reject the null hypothesis.

