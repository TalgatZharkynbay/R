require(moments)
X=rnorm(n=1000000, mean = 2, sd=3)
round(skewness(X), digits = 3)
kurtosis(X)
