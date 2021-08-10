#Central Limit Theorem Example
#no tidyverse packages, base R

#exponential distribution, mean 5, rate 1/5 ----
exponential_variates <- rexp(n=1000000,rate=1/5)

#sample just 50 of the 1M variates, 5000 times
sampled_exponential_variate_means <- numeric(length=5000)
sampled_exponential_variate_std <- numeric(length=5000)

for(i in 1:5000){
sampled_exponential_variates <- sample(x=exponential_variates,
                                       size=50,
                                       replace = FALSE)
sampled_exponential_variate_means[i] <- mean(sampled_exponential_variates)
sampled_exponential_variate_std[i] <- sd(sampled_exponential_variates)
}

#compare the mean of the exponential variates, to the mean of the sampled means
mean_exponential_variates <- mean(exponential_variates)
mean_sampled_exponential_variate_means <- mean(sampled_exponential_variate_means)
#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_exponential_variate_means)
#ratio will be very close to 1
mean_exponential_variates/mean_sampled_exponential_variate_means




#lognormal distribution, meanlog 0, sdlog 1 ----
lognormal_variates <- rlnorm(1000000, meanlog=0,sdlog=1)

#sample just 50 of the 1M variates, 5000 times
sampled_lognormal_variate_means <- numeric(length=5000)
sampled_lognormal_variate_std <- numeric(length=5000)

for(i in 1:5000){
  sampled_lognormal_variates <- sample(x=lognormal_variates,
                                         size=50,
                                         replace = FALSE)
  sampled_lognormal_variate_means[i] <- mean(sampled_lognormal_variates)
  sampled_lognormal_variate_std[i] <- sd(sampled_lognormal_variates)
}

#compare the mean of the lognormal variates, to the mean of the sampled means
mean_lognormal_variates <- mean(lognormal_variates)
mean_sampled_lognormal_variate_means <- mean(sampled_lognormal_variate_means)
#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_lognormal_variate_means)
#ratio will be very close to 1
mean_lognormal_variates/mean_sampled_lognormal_variate_means



#bimodal normal distribution: mean 30 SD 5, mean -20 SD 4 ----
normal_variates <- c(rnorm(n=500000,mean=30,sd=5),rnorm(n=500000,mean=-20,sd=4))


#sample just 50 of the 1M variates, 5000 times
sampled_normal_variate_means <- numeric(length=5000)
sampled_normal_variate_std <- numeric(length=5000)

for(i in 1:5000){
  sampled_normal_variates <- sample(x=normal_variates,
                                       size=50,
                                       replace = FALSE)
  sampled_normal_variate_means[i] <- mean(sampled_normal_variates)
  sampled_normal_variate_std[i] <- sd(sampled_normal_variates)
}

#compare the mean of the normal variates, to the mean of the sampled means
mean_normal_variates <- mean(normal_variates)
mean_sampled_normal_variate_means <- mean(sampled_normal_variate_means)
#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_normal_variate_means)
#ratio will be very close to 1
mean_normal_variates/mean_sampled_normal_variate_means
