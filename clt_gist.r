#Central Limit Theorem Example
#no tidyverse packages, base R
rm(list=ls())

##normal
#exponential distribution, mean 5, rate 1/5 ----
exponential_variates <- rexp(n=1000000,rate=1/5)

#sample just 50 of the 1M variates, 5000 times
sampled_exponential_variate_means <- numeric(length=5000)

for(i in 1:5000){
sampled_exponential_variates <- sample(x=exponential_variates,
                                       size=50,
                                       replace = FALSE)
sampled_exponential_variate_means[i] <- mean(sampled_exponential_variates)
}

#compare the mean of the exponential variates, to the mean of the sampled means
mean_exponential_variates <- mean(exponential_variates)
mean_sampled_exponential_variate_means <- mean(sampled_exponential_variate_means)

sd_exponential_variates <- sd(exponential_variates)
sd_estimate_exponential_sampled_means <- sd(sampled_exponential_variate_means)*sqrt(50)


#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_exponential_variate_means)
#means ratio will be very close to 1
mean_exponential_variates/mean_sampled_exponential_variate_means
#sd ratio will be very close to 1
sd_exponential_variates/sd_estimate_exponential_sampled_means

##lognormal ----
#lognormal distribution, meanlog 0, sdlog 1 
lognormal_variates <- rlnorm(1000000, meanlog=0,sdlog=1)

#sample just 50 of the 1M variates, 5000 times
sampled_lognormal_variate_means <- numeric(length=5000)

for(i in 1:5000){
  sampled_lognormal_variates <- sample(x=lognormal_variates,
                                         size=50,
                                         replace = FALSE)
  sampled_lognormal_variate_means[i] <- mean(sampled_lognormal_variates)
}

#compare the mean of the lognormal variates, to the mean of the sampled means
mean_lognormal_variates <- mean(lognormal_variates)
mean_sampled_lognormal_variate_means <- mean(sampled_lognormal_variate_means)

sd_lognormal_variates <- sd(lognormal_variates)
sd_estimate_lognormal_sampled_means <- sd(sampled_lognormal_variate_means)*sqrt(50)


#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_lognormal_variate_means)
#means ratio will be very close to 1
mean_lognormal_variates/mean_sampled_lognormal_variate_means
#sd ratio will be very close to 1
sd_lognormal_variates/sd_estimate_lognormal_sampled_means




#unimodal normal distribution: mean 30 SD 5 ----
normal_variates <- rnorm(n=1000000,mean=30,sd=5)


#sample just 50 of the 1M variates, 5000 times
sampled_normal_variate_means <- numeric(length=5000)

for(i in 1:5000){
  sampled_normal_variates <- sample(x=normal_variates,
                                       size=50,
                                       replace = FALSE)
  sampled_normal_variate_means[i] <- mean(sampled_normal_variates)
}

#compare the mean of the normal variates, to the mean of the sampled means
mean_normal_variates <- mean(normal_variates)
mean_sampled_normal_variate_means <- mean(sampled_normal_variate_means)

sd_normal_variates <- sd(normal_variates)
sd_estimate_normal_sampled_means <- sd(sampled_normal_variate_means)*sqrt(50)


#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_normal_variate_means)
#means ratio will be very close to 1
mean_normal_variates/mean_sampled_normal_variate_means
#sd ratio will be very close to 1
sd_normal_variates/sd_estimate_normal_sampled_means




#bimodal normal distribution ----

bimodal_normal_variates <- c(rnorm(n=500000,mean=30,sd=5),rnorm(n=500000,mean=-20,sd=4))


#sample just 50 of the 1M variates, 5000 times
sampled_bimodal_normal_variate_means <- numeric(length=5000)

for(i in 1:5000){
  sampled_bimodal_normal_variates <- sample(x=bimodal_normal_variates,
                                    size=50,
                                    replace = FALSE)
  sampled_bimodal_normal_variate_means[i] <- mean(sampled_bimodal_normal_variates)
}

#compare the mean of the normal variates, to the mean of the sampled means
mean_bimodal_normal_variates <- mean(bimodal_normal_variates)
mean_sampled_bimodal_normal_variate_means <- mean(sampled_bimodal_normal_variate_means)

sd_bimodal_normal_variates <- sd(bimodal_normal_variates)
sd_bimodal_estimate_normal_sampled_means <- sd(sampled_bimodal_normal_variate_means)*sqrt(50)



#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_bimodal_normal_variate_means)
#ratio will be very close to 1
mean_bimodal_normal_variates/mean_sampled_bimodal_normal_variate_means
#sd ratio will be very close to 1
sd_bimodal_normal_variates/sd_bimodal_estimate_normal_sampled_means


