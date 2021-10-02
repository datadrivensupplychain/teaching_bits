#Central Limit Theorem Example
#no tidyverse packages, base R
rm(list=ls())


#exponential distribution, mean 5, rate 1/5 ----

#sample just 50 variates, 60000 times
sampled_exponential_variate_means <- numeric(length=60000)

for(i in 1:60000){ sampled_exponential_variate_means[i] <- mean(rexp(n=50,rate=1/5)) }

mean_sampled_exponential_means <- mean(sampled_exponential_variate_means)

stderror_exponential <- 5/sqrt(50)
stdev_samplemeans_exponential <- sd(sampled_exponential_variate_means)

#compare sampling mean, to population mean

mean_sampled_exponential_means/5

#compare standard error calculation, to standard deviation of the sample means
stderror_exponential/stdev_samplemeans_exponential

#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_exponential_variate_means)




#normal distribution: mean 30 SD 5 ----

#sample just 50 variates, 60000 times
sampled_normal_variate_means <- numeric(length=60000)
for(i in 1:60000){  sampled_normal_variate_means[i] <- mean(rnorm(n=50,mean=30,sd=5))}

mean_sampled_normal_means <- mean(sampled_normal_variate_means)

stderror_normal <- 5/sqrt(50)
stdev_samplemeans_normal <- sd(sampled_normal_variate_means)

#compare sampling mean, to population mean

mean_sampled_normal_means/30

#compare standard error calculation, to standard deviation of the sample means
stderror_normal/stdev_samplemeans_normal

#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_normal_variate_means)




#normal distribution in heights----
#adult men  height mean 70 inches , sd 3inches
#adult women height mean 64.5 inches , sd 2.5 inches
#assuming 50% men/50% women: mean 67.25, sd 3.9

sampled_normal_variate_means_heights <- numeric(length=60000)
for(i in 1:60000){  sampled_normal_variate_means_heights[i] <- mean(rnorm(n=50,mean=67.25,sd=3.9))}

mean_sampled_normal_means_height <- mean(sampled_normal_variate_means_heights)

stderror_normal_height <- 3.9/sqrt(50)
stdev_samplemeans_normal_height <- sd(sampled_normal_variate_means_heights)

#compare sampling mean, to population mean

mean_sampled_normal_means_height/67.25

#compare standard error calculation, to standard deviation of the sample means
stderror_normal_height/stdev_samplemeans_normal_height

#histogram of the sampled means, verify it looks close to normally distributed
hist(sampled_normal_variate_means_heights)



#95% CI example for heights. mean 67.25 SD 3.9.  se <- 3.9/sqrt(50)
ci_95_heights <- cbind.data.frame(sample_mean = sampled_normal_variate_means_heights,stringsAsFactors=FALSE) %>%
  dplyr::mutate(CI_95_Low = sample_mean-1.96*stderror_normal_height, CI_95_High = sample_mean+1.96*stderror_normal_height) %>%
  dplyr::mutate(population_mean_in_interval = dplyr::if_else(CI_95_Low<=67.25 & CI_95_High>=67.25, "Yes",'No'))
