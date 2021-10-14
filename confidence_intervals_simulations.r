library(tidyverse)
library(magrittr)
rm(list=ls())


#z distribution with population stdev known, large sample.  can sample from whatever population distribution you want ----
ci_pct <- 0.95
sample_size <- 40

means <- numeric()

for(i in 1:200000){
  #k is a sample from the population
  k <- rnorm(n = sample_size , mean = 45, sd =3) #any population distribution can be used for k, if you specify stdev
  means[i] <- mean(k)
}

zdist_results_df <- as.data.frame(means,stringsAsFactors=FALSE) %>%
  dplyr::mutate(stderror = 3 / sqrt(sample_size)) %>% #3 is population standard deviation, replace with variable if desired
  dplyr::mutate(CI_Low = means - qnorm(1- (1-ci_pct)/2)*stderror,
                CI_High =means + qnorm(1- (1-ci_pct)/2)*stderror) %>%
  dplyr::mutate(Population_Mean_In_CI = (45 >= CI_Low) & (45 <= CI_High))
                  
#Population_Mean_In_CI is boolean, so mean of that column is % of confidence intervals
#containing population mean
mean(zdist_results_df$Population_Mean_In_CI)



#t distribution with normally distributed population, population stdev unknown in sampling ----
ci_pct <- 0.9
sample_size <- 15
normal_popn_mean <- 30
normal_popn_sd <- 5

means <- numeric()
mfe <- numeric()

for(i in 1:200000){
  
  #sample of size sample_size, df sample_size-1 from the normally distributed population
  k <- rnorm(sample_size,normal_popn_mean,normal_popn_sd)
  means[i] <- mean(k)
  mfe[i] <- qt(1- (1-ci_pct)/2, (sample_size-1)) * ( sd(k)/sqrt(sample_size) ) #t value * SE
}

tdist_results_df <- cbind.data.frame(means = means,mfe = mfe, stringsAsFactors=FALSE) %>%
  dplyr::mutate(CI_Low = means - mfe, CI_High =means+mfe) %>%
  dplyr::mutate(Population_Mean_In_CI = (CI_Low <= normal_popn_mean) & (CI_High >= normal_popn_mean))

mean(tdist_results_df$Population_Mean_In_CI)

######


#binomial distribution, known p, assume np>=15 and nq>=15
ci_pct <- 0.9
p <- 0.46
q <- 1-p
sample_size = 400


p_hat <- numeric()

for(i in 1:200000){
  k <- rbinom(n=sample_size, size = 1, prob = p)
  p_hat[i] <- mean(k)
  
}

proportions_df <- as.data.frame(p_hat,stringsAsFactors=FALSE) %>%
  dplyr::mutate(CI_Low = p_hat - qnorm(1- (1-ci_pct)/2)*sqrt(p*q/sample_size),
                CI_High = p_hat + qnorm(1- (1-ci_pct)/2)*sqrt(p*q/sample_size)) %>%
  dplyr::mutate(Population_Prop_In_CI = (CI_Low <= p) & (CI_High >= p))
