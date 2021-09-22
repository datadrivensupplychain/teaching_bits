#summarize results for coin flipping experiment ----

library(tidyverse)
library(magrittr)
rm(list=ls())

coincount <- 100
repeats_count <- 5000
p_heads = .5


##simulation -----

#simulate 10000 students, flipping 100 coins each:
ht_matrix <- matrix(nrow=coincount,ncol = repeats_count,data=NA)
for(i in 1:repeats_count){
  #size of experiment: 1 coin.  value of 1 "success" = 1 head. value of 0 "success" = 1 tail.
  ht_matrix[,i] <- ifelse(rbinom(n=coincount,size=1,prob=p_heads)==1,"H","T")
}

heads_p_sim <- colMeans(ht_matrix=='H')
#find longest heads run
longest_heads_run_sim <- numeric()
longest_tails_run_sim <- numeric()

for(i in 1:repeats_count){
  
  if(i %% 100==0){print(i)}
  rle_out <- rle(x= ht_matrix[,i])
  df1 <- cbind.data.frame(lengths=rle_out$lengths,values=rle_out$values)
  longest_heads_run_sim[i] <- max(df1$lengths[df1$values=='H'])
  longest_tails_run_sim[i] <- max(df1$lengths[df1$values=='T'])
  
}

#histogram and frequency tables of longest_heads_run_sim and longest_tails_run_sim:
#helps establish probability that coincount flips will yield a string with N heads or tails in a row
#can also be used to judge if data is faked...



longest_heads_run_sim_freqtable <- longest_heads_run_sim %>% table() %>% as.data.frame() %>%
  dplyr::mutate(pct_instances = Freq/sum(Freq)) %>%
  dplyr::mutate(cumpct_instances = cumsum(pct_instances)) %>%
  dplyr::select(Longest_Run = '.', Count_Repeats = Freq,
                Pct_Repeats = pct_instances, Cumulative_Pct_Instances =cumpct_instances )

longest_tails_run_sim_freqtable <- longest_tails_run_sim %>% table() %>% as.data.frame() %>%
  dplyr::mutate(pct_instances = Freq/sum(Freq)) %>%
  dplyr::mutate(cumpct_instances = cumsum(pct_instances)) %>%
  dplyr::select(Longest_Run = '.', Count_Repeats = Freq,
                Pct_Repeats = pct_instances, Cumulative_Pct_Instances =cumpct_instances )

longest_heads_run_histogram <- ggplot2::ggplot(data=longest_heads_run_sim_freqtable,
aes(x=Longest_Run,y=Pct_Repeats))+geom_bar(stat='identity') +
  ggtitle(paste0("Longest Run of Heads in ",coincount," Coin Flips, by % of Time Occurring"))+
  scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.02)) 

longest_tails_run_histogram <- ggplot2::ggplot(data=longest_tails_run_sim_freqtable,
                                               aes(x=Longest_Run,y=Pct_Repeats))+geom_bar(stat='identity') +
  ggtitle(paste0("Longest Run of Tails in ",coincount," Coin Flips, by % of Time Occurring"))+
  scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.02)) 


pct_heads_histogram <- ggplot2::ggplot(data=heads_p_sim %>% as.data.frame(),
aes(x=`.`))+geom_histogram(bins=100) + 
  ggtitle(paste0("Proportion of Flips that are Heads, ",repeats_count," Repeats of ",coincount," Coin Flips"))+
  scale_x_continuous(breaks=seq(0,1,0.02))+
  xlab("Proportion of Flips that were Heads")+
  ylab(paste0("Count of Trials (",repeats_count," Repeats Total)"))

