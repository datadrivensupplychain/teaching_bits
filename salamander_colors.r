#markov chain model for salamander transformation

library(tidyverse)
library(magrittr)
library(markovchain)

rm(list=ls())

colordf <- cbind.data.frame(red = seq(0,30,1),stringsAsFactors=FALSE) %>%
  tidyr::crossing(green = seq(0,30,1)) %>%
  tidyr::crossing(blue = seq(0,30,1)) %>%
  dplyr::filter(red + green + blue == 30)

current_state_df <- colordf %>% dplyr::rename(red_0 = red, green_0 = green, blue_0 = blue) %>%
  dplyr::mutate(currentstate = paste0("rgb_",red_0,"_",green_0,"_",blue_0))

next_state_df <- colordf %>% dplyr::rename(red_1 = red, green_1 = green, blue_1 = blue) %>%
  dplyr::mutate(nextstate = paste0("rgb_",red_1,"_",green_1,"_",blue_1))


crossjoin_states <- current_state_df %>% tidyr::crossing(next_state_df) %>%
  
  #assume all meetings are equally likely based upon current color mix
dplyr::mutate(transition_probability = dplyr::case_when(
( (red_1 == red_0 - 2) & (blue_1==blue_0 + 1) & (green_1==green_0 + 1) ) ~ red_0*(red_0-1)/(30*29), 
( (red_1 == red_0 + 1) & (blue_1==blue_0 - 2 ) & (green_1==green_0 + 1) ) ~  blue_0*(blue_0-1)/(30*29), 
( (red_1 == red_0 + 1) & (blue_1==blue_0 + 1 ) & (green_1==green_0 -2 ) ) ~  green_0*(green_0-1)/(30*29),
( (red_1 == red_0 - 1) & (blue_1==blue_0 + 2 ) & (green_1==green_0 -1 ) ) ~ 2*red_0*green_0/(30*29),
( (red_1 == red_0 - 1) & (blue_1==blue_0 - 1 ) & (green_1==green_0 + 2 ) ) ~ 2*red_0*blue_0/(30*29),
( (red_1 == red_0 + 2) & (blue_1==blue_0 - 1 ) & (green_1==green_0 - 1 ) ) ~ 2*blue_0*green_0/(30*29),
#all other transitions impossible per rules of the problem
TRUE ~ 0) )

currentstate_vector <- unique(crossjoin_states$currentstate)
nextstate_vector <- unique(crossjoin_states$nextstate)

transmatrix <- matrix(nrow=length(currentstate_vector), ncol=length(nextstate_vector),data=NA)
rownames(transmatrix) <- currentstate_vector
colnames(transmatrix) <- nextstate_vector

for(i in 1:nrow(crossjoin_states)){
  
  k <- which(currentstate_vector == crossjoin_states$currentstate[i])
  m <- which(nextstate_vector == crossjoin_states$nextstate[i])
  transmatrix[k,m] <- crossjoin_states$transition_probability[i]
  }

markov_transmat <- new("markovchain",states=currentstate_vector, transitionMatrix=transmatrix )

markovchain::is.accessible(object= markov_transmat,from="rgb_15_7_8",to="rgb_30_0_0")
#[1] FALSE