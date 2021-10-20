#Monty Hall Simulation

library(tidyverse)
library(magrittr)

simulation_count <- 1000000

door_df <- cbind.data.frame(
#randomly assign door with prize
Door_With_Prize = sample(x=c('A','B','C'),size=simulation_count,replace=TRUE,prob=c(1/3,1/3,1/3)),
#randomly assign the door you initially choose
Door_You_Initially_Choose = sample(x=c('A','B','C'),size=simulation_count,replace=TRUE,prob=c(1/3,1/3,1/3)),
stringsAsFactors=FALSE) %>%

#new column: would it be better to stay (the Door_With_Prize is the same as Door_You_Initially_Choose)
#or to switch?

  
dplyr::mutate(
  Stay_or_Switch = dplyr::if_else(Door_With_Prize==Door_You_Initially_Choose,"Stay","Switch"))


#summarize results of the simulation: how often would it be better to Stay or to Switch?
#results confirm: 2/3rds of the time, better to switch.  Switching is a better strategy.
results <- door_df %>%
  dplyr::group_by(Stay_or_Switch) %>%
  dplyr::summarise(Count = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Pct_Correct_Choice =scales::percent(Count/sum(Count) ))

results
