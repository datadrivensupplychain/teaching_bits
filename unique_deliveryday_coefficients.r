#proof of concept on tying binary variables (delivery days) to acceptable schedules in MILP ----
library(tidyverse)
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.symphony)

rm(list=ls())
gc(T,T,T)
options(scipen=9999999)

#test a sequence of 7 numbers to ensure that each combination of the numbers yields a unique sum ----

#first element is for Sunday, second element Monday, ... seventh element Saturday

#using Conway-Guy Sequence https://oeis.org/A096858
#{20,31,37,40,42,43,44}
coefficient_vector <- c(20,31,37,40,42,43,44)


#generate all the potential options of delivery days, from 1 to 7 days per week

delivery_list <- list()

for(i in 1:7){
k <- combn(x=1:7, m=i)
for(j in 1:ncol(k)){
  m <- rep(0, 7)
  m[k[,j]] <- 1
  df <- as.data.frame(t(m))
  total <- sum(as.numeric(df) * coefficient_vector)
  daycount <- sum(as.numeric(df))
  df$DayCount <- daycount
  df$Coefficient <- total
  
  p <- length(delivery_list)
  delivery_list[[p+1]] <- df
}
}
delivery_df <- delivery_list %>% dplyr::bind_rows() %>%
  dplyr::rename(Sunday=V1,Monday=V2,Tuesday=V3,Wednesday=V4,
                Thursday=V5,Friday=V6,Saturday=V7) %>%
  dplyr::mutate(
    max_lag_daycount = NA)

#identify max gap in deliveries

for(i in 1:nrow(delivery_df)){
  #find max gap.
  k <- delivery_df[i,c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')] %>%
    as.numeric()
  #move to next row if 1x weekly
  if(sum(k) == 1){
    delivery_df$max_lag_daycount[i] <- 7
    next()}
  
  #duplicate week to avoid end of week gaps
  k <- c(k,k)
  
  #find indices for delivery days, calculate lag, and identify max lag
  m <- which(k==1)
  n <- m - lag(m)
  n <- n[is.na(n)==FALSE]
  
  delivery_df$max_lag_daycount[i] <- max(n)
  }
  
#should be same as number of rows in delivery_df
length(unique(delivery_df$Coefficient))
  
#now identify if a given delivery schedule has sufficient spacing:
# 1x weekly: all allowed
# 2x weekly: at most 4 day gap (Monday & Friday = 4 day gap)
# 3x weekly: at most 3 day gap
# 4x weekly: at most 2 day gap
# 5x to 7x weekly: all allowed
  
delivery_df <- delivery_df %>%
  dplyr::mutate(delivery_schedule_allowed = dplyr::case_when(
    DayCount == 1 ~ TRUE,
    (DayCount==2 & max_lag_daycount<= 4) ~ TRUE,
    (DayCount==3 & max_lag_daycount<= 3) ~ TRUE,
    (DayCount==4 & max_lag_daycount<= 2) ~ TRUE,
    (DayCount >= 5) ~ TRUE,
    TRUE ~ FALSE)) %>%
  dplyr::filter(delivery_schedule_allowed==TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Schedule_Number= dplyr::row_number())


    
#clear workspace of unnecesary objects
rm(list=setdiff(ls(), c('delivery_df','coefficient_vector')))
schedule_count <- nrow(delivery_df)

#test optimization model
#create a vector to give the objective function "guidance" on whichschedule to pick
obj_runif <- runif(n=schedule_count,min=0,max=100)

testmodel <- ompr::MILPModel() %>%
  #binary variables for each day of delivery
  add_variable(delivery_day[dayindex], dayindex=1:7,type="binary") %>%
  #binary variables: select schedule yes or no?
  add_variable(select_schedule[scheduleindex], scheduleindex =1:schedule_count,type="binary") %>%
  #constraint: only single schedule selected
  add_constraint(sum_expr(select_schedule[scheduleindex], scheduleindex=1:schedule_count)==1) %>%
  
  #constraint: sumproduct of day binaries * coefficient_vector, equals select_schedule * coefficients
  
  add_constraint( 
    #LHS : schedule binaries
    (sum_expr(select_schedule[scheduleindex] * ompr::colwise(delivery_df$Coefficient[scheduleindex]),scheduleindex=1:schedule_count)) == 
    #RHS: day binaries *    coefficient_vector
  (sum_expr(delivery_day[dayindex] * ompr::colwise(coefficient_vector[dayindex]), dayindex=1:7)))  %>%
  
  #objective function: maximize obj_runif
  set_objective(sum_expr(select_schedule[scheduleindex] * ompr::colwise(obj_runif[scheduleindex]),scheduleindex=1:schedule_count),
                sense='max')

#hold onto your butts!

solution <- ompr::solve_model(testmodel,with_ROI(solver = "symphony",time_limit=100,
gap_limit = 0, first_feasible = FALSE, verbosity = 1 ))

#retrieve solution
dayindex_solution <- ompr::get_solution(solution, delivery_day[dayindex])
  
scheduleindex_solution <- ompr::get_solution(solution, select_schedule[scheduleindex])

#check for consistency betwen solutions
k <- sum(dayindex_solution$value*coefficient_vector)
m <- sum(scheduleindex_solution$value * delivery_df$Coefficient)

if(k != m){stop("mismatch of coefficient values")}
#pairwise comparison, mean should be 1 indicating all TRUE
p <- mean( as.numeric( delivery_df[which(scheduleindex_solution$value==1), 1:7] )== as.numeric( dayindex_solution$value))
if(p != 1){stop("pairwise comparison failed")}