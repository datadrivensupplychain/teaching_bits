#Discrete Event Simulation Example: Inbound, PIPO Storage, Outbound
library(tidyverse)
library(magrittr)
library(simmer)
library(simmer.plot)

#clear workspace and run Garbage Collector
rm(list=ls())
gc(T,T,T)
#no scientific notation
options(scipen=999)

#in this simulation, five trucks arrive at at dock.
#each truck has a different number of pallets, and the unload time is dependent upon the number of pallets
#on the truck.

truck_count <- 5

inbound_truck_df <- data.frame( 
  Name = seq(1,truck_count,1),
  stringsAsFactors=FALSE) %>%
  dplyr::group_by(rownum = dplyr::row_number()) %>%
#trucks arrive at time 0 for the first truck, 30 for the second truck, 60 for thrid truck
  dplyr::mutate(Inbound_Time = 30*Name) %>%
  #pallet count sampled from [21,22,23,24,25] with equal probability
  dplyr::mutate(Pallet_Count = sample(x=c(21,22,23,24,25),size=1))



#create a vector of pallet inbound times 
ib_time_vec <- numeric()
ib_truckname_vec <- numeric()
for(i in 1:nrow(inbound_truck_df)){
  ib_time_vec <- c(ib_time_vec, rep(inbound_truck_df$Inbound_Time[i],inbound_truck_df$Pallet_Count[i]))
}
ib_time_vec <- sort(ib_time_vec)


pallet_df <- cbind.data.frame(Inbound_Arrival_Time = ib_time_vec,  stringsAsFactors=FALSE) %>%
  #identify Batch Size and dock seizure for both inbound and outbound
  dplyr::group_by(Inbound_Arrival_Time) %>%
  
  dplyr::mutate(    Inbound_Batch_Size = dplyr::n(),
                    Unload_Time = runif(1,0.9,1.1)*Inbound_Batch_Size )

#create simulation ----

#Entities in the simulation:
#Inbound Trucks
#pallets - these move through the system and utilize resources on a trajectory
#Docks

#Simulation flow: pallets "arrive" in batches, "seize" a truck, then are unloaded
warehouse_env <- simmer(name="pallets_movement", verbose=TRUE) 


pallets_trajectory <- trajectory("pallets_movement") %>%  
  set_global("truck_unload_timeout",function() {get_attribute(warehouse_env,"Unload_Time")}) %>%
  batch(n= function() {get_attribute(warehouse_env, "Inbound_Batch_Size")}, permanent=FALSE) %>%
  seize("truck",1) %>%
  seize("dock",1) %>%
  timeout(function() get_global(warehouse_env, "truck_unload_timeout")) %>%
  release("truck") %>%
  release("dock")




#Trajectory complete.  Now create a simmer environment

warehouse_env <- warehouse_env %>%
  #add truck as a resource, (effectively) infinite capacity
  add_resource("truck",capacity=Inf) %>%
  #add dock as a resource, with capacity 5.
  add_resource("dock",capacity=5) %>%
  #use the pallet_df dataframe as the arrivals dataframe
  add_dataframe(name_prefix="Pallet", trajectory = pallets_trajectory, data=pallet_df,
                col_attributes = c("Inbound_Arrival_Time","Inbound_Batch_Size","Unload_Time"),
                col_time = "Inbound_Arrival_Time", time="absolute")



#now that we have created the environment and trajectory, run the simulation
sim_out <- warehouse_env %>% run()
  
pallet_arrivals_df <- simmer::get_mon_arrivals(sim_out,per_resource=TRUE) %>%
  dplyr::filter(resource=='truck') %>%
  dplyr::filter(stringr::str_detect(string=name,pattern="Pallet")) %>%
  #by default, arrivals are index 0
  dplyr::mutate(Arrival_Number = stringr::str_replace(name,"Pallet","") %>% as.numeric() +1) %>%
  dplyr::arrange(start_time)
