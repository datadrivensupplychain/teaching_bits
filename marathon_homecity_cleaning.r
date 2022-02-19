library(tidyverse)
library(magrittr)
require(maps)
require(countrycode)

rm(list=ls())
gc(T,T,T)


marathon_data <- readr::read_csv("parsed_marathon_data_2.csv")
country_names_uppercase1 <- maps::world.cities$country.etc %>% unique() %>% stringr::str_to_upper()
country_names_uppercase2 <- countrycode::codelist$country.name.en %>% unique() %>% stringr::str_to_upper()
country_abbrv_uppercase1 <- countrycode::codelist$ioc %>% unique() %>% stringr::str_to_upper()

country_names_uppercase <- unique(c(country_names_uppercase1,country_names_uppercase2,country_abbrv_uppercase1))




country_vec <- character(length = nrow(marathon_data))
state_province_vec <-  character(length = nrow(marathon_data))
city_vec <-  character(length = nrow(marathon_data))


last_component_vec <-  character(length = nrow(marathon_data))

for(rowindex in 1:nrow(marathon_data)){
  if(rowindex %%1000 ==0){print(rowindex)}
  k <- stringr::str_split(marathon_data$raw_results[rowindex], pattern=" ")
  
  #coerce to all uppercase
  k <- stringr::str_to_upper(k[[1]])
  #identify presumed country index within k
  country_index <- which(k %in% c('USA','US','CAN','UNITED STATES','UNITED STATES OF AMERICA','UNITED KINGDOM','BRASIL', country_names_uppercase))
  if(length(country_index)>0){country_vec[rowindex] <-k[country_index[1]]}
  
  if(length(country_index)==0){
    #test to see if last component is a time or age
    last_component <- k[length(k)]
    
    #try test for numeric (age)
    if( is.na(try(as.numeric(last_component)))==FALSE){
      country_vec[rowindex] <- 'PRESUMED_NO_COUNTRY_LAST_COMPONENT_AGE' 
      }
    
    #test for finish time
    if( is.na(try(lubridate::hms(last_component)))==FALSE){
      country_vec[rowindex] <- 'PRESUMED_NO_COUNTRY_LAST_COMPONENT_FINISHTIME'
      }
    
    if(country_vec[rowindex]==""){ country_vec[rowindex] <- 'LASTWORD_PRESUMED_NOT_COUNTRY'}
  
    #next test for age groups
    
    
    
    last_component_vec[rowindex] <- last_component
    }

  
}


marathon_data$last_component_vec <- last_component_vec
marathon_data$country_vec <- country_vec



