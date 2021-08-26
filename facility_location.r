#learning R script on warehouse network design
#install packages if necessary first!
library(tidyverse)
library(magrittr)
library(leaflet)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

require(geosphere)
require(measurements)

# customer set are the largest cities in Texas

tx_cities <- maps::us.cities %>% dplyr::filter(country.etc=='TX') %>%
  #demand is proportional to population
  dplyr::mutate(demand = ceiling(pop/10))

#real quick leaflet of the cities
leaflet(tx_cities) %>% addTiles() %>% addMarkers()

#potential DC sets are Dallas, Amarillo, Houston, El Paso, San Antonio, Beaumont
tx_dc <- tx_cities %>% dplyr::filter(
  name %in% c('Dallas TX','Amarillo TX','Houston TX','El Paso TX',
              'San Antonio TX', 'Beaumont TX'))

#create a distance matrix between the demand points and DCs
customer_dc_distmat <- geosphere::distm(
  x=cbind(tx_cities$long,tx_cities$lat),
  y=cbind(tx_dc$long,tx_dc$lat)) %>% 
  #convert from meters (default) to miles
  measurements::conv_unit('m','mi')
row.names(customer_dc_distmat) = paste0('Customer_',tx_cities$name)
colnames(customer_dc_distmat) = paste0('DC_',tx_dc$name)

#create a matrix that is the metric you wish to minimize or maximize.
#in this example, the metric is unit-miles
#just multiply the demand by city, into the distance from that city to each DC
unitmiles_customer_dc_matrix <- 
  tx_cities$demand * customer_dc_distmat

customer_count <- nrow(tx_cities)
dc_option_count <- nrow(tx_dc)

#now make optimization model
dc_location_model <- ompr::MIPModel() %>%
  #integer decision variables: for each customer, which DC to align to?
  add_variable(customer_dc_align[customerindex,dcindex],
               customerindex=1:customer_count,
               dcindex=1:dc_option_count,type='binary') %>%
  #integer decision variable: open a DC or no?
  add_variable(open_dc_binary[dcindex],dcindex=1:dc_option_count,type='binary') %>%
  #first constraint: each customer aligned to 1 and only 1 DC
  add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                          dcindex=1:dc_option_count)==1,
                 customerindex=1:customer_count) %>%
  #add in "Big M" constraints that activate open_dc_binary when
  #any customers are aligned to a DC
  add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                          customerindex=1:customer_count)<=
                   99999*open_dc_binary[dcindex],dcindex=1:dc_option_count) %>%
  
  #limit the number of opened DCs to EXACTLY 2
  add_constraint(sum_expr(open_dc_binary[dcindex],dcindex=1:dc_option_count)==2) %>%
  #set objective function, the sumproduct
  #of the customer/DC alignment integer variables,
  #and the matrix of the unit-miles for each customer/DC pair
  #sense is either "min" or "max", minimize or maximize the values?
  set_objective(sum_expr(customer_dc_align[customerindex,dcindex]*
                           unitmiles_customer_dc_matrix[customerindex,dcindex],
                         customerindex=1:customer_count,
                         dcindex=1:dc_option_count),sense='min')
  
solution <- ompr::solve_model(dc_location_model,with_ROI(solver = "glpk"))

customer_dc_alignment_df <- get_solution(solution,customer_dc_align[customerindex,dcindex]) %>%
  dplyr::filter(value==1) %>%
  dplyr::select(customerindex,dcindex) %>%
  #add in customer and DC names and lat/long
  dplyr::mutate(Customer_City = tx_cities$name[customerindex],
                Customer_Lat = tx_cities$lat[customerindex],
                Customer_Lng = tx_cities$long[customerindex],
                DC_City = tx_dc$name[dcindex],
                DC_Lat = tx_dc$lat[dcindex],
                DC_Lng = tx_dc$long[dcindex]) %>%
  dplyr::select(Customer_City,Customer_Lat,Customer_Lng,
                DC_City,DC_Lat,DC_Lng)

#verify only two DCs selected
dc_cities_selected <- unique(customer_dc_alignment_df$DC_City)

#leaflet: color customer city by aligned DC.
customer_dc_alignment_df %<>% dplyr::mutate(
  leaflet_dc_color = dplyr::if_else(DC_City==dc_cities_selected[1],'red','blue'))

leaflet(customer_dc_alignment_df) %>% addTiles() %>%
  addCircleMarkers(lat=~Customer_Lat,lng=~Customer_Lng,
                   color=~leaflet_dc_color,radius=3)


#note that this optimization model is completely unconstrained in the number of customers
#or volume that can be aligned to a given warehouse. Also you would generally
#want to consider more than just 5 options for DC locations.
