library(tidyverse)
library(ompr)
library(ompr.roi)
library(gurobi)
library(ROI.plugin.gurobi)

rm(list=ls())

set.seed(42)
array1 <- array(dim=c(5,10,7),data=runif(5*10*7))

#define this function for 3d array multiplication using MILPModel
array_3d_multiplication_fcn <- function(static_array, row_variable, column_variable,dim3_variable){
  vapply(seq_along(static_array), function(k) static_array[
    row_variable[k], column_variable[k], dim3_variable[k]], numeric(1L))  }


milp_model <- ompr::MILPModel() %>%
  add_variable(assign_units[rowindex,colindex,dim3index], 
               rowindex=1:5,colindex=1:10,dim3index=1:7,type='binary') %>%
  #select 27 binaries, total == 27 through next constraint
  add_constraint(sum_expr(assign_units[rowindex,colindex,dim3index], 
                          rowindex=1:5,colindex=1:10,dim3index=1:7 ) ==27) %>%
  
  #sum of binaries * mat1 <= 7
  add_constraint( sum_expr( ompr::colwise(
    array_3d_multiplication_fcn(static_array=array1,
                                row_variable=rowindex,column_variable=colindex,dim3_variable=dim3index)) *
      assign_units[rowindex,colindex,dim3index], rowindex=1:5,colindex=1:10,dim3index=1:7) <= 7)  %>%
  
  #objective: maximize value
  set_objective(sum_expr( ompr::colwise(
    array_3d_multiplication_fcn(static_array=array1,
                                row_variable=rowindex,column_variable=colindex,dim3_variable=dim3index)) *
      assign_units[rowindex,colindex,dim3index], rowindex=1:5,colindex=1:10,dim3index=1:7),sense='max')

milp_model_out <-   milp_model %>% ompr::solve_model(with_ROI(solver='gurobi'))


soln_out <- get_solution(milp_model_out,assign_units[rowindex,colindex,dim3index]) %>% dplyr::filter(value==1) %>%
  dplyr::group_by(dplyr::row_number()) %>%
  dplyr::mutate(array_value = array1[rowindex,colindex,dim3index])
soln_out
sum(soln_out$array_value)


#4d version ----


rm(list=ls())

set.seed(42)
array1 <- array(dim=c(5,10,7,4),data=runif(5*10*7*4))

#define this function for 4d array multiplication using MILPModel
array_4d_multiplication_fcn <- function(static_array, row_variable, column_variable,dim3_variable,dim4_variable){
  vapply(seq_along(static_array), function(k) static_array[
    row_variable[k], column_variable[k], dim3_variable[k],dim4_variable[k]], numeric(1L))  }


milp_model <- ompr::MILPModel() %>%
  add_variable(assign_units[rowindex,colindex,dim3index,dim4index], 
               rowindex=1:5,colindex=1:10,dim3index=1:7,dim4index=1:4,type='binary') %>%
  #total binaries ==27
  add_constraint(sum_expr(assign_units[rowindex,colindex,dim3index,dim4index], 
                          rowindex=1:5,colindex=1:10,dim3index=1:7,dim4index=1:4 ) == 27) %>%
  
  #sum of binaries * mat1 <= 7
  add_constraint( sum_expr( ompr::colwise(
    array_4d_multiplication_fcn(static_array=array1,
                                row_variable=rowindex,column_variable=colindex,dim3_variable=dim3index,dim4_variable=dim4index)) *
      assign_units[rowindex,colindex,dim3index,dim4index], 
    rowindex=1:5,colindex=1:10,dim3index=1:7,dim4index=1:4) <= 7)  %>%
  
  #objective: maximize value
  set_objective(sum_expr( ompr::colwise(
    array_4d_multiplication_fcn(static_array=array1,
                                row_variable=rowindex,column_variable=colindex,dim3_variable=dim3index,dim4_variable=dim4index)) *
      assign_units[rowindex,colindex,dim3index,dim4index],
    rowindex=1:5,colindex=1:10,dim3index=1:7,dim4index=1:4),sense='max')

milp_model_out <-   milp_model %>% ompr::solve_model(with_ROI(solver='gurobi'))


soln_out <- get_solution(milp_model_out,assign_units[rowindex,colindex,dim3index,dim4index]) %>%
  dplyr::group_by(dplyr::row_number()) %>%
  dplyr::mutate(array_value = array1[rowindex,colindex,dim3index,dim4index])
soln_out
sum(soln_out$array_value)



###5d version ----

rm(list=ls())

set.seed(42)
array1 <- array(dim=c(5,10,7,4,2),data=runif(5*10*7*4*2))

#define this function for 4d array multiplication using MILPModel
array_5d_multiplication_fcn <- function(static_array, row_variable, column_variable,dim3_variable,dim4_variable,dim5_variable){
  vapply(seq_along(static_array), function(k) static_array[
    row_variable[k], column_variable[k], dim3_variable[k],dim4_variable[k], dim5_variable[k]], numeric(1L))  }  


milp_model <- ompr::MILPModel() %>%
  add_variable(assign_units[rowindex,colindex,dim3index,dim4index,dim5index], 
               rowindex=1:5,colindex=1:10,dim3index=1:7,dim4index=1:4,dim5index=1:2,type='binary') %>%
  #total binaries ==27
  add_constraint(sum_expr(assign_units[rowindex,colindex,dim3index,dim4index,dim5index], 
                          rowindex=1:5,colindex=1:10,dim3index=1:7,dim4index=1:4,dim5index=1:2 ) == 27) %>%
  
  #sum of binaries * mat1 <= 7
  add_constraint( sum_expr( ompr::colwise(
    array_5d_multiplication_fcn(static_array=array1,
                                row_variable=rowindex,column_variable=colindex,
                                dim3_variable=dim3index,dim4_variable=dim4index,dim5_variable=dim5index)) *
      assign_units[rowindex,colindex,dim3index,dim4index,dim5index], 
    rowindex=1:5,colindex=1:10,dim3index=1:7,dim4index=1:4,dim5index=1:2) <= 7)  %>%
  
  #objective: maximize value
  set_objective(sum_expr( ompr::colwise(
    array_5d_multiplication_fcn(static_array=array1,
row_variable=rowindex,column_variable=colindex,dim3_variable=dim3index,dim4_variable=dim4index,
dim5_variable = dim5index)) *
      assign_units[rowindex,colindex,dim3index,dim4index,dim5index],
    rowindex=1:5,colindex=1:10,dim3index=1:7,dim4index=1:4,dim5index=1:2),sense='max')

milp_model_out <-  milp_model %>% ompr::solve_model(with_ROI(solver='gurobi'))


soln_out <- get_solution(milp_model_out,assign_units[rowindex,colindex,dim3index,dim4index,dim5index]) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(array_value = array1[rowindex,colindex,dim3index,dim4index,dim5index])

