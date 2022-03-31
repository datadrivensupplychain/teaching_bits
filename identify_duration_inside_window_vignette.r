#vignette to demonstrate constraints that will identify portion of a time window,
#that is overlapped with another time window.

#this can be used to identify what portion of an activity lies within a timeframe to penalize
#(like a weekend) or a larger time horizon (like a month) for summing of activity.
library(tidyverse)
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.symphony)
library(ROI.plugin.glpk)
rm(list=ls())
gc(T,T,T)

#identify start and duration of the activity
activity_start <- 24
activity_duration <- 2.2
activity_end <- activity_start + activity_duration

#identify the start and end time of the window (e.g., weekend)
window_start <- 20
window_end <- 23

#build linear model, objective function is just to calculate the overlap size

overlap_milp <- ompr::MILPModel() %>%
  
  add_variable(alpha , type ='continuous', lb=0) %>%
  add_variable(beta , type ='continuous', lb=0) %>%
  add_variable(overlap_length, type ='continuous', lb=0) %>%
   
  #because k is lower-bounded at 0, adding this constraint effectively makes
  # k = max(0, window_end - activity_end)
  add_constraint(beta >= window_end - activity_end) %>%
  add_constraint(alpha >= activity_start - window_start) %>%
  add_constraint(overlap_length >= window_end - window_start - alpha-beta) %>%
  
  #real objective is to minimize overlap_length.  HOWEVER, you need to
  #penalize increasing alpha or beta beyond their constrained minimums,
  #which would reduce overlap_length to zero.
  #heavily penalize increasing alpha or beta to reduce overlap_length RHS
  set_objective(overlap_length + 5000*alpha + 5000*beta , sense = 'min')


solution <- ompr::solve_model(overlap_milp,with_ROI(solver = "symphony"))
solution$objective_value
solution$solution
