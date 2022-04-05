#ompr / CBC parameter testing ----

library(tidyverse)
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.cbc)


model <- ompr::MILPModel()  %>%
  add_variable(x, type = "integer")  %>%
  add_variable(y, type = "continuous", lb = 0) %>%
  set_bounds(x, lb = 0)  %>%
  set_objective(x + y, "max")  %>%
  add_constraint(x + y <= 11.25)

model_soln <- model %>%   solve_model(with_ROI(solver = "cbc",
max_time=100,verbosity_level=1,ratioGap=0.4,feas="both"))
#verbosity_level = -1 (no verbosity). 1 or 2 gives verbosity.
#max_time is in seconds. ratioGap is in percentx100, so 0.4 is 40% gap between
#incumbent solution and current best possible solution
#feas is feasibility pump heursitic https://projects.coin-or.org/Cbc/wiki/fpump


####################################
####################################
####################################
####################################
####################################
####################################
#### GUROBI ##### 

#test gurobi with ompr

#install gurobi package, from Gurobi Optimizer install on machine

library(gurobi)


#ensure these path variables are set to Gurobi directory:

#>  Sys.getenv("GUROBI_HOME")
#[1] "C:\\gurobi951\\win64"
#>  Sys.getenv("LD_LIBRARY_PATH")
#[1] "C:\\gurobi951\\win64\\lib"


#remotes:::install_github("roigrp/ROI.plugin.gurobi", INSTALL_opts="--no-multiarch")

#if this error occurs,
# DLL 'gurobi' not found: maybe not installed for this architecture?
#then add the INSTALL_opts="--no-multiarch" argument
#https://github.com/roigrp/ROI.plugin.gurobi/issues/5 

library(ROI.plugin.gurobi)

library(tidyverse)
library(ompr)
library(ompr.roi)
#library(ROI.plugin.cbc)
result <- MIPModel() |>
  add_variable(x, type = "integer") |>
  add_variable(y, type = "continuous", lb = 0) |>
  set_bounds(x, lb = 0) |>
  set_objective(x + y, "max") |>
  add_constraint(x + y <= 11.25) |>
  solve_model(with_ROI(solver = "gurobi", OutputFlag =1, TimeLimit = 100, MIPGap = 0.05))

#https://rdrr.io/github/roigrp/ROI.plugin.gurobi/src/R/solver_controls.R
#OutputFlag=0 turns off verbosity, OutputFlag=1 turns on verbosity
#TimeLimit =  Presumably in seconds??
#MIPGap =  percentage (relative gap)

