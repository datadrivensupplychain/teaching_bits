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
