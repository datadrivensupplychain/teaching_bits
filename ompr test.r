library(tidyverse)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

result <- MIPModel() |>
  add_variable(x, type = "integer") |>
  add_variable(y, type = "continuous", lb = 0) |>
  set_bounds(x, lb = 0) |>
  set_objective(x + y, "max") |>
  add_constraint(x + y <= 11.25) |>
  solve_model(with_ROI(solver = "glpk"))
