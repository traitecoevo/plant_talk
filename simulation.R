library(tree)
dir.create("output", FALSE)
## This is fairly ugly, but finds the reference functions within the
## current setup of the SIEF_BDKD project.  These functions will move
## into tree soon, and this can then disappear.
source("../../../../tree/reference/reference-fun.R")

cohort.introduction.times <- function(max.time, multiplier=0.2,
                                      min.step.size=1e-05,
                                      max.step.size=2) {
  if (min.step.size <= 0)
    stop("The minimum step size must be greater than zero")
  trim <- function(x)
    max(min(x, max.step.size), min.step.size)
  times <- numeric(0)
  dt <- time <- 0
  while (time <= max.time) {
    times <- c(times, time)
    time <- time + trim(time * multiplier)
  }
  times
}

p <- new(Parameters)
p$add_strategy(new(Strategy))

p$seed_rain <- 1.1
p$set_parameters(list(patch_area=1.0)) # See issue #13

## Relatively quick control settings:
ctrl.new <- list()
ctrl.new$environment_light_rescale_usually <- TRUE
ctrl.new$environment_light_tol <- 1e-4
ctrl.new$plant_assimilation_rule <- 21
ctrl.new$plant_assimilation_over_distribution <- FALSE
ctrl.new$plant_assimilation_tol <- 1e-4
ctrl.new$ode_tol_rel <- 1e-4
ctrl.new$ode_tol_abs <- 1e-4
ctrl.new$ode_step_size_max <- 5
ctrl.new$cohort_gradient_direction <- -1
ctrl.new$cohort_gradient_richardson <- FALSE
p$set_control_parameters(ctrl.new)

ebt <- new(EBT, p)

tt <- cohort.introduction.times(104)
sched <- new(CohortSchedule, 1)
sched$set_times(tt, 1)
sched$max_time <- max(tt)
ebt$cohort_schedule <- sched
res <- run.ebt(ebt)

saveRDS(res, "output/tree.output.rds")
