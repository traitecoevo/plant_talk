make_fitness_data <- function() {
  p0 <- scm_base_parameters("FF16")
  p0$control$equilibrium_nsteps <- 30
  p0$control$equilibrium_solver_name <- "hybrid"
  p0$disturbance_mean_interval <- 30.0

  bounds <- bounds_infinite("lma")

  bounds <- viable_fitness(bounds, p0)
  bounds <- check_bounds(bounds)

  ## Communities:
  communities <-
    list(first      = 0.2,           # first individual
         attractor1 = 0.0825,           # first branching point
         second1    = c(0.0825, 0.355),  # ~= the max fitness
         second2    = c(0.0825, 0.2878),  # ~= the max fitness
         attractor2 = c(0.0825, 0.2625)) # stable

  lma_mutant <- seq_log_range(bounds, 101)

  lma <- sort(c(unique(unlist(communities)), lma_mutant))

  f <- function(x, p) {
    equilibrium_seed_rain(expand_parameters(trait_matrix(x, "lma"), p, FALSE))
  }
  pp <- lapply(communities, f, p0)
  ww <- lapply(pp, fitness_landscape, trait_matrix=trait_matrix(lma, "lma"))

  list(communities=communities,
       lma=lma,
       p=pp,
       w=ww)
}

figure_fitness_landscape <- function(data) {

  communities <- data$communities
  lma <- data$lma
  w <- data$w

  ## Start the plots:
  xlim <- c(0.05, 2.00)
  ylim <- c(-3, 3)

  op <- par(mfrow=c(1, 1), mar=c(1.1, 4.1, .5, .5), oma=c(3.1, 0, 0, 0))
  on.exit(par(op))

  # plot(lma, w$first, type="l", lty=2,
  #      log="x", xlim=xlim, ylim=ylim, ylab="Fitness", las=1, xaxt="n")
  # # label_panel(1)
  # axis(1, labels=FALSE)
  # lines(lma, w$attractor1)
  # abline(h=0, col="grey")
  # points(communities$first, 0, pch=1)
  # points(communities$attractor1, 0, pch=19)


  plot(NA, log="x", xlim=xlim, ylim=ylim, ylab="Fitness", las=1)

  mtext(expression("Leaf mass per unit leaf area"~("LMA;"~kg~m^-2)), 1, 3, cex=1)
  # lines(lma, w$second1, lty=3)
  # lines(lma, w$second2, lty=3)
  lines(lma, w$attractor2, lwd=2, col="darkgreen")
  abline(h=0)
  # points(communities$first, 0, pch=1)
   points(communities$attractor2[[1]], 0, pch=19)
  points(communities$attractor2[[2]], 0, pch=19)
  # points(communities$second1[[2]], 0)
  # points(communities$second2[[2]], 0, col="grey")
}
