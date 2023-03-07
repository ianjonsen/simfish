##' \code{sim_par} defines the simulation parameters & control scenarios used by \code{simulate}.
##'
##' The movement process used predominantly in the simulation is
##' selected by the \code{move} argument.  Additional
##' parameters include: \code{temp} is movement temperature-dependent; \code{advect} do ocean
##' currents influence movement; \code{growth} do smolts grow in temp-dependent fashion;
##' \code{start.date};
##' \code{start} (location); \code{coa} centre of attraction (can be NULL);
##' \code{mdir} directional bias (a 2-element vector);
##' \code{rho} concentration of step directions (for wrapped-Cauchy, a 2-element vector); ...
##'
##' @title Control Values for \code{simulate}.
##' @param ... simulation control parameters
##' @return ...
##' @export

sim_par <-
  function(...) {

    dots <- list(...)

    mpar <- list(
      N = 1440,
      start.dt = ISOdatetime(2023,05,25,16,00,00, tz = "UTC"),
      time.step = 1, # in minutes
      start = c(6912, 1465), # start coordinates, units = km
      coa = c(7120, 2350), # centre of attraction coordinates, units = km
      nu = 5, # strength of attraction to coa (range: 0 - infinity)
      rho = 0.4, # directional persistence for brw
      bl = 2, # weibull scale parameter (in body lengths)
      fl = 0.15, # forklength (in m)
      pdrf = c(5, -0.02), # = p(0.5) @ 250 m  + < p(0.01) @ 500 m   [c(4.865, -0.0139)  (~ consistent w HFX line V9 @ high power)]
      beta = c(-200, -200), # potential fn params to keep fish off land
      buffer = 0.2, # distance radius in km to search for water when simulation stuck on land. Smaller values reduce chance of unrealistic
        # jumps away from land but can lead to simulations getting stuck on land. Larger values increase chance of unrealistic jumps
        # (possibly across land) but reduce chance of simulations getting stuck on land.
      land = FALSE,
      boundary = FALSE
    )

    ## overide default control pars
    mpar[names(dots)] <- dots

    mpar
  }
