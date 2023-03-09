##' @title Define the simulation parameters used by \code{sim_fish}
##'
##' @description Define simulation parameters
##'
##' @param ... simulation control parameters
##' @return Returns a list of simulation parameters to be used by \code{sim_fish}.
##' See example, below
##'
##' @examples
##' ## A minimal example - simulation with no environment
##' my.par <- sim_par(N = 1440, time.step = 5, start = c(0, 0), coa = c(0,30))
##'
##' out <- sim_fish(id = 1, mpar = my.par)
##' plot(out)
##'
##'
##' @export

sim_par <-
  function(...) {

    dots <- list(...)

    mpar <- list(
      N = 250,
      start.dt = ISOdatetime(2023,03,15,12,00,00, tz = "UTC"),
      time.step = 30, # in minutes
      start = c(0, 0), # start coordinates, units = km
      coa = c(15, 30), # centre of attraction coordinates, units = km
      nu = 1, # strength of attraction to coa (range: 0 - infinity)
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
