##' @title Define the simulation parameters used by \code{sim_fish}
##'
##' @description Define simulation parameters
##'
##' @param N number of time steps to simulate
##' @param time.step duration (min) of each simulation time step
##' @param start.dt start datetime (POSIXct) of simulation
##' @param start start location coordinates (grid x, y in km)
##' @param coa Centre-of-Attraction coordinates (grid x, y in km)
##' @param nu strength of attraction to CoA (range: 0 - infinity)
##' @param rho angular dispersion parameter (Wrapped Cauchy) for turn angles
##' @param bl move speed in body-lengths per second
##' @param fl fish fork-length (m)
##' @param pdrf logistic intercept & slope parameters defining detection
##' probability as a function of distance between fish and receiver. Default values
##' roughly correspond to detection range of V9 tags in the ocean.
##' @param beta potential function parameters for x and y directions to
##' keep fish off land. Larger -ve values result in stronger land avoidance but can
##' introduce unrealistic jumps (possibly across narrow land features) in the track.
##' @param buffer distance radius in km to search for water when simulation stuck
##' on land. Smaller values reduce chance of unrealistic jumps away from land but
##' can lead to simulations getting stuck on land. Larger values increase chance
##' of unrealistic jumps (possibly across land) but reduce chance of simulations
##' getting stuck on land.
##' @return Returns a list of simulation parameters to be used by \code{sim_fish}.
##' See example, below
##'
##' @examples
##' ## A minimal example - simulation with no environment
##' my.par <- sim_par(N = 1440, time.step = 5, start = c(0, 0), coa = c(0,30))
##'
##' out <- sim_fish(id = 1, mpar = my.par)
##'
##' plot(out)
##'
##' @export

sim_par <-
  function(...) {

    dots <- list(...)

    mpar <- list(
      N = 250,
      time.step = 30,
      start.dt = ISOdatetime(2023,03,10,12,00,00, tz="UTC"),
      start = c(0, 0),
      coa = c(15, 30),
      nu = 1,
      rho = 0.4,
      bl = 2,
      fl = 0.15,
      pdrf = c(5, -0.02), # = p(0.5) @ 250 m  + < p(0.01) @ 500 m   [c(4.865, -0.0139)  (~ consistent w HFX line V9 @ high power)]
      beta = c(-10, -10),
      buffer = 0.2,
      land = FALSE,
      boundary = FALSE
    )

    ## overide default control pars
    mpar[names(dots)] <- dots

    mpar
  }
