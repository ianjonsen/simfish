##' @title Define the simulation parameters used by \code{sim_fish}
##'
##' @description Define simulation parameters
##'
##' @param ... simulation parameters:
##'
##' **`N`** - number of time steps to simulate, if NULL then simulation runs until
##' fish reaches the tolerance distance to the final Centre-of-Attraction (`coa.tol`);
##'
##' **`time.step`** - duration (min) of each simulation time step;
##'
##' **`start.dt`** - start datetime (POSIXct) of simulation;
##'
##' **`start`** - start location coordinates (grid x, y in km);
##'
##' **`coa`** - Centre-of-Attraction coordinates (grid x, y in km). If > 1 coa
##' supplied (as a matrix) then the simulation uses the coa's sequentially - for example,
##' to guide the track around land features lying between the specified start and
##' end points;
##'
##' **`coa.tol`** - tolerance distance (km) around CoA(s);
##'
##' **`nu`** - strength of attraction to CoA (range: 0 - infinity);
##'
##' **`rho`** - angular dispersion parameter (Wrapped Cauchy) for turn angles;
##'
##' **`bearing`** - a scalar or vector of bearings (in radians) that give the bias
##' in `bcrw` directions. Ignored if model = `bcrw.coa`
##'
##' **`bl`** - move speed in body-lengths per second. Can be a vector, if model = "bcrw".
##' If bl is a vector then it must have the same length as `bearing`;
##'
##' **`fl`** - fish fork-length (m);
##'
##' **`pdrf`** - logistic intercept & slope parameters defining detection
##' probability as a function of distance between fish and receiver. Default values
##' roughly correspond to detection range of V9 tags in the ocean;
##'
##' **`beta`** - potential function parameters for x and y directions to
##' keep fish off land. Larger -ve values result in stronger land avoidance but can
##' introduce unrealistic jumps (possibly across narrow land features) in the track;
##'
##' **`buffer`** - distance radius in km to search for water when simulation stuck
##' on land. Smaller values reduce chance of unrealistic jumps away from land but
##' can lead to simulations getting stuck on land. Larger values increase chance
##' of unrealistic jumps (possibly across land) but reduce chance of simulations
##' getting stuck on land.
##'
##' @return Returns a list of simulation parameters to be used by \code{sim_fish}.
##' See example, below
##'
##' @examples
##' ## A minimal example - simulation with no environment
##' my.par <- sim_par(N = 1440, time.step = 5, start = c(0, 0), coa = c(0,30))
##'
##' z <- sim_fish(id = 1, mpar = my.par)
##'
##' plot(z)
##'
##' @md
##' @export

sim_par <-
  function(...) {

    dots <- list(...)

    mpar <- list(
      N = NULL,
      time.step = 60,
      start.dt = as.POSIXct(unclass(Sys.time()), origin = "1970-01-01", tz = "UTC"),
      start = c(0, 0),
      model = c("bcrw.coa", "bcrw"),
      coa = cbind(c(15, 30)),
      coa.tol = 1,
      nu = 1,
      rho = 0.5,
      bearing = NA,
      bl = 1.5,
      fl = 0.15,
      pdrf = c(5, -0.02), # = p(0.5) @ 250 m  + < p(0.01) @ 500 m   [c(4.865, -0.0139)  (~ consistent w HFX line V9 @ high power)]
      beta = c(-10, -10),
      buffer = 1,
      land = FALSE,
      boundary = FALSE
    )

    mpar$coa <- cbind(mpar$coa)

    ## overide default control pars
    mpar[names(dots)] <- dots

    mpar
  }
