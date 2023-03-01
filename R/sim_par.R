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
      start = c(6912, 1465),
      coa = c(7120, 2350),
      rho = 0.4, # directional persistence for brw
      bl = 2, # weibull scale parameter
      fl = 0.15,
      pdrf = c(5, -0.02), # = p(0.5) @ 250 m  + < 0.01 @ 500 m   [c(4.865, -0.0139)  (~ consistent w HFX line V9 @ high power)]
      beta = c(-200, -200) # potential fn params to keep fish off land
    )

    ## overide default control pars
    mpar[names(dots)] <- dots

    list(mpar = mpar)
  }