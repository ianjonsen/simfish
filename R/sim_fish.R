##' @title simulate a fish track
##'
##' @description simulates fish tracks in featureless or semi-realistic
##' environments. Semi-realistic environments are user-defined rasters of water
##' bounded by land, which constrain the simulated tracks. User-supplied acoustic
##' receiver locations can be used to simulate acoustic detections based on a
##' user-defined detection probability with distance. Utility functions are
##' provided to generate water-land rasters (`generate_land()`), required
##' gradient rasters (`generate_grad()`), and to convert acoustic tag
##' detection range expectations into logistic regression parameters (`calc_pdrf()`).
##'
##' @param id - identifier for simulation run (individual fish)
##' @param data - a list of required data. If missing then simulation runs on a
##' Cartesian grid (a featureless environment).
##' @param mpar - simulation control parameters supplied as a list using `sim_par()`
##' See `?sim_par` for details on the simulation parameters.
##' @param ... additional, optional arguments to `find_route()`
##' @importFrom raster extract xyFromCell nlayers crs
##' @importFrom CircStats rwrpcauchy
##' @importFrom dplyr "%>%" mutate lag select filter everything
##' @importFrom tibble as_tibble
##' @importFrom stats runif rbinom
##' @importFrom lubridate week yday
##' @importFrom stringr str_split
##'
##' @examples
##' ## A minimal example - simulation with no environment
##' my.par <- sim_par(N = 1440, time.step = 5, start = c(0, 0), coa = c(0,30))
##'
##' z <- sim_fish(id = 1, mpar = my.par)
##'
##' plot(z)
##'
##' ## Simulate in a semi-realistic environment
##' x <- generate_env(ext = c(-70,43,-52,53), res = c(0.05,0.05), grad = TRUE)
##'
##' my.par <- sim_par(N=400, time.step=60*6, start = c(-7260, 5930),
##' coa = c(-6300, 6680), nu = 0.6, rho = 0.7)
##' z <- sim_fish(id = 1, data = x, mpar = my.par)
##'
##' map(z, env = x)
##' @export

sim_fish <-
  function(id=1,
           data = NULL,
           mpar = sim_par(),
           ...
  ) {

    if (!is.null(data)) {
      if (class(data$land)[1] != "RasterLayer") stop("land must be a RasterLayer")
      if (class(data$grad)[1] != "RasterStack") stop("grad must be a RasterStack")
      if (class(data$grad)[1] == "RasterStack" & nlayers(data$grad) != 2)
        stop("grad must be a RasterStack with 2 layers")

      if (length(grep("+units=km", data$land)) == 0)
        stop("raster projection must have units in km")
      if (length(grep("prj", names(data))) == 0) {
        data$prj <- crs(data$land)
      }
    }

    if(is.null(dim(mpar$coa)[1]) & !is.null(data)) {
      ## check for barriers between start and coa & reroute via intermediate CoA's
      preroute <- FALSE
      fr.list <- find_route(data, mpar, ...)

      if(!is.null(dim(fr.list[[1]])) & is.null(mpar$N)) {
        mpar$N <- 10000
        mpar$coa <- fr.list[[1]]
      }
    } else if(!is.null(dim(mpar$coa)[1]) & !is.null(data)) {
      preroute <- TRUE
      mpar$N <- 10000
    } else if (is.null(data)) {
      preroute <- TRUE
    }

    cat("simulating track...\n")
    ## define location matrix & initialise start position
    ## xy[, 1:2] - location coordinates
    ## xy[, 3] - mean turn angle
    xy <- matrix(NA, mpar$N, 3)
    xy[1, 1:2] <- cbind(mpar$start)
    xy[1, 3] <- 0

    ## define step length based on forklength (fl), swim speed in body-lengths/s,
    ##  and time.step. Convert from m/s to km/min * mpar$time.step
    s <- mpar$fl/1000 * mpar$bl * 60 * mpar$time.step

    ## call simulation fn
    if(!inherits(mpar$coa, "matrix")) {
      ## single CoA
      tmp <- simf(data, mpar, s, xy)
      xy <- tmp$xy
      mpar <- tmp$mpar
    } else {
      ## multiple CoA to get around land masses
      coas <- mpar$coa
      tmp <- list()
      for (i in 1:nrow(mpar$coa)) {
        if(i == 1) {
          mpar$coa <- coas[1,]
          tmp[[1]] <- simf(data, mpar, s, xy, coas = TRUE)

        } else {
          mpar$coa <- coas[i,]
          #if(i == nrow(coas)) mpar$coa.tol <- 0
          tmp[[i]] <- simf(data, mpar, s, tmp[[i-1]]$xy, coas = TRUE)
        }
      }

      xy <- tmp[[length(tmp)]]$xy
      mpar <- tmp[[length(tmp)]]$mpar
      mpar$coa <- coas
      mpar$N <- nrow(xy)
    }

    N <- ifelse(!is.na(which(is.na(xy[,1]))[1] - 1), which(is.na(xy[,1]))[1] - 1, mpar$N)

    ## process sim results
    X <-
      data.frame(
        x = xy[, 1],
        y = xy[, 2],
        dx = xy[, 1] - lag(xy[, 1]),
        dy = xy[, 2] - lag(xy[, 2]),
        mu = xy[, 3]
      )[1:N,]

    sim <- X %>% as_tibble()

    ## remove records after sim is stopped for being stuck on land, etc...
    if(mpar$land | mpar$boundary) {
      sim <- sim %>%
        filter(!is.na(x) & !is.na(y))
    }

    nsim <- nrow(sim)

 ## add time - base is 1 min intervals
    sim <- sim %>%
      mutate(id = id) %>%
      mutate(date = seq(mpar$start.dt, by = 60 * mpar$time.step, length.out = nsim)) %>%
      dplyr::select(id, date, everything())

    param <- mpar
    if(preroute) {
      out <- list(sim = sim, params = param)
    } else {
      out <- list(sim = sim, params = param, fr.coas = fr.list[[1]], fr.map = fr.list[[2]])
    }

    class(out) <- "simfish"

    return(out)
  }
