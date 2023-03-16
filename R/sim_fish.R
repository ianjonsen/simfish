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
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param id - identifier for simulation run (individual fish)
##' @param data - a list of required data. If missing then simulation runs on a
##' Cartesian grid (a featureless environment).
##' @param mpar - simulation control parameters supplied as a list using `sim_par()`
##' See `?sim_par` for details on the simulation parameters.
##' @param pb - use progress bar (logical)
##' @importFrom raster extract xyFromCell nlayers
##' @importFrom CircStats rwrpcauchy
##' @importFrom dplyr %>% mutate lag select
##' @importFrom tibble as_tibble
##' @importFrom stats runif rbinom
##' @importFrom lubridate week yday
##' @importFrom stringr str_split
##'
##' @examples
##' ## A minimal example - simulation with no environment
##' my.par <- sim_par(N = 1440, time.step = 5, start = c(0, 0), coa = c(0,30))
##'
##' out <- sim_fish(id = 1, mpar = my.par, pb = FALSE)
##'
##' plot(out)
##'
##' ## Simulate in a semi-realistic environment
##' land <- generate_env(ext = c(-70,43,-52,53), res = c(0.05,0.05))
##' grad <- generate_grad(land)
##' x <- list(land = land, grad = grad)
##'
##' my.par <- sim_par(N=400, time.step=60*6, start = c(-7260, 5930),
##' coa = c(-6300,6680), nu = 0.6, rho = 0.7)
##' out <- sim_fish(id = 1, data = x, mpar = my.par, pb = FALSE)
##'
##' map(out, env = x)
##' @export

sim_fish <-
  function(id=1,
           data = NULL,
           mpar = sim_par(),
           pb = TRUE
  ) {


    if (!is.null(data)) {
      if (class(data$land)[1] != "RasterLayer") stop("land must be a RasterLayer")
      if (class(data$grad)[1] != "RasterStack") stop("grad must be a RasterStack")
      if (class(data$grad)[1] == "RasterStack" & nlayers(data$grad) != 2)
        stop("grad must be a RasterStack with 2 layers")

      if (length(grep("+units=km", data$land)) == 0)
        stop("raster projection must have units in km")
    }

    N <- mpar$N

    ## define location matrix & initialise start position
    ## xy[, 1:2] - location coordinates
    ## xy[, 3] - mean turn angle
    xy <- matrix(NA, N, 3)
    xy[1, 1:2] <- cbind(mpar$start)
    xy[1, 3] <- 0

    s <- mpar$fl/1000 * mpar$bl * 60 * mpar$time.step # convert from m/s to km/min * mpar$time.step

    ## iterate movement
    for (i in 2:N) {
      if(i==2 && pb)  tpb <- txtProgressBar(min = 2, max = N, style = 3)

      ## Movement kernel
      xy[i, ] <- move_kernel(data,
                         xy = xy[i-1, ],
                         mpar = mpar,
                         s)

      if (!is.null(data$land)) {
        if (!is.na(extract(data$land, rbind(xy[i, 1:2]))) &
            any(!is.na(xy[i, 1:2]))) {
          mpar$land <- TRUE
          cat("\n stopping simulation: stuck on land")
          break
        }

        if (any(is.na(xy[i, 1:2]))) {
          mpar$boundary <- TRUE
          cat("\n stopping simulation: hit a boundary")
          break
        }
      }

      if(pb){
        setTxtProgressBar(tpb, i)
        if(i==N) close(tpb)
      }
    #print(i)
    }

    N <- ifelse(!is.na(which(is.na(xy[,1]))[1] - 1), which(is.na(xy[,1]))[1] - 1, N)

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
    out <- list(sim = sim, params = param)
    class(out) <- "simfish"

    return(out)
  }
