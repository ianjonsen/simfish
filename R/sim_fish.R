#' @title simulate a fish track
#'
#' @description simulates fish tracks on a user-supplied raster depicting a water body
#'
#' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
#'
#' @param id - identifier for simulation run (individual animal)
#' @param data - a list of required data from `sim_setup()`
#' @param mpar - simulation control parameters supplied as a list using `sim_par()`
#' @param pb - use progress bar (logical)
#' @importFrom raster extract xyFromCell
#' @importFrom CircStats rwrpcauchy
#' @importFrom dplyr %>% mutate lag
#' @importFrom tibble as_tibble
#' @importFrom stats runif rbinom
#' @importFrom lubridate week yday
#' @importFrom stringr str_split
#' @export

sim_fish <-
  function(id=1,
           data = NULL,
           mpar = sim_par(),
           pb = TRUE
  ) {


    if (is.null(data))
      stop("Can't find output from sim_setup()\n")
    if (class(data$land)[1] != "RasterLayer") stop("d2land must be a RasterLayer")

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

      if(!is.na(extract(data$land, rbind(xy[i, 1:2]))) & any(!is.na(xy[i, 1:2]))) {
        mpar$land <- TRUE
        cat("\n stopping simulation: stuck on land")
        break
      }

      if(any(is.na(xy[i, 1:2]))) {
        mpar$boundary <- TRUE
        cat("\n stopping simulation: hit a boundary")
        break
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
      select(id, date, everything())

    param <- mpar
    out <- list(sim = sim, params = param)
    class(out) <- "simfish"

    return(out)
  }
