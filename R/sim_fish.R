#' @title simulate a fish track
#'
#' @description simulates fish tracks
#'
#' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
#'
#' @param id - identifier for simulation run (individual animal)
#' @param data - a list of required data from \code{presim}
#' @param mpar - simulation control parameters supplied as a list, see details
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
    ## ds - active swimming displacements
    ## dl - displacements to deflect away from land
    xy <- matrix(NA, N, 2)
    xy[1,] <- cbind(mpar$start)
    ds <- matrix(NA, N, 4) #x, y, s

    s <- mpar$fl * mpar$bl # units = m/s

    ## iterate movement
    for (i in 2:N) {
      if(i==2 && pb)  tpb <- txtProgressBar(min = 2, max = N, style = 3)

      ## calculate potential fn values
      pv <- c(extract(data$grad[[1]], rbind(xy[i-1,]))[1],
              extract(data$grad[[2]], rbind(xy[i-1,]))[1])

      ## Movement kernel
      xy[i, 1:2] <- move_kernel(data,
                         xy = xy[i-1,],
                         mpar = mpar,
                         s,
                         pv)


      if(!is.na(extract(data$land, rbind(xy[i, ])))  & any(!is.na(xy[i,]))) {
        mpar$land <- TRUE
        cat("\n stopping simulation: stuck on land")
        break
      }

      if(any(is.na(xy[i, ]))) {
        mpar$boundary <- TRUE
        cat("\n stopping simulation: hit a boundary")
        break
      }

      if(pb){
        setTxtProgressBar(tpb, i)
        if(i==N) close(tpb)
      }

    }

    N <- ifelse(!is.na(which(is.na(xy[,1]))[1] - 1), which(is.na(xy[,1]))[1] - 1, N)
    X <-
      data.frame(
        x = xy[, 1],
        y = xy[, 2],
        dx = ds[, 1] - lag(xy[, 1]),
        dy = ds[, 2] - lag(xy[, 2]),
        phi = phi
      )[1:N,]

    sim <- X %>% as_tibble()

    ## remove records after sim is stopped for being stuck on land, etc...
    if(mpar$land | mpar$boundary) {
      sim <- sim %>%
        filter(!is.na(x) & !is.na)
    }

    nsim <- nrow(sim)


 ## add time - base is 1 min intervals
    sim <- sim %>%
      mutate(id = id) %>%
      mutate(date = seq(mpar$pars$start.dt, by = 60 * mpar$pars$time.interval, length.out = nsim)) %>%
      select(id, date, everything())

    param <- mpar
    out <- list(sim = sim, params = param)
    class(out) <- "simfish"

    return(out)
  }
