##' @title simf
##'
##' @description simulates fish tracks in featureless or semi-realistic
##' environments. Workhorse fn called by front-end `sim_fish()`.
##'
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param data - a list of required data. If missing then simulation runs on a
##' Cartesian grid (a featureless environment).
##' @param mpar - simulation control parameters supplied as a list using `sim_par()`
##' See `?sim_par` for details on the simulation parameters.
##' @importFrom raster extract xyFromCell nlayers
##' @importFrom CircStats rwrpcauchy
##' @importFrom dplyr "%>%" mutate lag select filter everything
##' @importFrom tibble as_tibble
##' @importFrom stats runif rbinom
##' @importFrom lubridate week yday
##' @importFrom stringr str_split
##'
##' @keywords internal

simf <- function(data,
                 mpar,
                 s,
                 xy,
                 coas = FALSE) {

  ## iterate movement
  st <- which(is.na(xy[,1]))[1]
  if(is.na(st)) {
    st <- nrow(xy)
    tmp <- matrix(NA, nrow(xy) + mpar$N, ncol(xy))
    tmp[1:nrow(xy), ] <- xy
    xy <- tmp
  }

  for (i in st:mpar$N) {

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

    ## if multiple coas present in mpar then stop simulation once fish is within
    ##  coa.tol km of current CoA
    if (coas) {
      if(sqrt((xy[i, 1] - mpar$coa[1])^2 + (xy[i, 2] - mpar$coa[2])^2) <= mpar$coa.tol) {
        break
      }
    }

  }

  ## remove unused rows from xy
  idx <- which(is.na(xy[, 1]))[1]
  if(!is.na(idx)) xy <- xy[1:(idx - 1), ]

  return(list(xy = xy, mpar = mpar))
}
